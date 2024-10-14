package nl.rivm.screenit.batch.jobs.colon.selectie.selectiestep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-dk
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
 * %%
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * =========================LICENSE_END==================================
 */

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.batch.jobs.colon.selectie.selectiestep.ColonClientSelectieContext.EersteRondeUitnodiging;
import nl.rivm.screenit.batch.jobs.colon.selectie.selectiestep.ColonClientSelectieContext.ProjectGroupUitnodiging;
import nl.rivm.screenit.batch.jobs.colon.selectie.selectiestep.ColonClientSelectieContext.UitnodigingsTaak;
import nl.rivm.screenit.batch.service.impl.ColonUitnodigingsgebiedSelectieContext;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.colon.ClientCategorieEntry;
import nl.rivm.screenit.model.colon.UitnodigingCohortGeboortejaren;
import nl.rivm.screenit.model.colon.UitnodigingsGebied;

import org.hibernate.ScrollableResults;

@Slf4j
public class ClientSelectieMetCapaciteitPerGebiedItemCursor implements Iterator<ClientCategorieEntry>
{

	private final ColonUitnodigingsgebiedSelectieContext uitnodigingsgebiedContext;

	private ScrollableResults cursor;

	private boolean cursorClosed = true;

	private int cursorCount;

	private int huidigeTaak = -1;

	private final List<ColonClientSelectieContext.UitnodigingsTaak> taken = new ArrayList<>();

	private final Set<Integer> verwerkteGeboortjaren = new HashSet<>();

	private final List<Long> exclusieGroepIds = new ArrayList<>();

	private final ColonClientSelectieContext selectieContext;

	private UitnodigingsGebied cachedUitnodigingsgebied = null;

	public ClientSelectieMetCapaciteitPerGebiedItemCursor(ColonClientSelectieContext selectieContext, ColonUitnodigingsgebiedSelectieContext gebiedContext)
	{
		this.selectieContext = selectieContext;
		this.uitnodigingsgebiedContext = gebiedContext;
		synchronized (selectieContext.exclusieGroepIds)
		{
			exclusieGroepIds.addAll(selectieContext.exclusieGroepIds);
		}
		synchronized (selectieContext.taken)
		{
			selectieContext.taken.forEach(taak -> taken.add(taak.cloneTaak()));
		}
		eersteTaakVoorbereiden();
	}

	@Override
	public boolean hasNext()
	{
		if (cursorClosed)
		{
			return false;
		}
		boolean hasNext = true;
		boolean heeftGebiedGeenCapaciteitOver = !heeftGebiedCapaciteitOver();
		while (heeftGebiedGeenCapaciteitOver || isMaxAantalClientenVoorHuidigeTaakBereikt() || !cursor.next())
		{
			logClientenGeselecteerd();
			if (cursorClosed)
			{
				hasNext = false;
				break;
			}
			boolean gebiedKlaar = heeftGebiedGeenCapaciteitOver || !volgendeTaak();
			if (gebiedKlaar)
			{
				hasNext = false;
				break;
			}
			close();
			maakOfVerversCursor();
		}
		logHasNext(hasNext);
		return hasNext;
	}

	@Override
	public ClientCategorieEntry next()
	{

		if (!cursorClosed)
		{
			var data = cursor.get();
			var client = (Client) data[0];

			var gemeente = client.getPersoon().getGbaAdres().getGbaGemeente();

			var andereClientMetActieveFit = selectieContext.fitService.getAndereClientOpZelfdeAdresEnActieveFit(client, selectieContext.uitgenodigdeClientIds);

			var uitnodigingsTaak = getHuidigeUitnodigingsTaak();
			if (andereClientMetActieveFit == null)
			{
				cursorCount++;

				uitnodigingsTaak.clientGeselecteerd();
				uitnodigingsgebiedContext.consumeUitnodigingscapaciteit();

				selectieContext.uitgenodigdeClientIds.add(client.getId());

				Long projectGroepId = null;
				if (uitnodigingsTaak instanceof ProjectGroupUitnodiging)
				{
					projectGroepId = ((ProjectGroupUitnodiging) uitnodigingsTaak).projectGroupId;
				}
				LOG.info("{}Client (id: '{}') geselecteerd.", getDebugLogPrefix(), client.getId());
				return new ClientCategorieEntry(client.getId(), uitnodigingsTaak.getCategorie(), gemeente.getScreeningOrganisatie().getId(), projectGroepId);
			}
			else
			{

				uitnodigingsTaak.clientGeselecteerd();
				LOG.info("{}Client (id: '{}') overgeslagen omdat ook een ander client (id: '{}') op zelfde adres met actieve FIT woont", getDebugLogPrefix(), client.getId(),
					andereClientMetActieveFit.getId());
			}
		}
		return null;
	}

	@Override
	public void remove()
	{
		throw new UnsupportedOperationException("remove is not supported");
	}

	private void eersteTaakVoorbereiden()
	{
		if (heeftGebiedCapaciteitOver())
		{
			volgendeTaak();
			if (huidigeTaak < taken.size())
			{
				maakOfVerversCursor();
			}
		}
		else
		{
			LOG.info("'{}'. Geen capaciteit in uitnodigingsgebied. Niets te doen.", uitnodigingsgebiedContext.getUitnodigingsgebiedNaam());
		}
	}

	private boolean volgendeTaak()
	{
		boolean heeftVolgendeTaak;
		do
		{
			if (heeftMeerTaken())
			{
				huidigeTaak++;
				uitnodigingsTaakVoorbereiden();
				heeftVolgendeTaak = true;
			}
			else
			{
				heeftVolgendeTaak = false;
				break;
			}
		}
		while (isMaxAantalClientenVoorHuidigeTaakBereikt());
		return heeftVolgendeTaak;
	}

	private boolean heeftMeerTaken()
	{
		return huidigeTaak + 1 < taken.size();
	}

	private UitnodigingsTaak getHuidigeUitnodigingsTaak()
	{
		return taken.get(huidigeTaak);
	}

	private void uitnodigingsTaakVoorbereiden()
	{
		var uitnodigingsTaak = getHuidigeUitnodigingsTaak();
		if (LOG.isDebugEnabled())
		{
			LOG.debug("{}Start taak.", getDebugLogPrefix());
		}
		close();

		uitnodigingsTaak.fetchMaxAantalClienten(uitnodigingsgebiedContext.getUitnodigingsgebiedId());

	}

	private boolean isMaxAantalClientenVoorHuidigeTaakBereikt()
	{
		return getHuidigeUitnodigingsTaak().isMaxAantalClientenBereikt();
	}

	private UitnodigingsGebied getUitnodigingsgebied()
	{
		if (cachedUitnodigingsgebied == null)
		{
			cachedUitnodigingsgebied = selectieContext.hibernateService.load(UitnodigingsGebied.class,
				uitnodigingsgebiedContext.getUitnodigingsgebiedId());
		}
		return cachedUitnodigingsgebied;
	}

	private boolean heeftGebiedCapaciteitOver()
	{
		return uitnodigingsgebiedContext != null && uitnodigingsgebiedContext.isGenoegUitnodigingscapaciteitOver();
	}

	public void close()
	{
		cursorClosed = true;
		if (cursor != null)
		{
			cursor.close();
		}
	}

	private void maakOfVerversCursor()
	{
		var uitnodigingsgebied = getUitnodigingsgebied();
		Long projectGroupId = null;
		List<Integer> geboorteJaren = null;
		var uitnodigingsTaak = getHuidigeUitnodigingsTaak();

		if (uitnodigingsTaak instanceof ProjectGroupUitnodiging)
		{
			projectGroupId = ((ProjectGroupUitnodiging) uitnodigingsTaak).projectGroupId;
		}
		var uitnodigingsDao = selectieContext.uitnodigingsDao;
		var uitnodigingService = selectieContext.uitnodigingService;
		if (uitnodigingsTaak instanceof EersteRondeUitnodiging)
		{
			var cohortJaar = ((EersteRondeUitnodiging) uitnodigingsTaak).cohortJaar;
			geboorteJaren = uitnodigingService.getUitnodigingCohort(cohortJaar).getGeboortejaren().stream()
				.map(UitnodigingCohortGeboortejaren::getGeboortejaren)
				.collect(Collectors.toList());
			geboorteJaren.removeAll(verwerkteGeboortjaren);
			verwerkteGeboortjaren.addAll(geboorteJaren);
			if (geboorteJaren.isEmpty())
			{
				geboorteJaren.add(4000); 
			}
		}

		cursor = uitnodigingsDao.getUitnodigingsCursor(uitnodigingsTaak.getCategorie(), uitnodigingsgebied, geboorteJaren, selectieContext.minimaleLeeftijd,
			selectieContext.maximaleLeeftijd, projectGroupId, exclusieGroepIds, selectieContext.fetchSize);

		cursorClosed = false;

	}

	private void logHasNext(boolean hasNext)
	{
		if (LOG.isDebugEnabled())
		{
			LOG.debug("hasNext={}, {}", hasNext, this);
		}
	}

	private void logClientenGeselecteerd()
	{
		if (LOG.isDebugEnabled())
		{
			LOG.debug("{}{} client(en) geselecteerd in taak. cursorNext={}, heeftGebiedCapaciteitOver={}, isMaxAantalClientenVoorHuidigeTaakBereikt={}",
				getDebugLogPrefix(), getHuidigeUitnodigingsTaak().getClientenGeselecteerd(), (cursorClosed ? "closed" : cursor.next()), heeftGebiedCapaciteitOver(),
				isMaxAantalClientenVoorHuidigeTaakBereikt());
		}
	}

	private String getDebugLogPrefix()
	{
		return String.format("Gebied: '%s', %s, ", uitnodigingsgebiedContext.getUitnodigingsgebiedNaam(), getHuidigeUitnodigingsTaak());
	}

	@Override
	public String toString()
	{
		var taak = getHuidigeUitnodigingsTaak();
		return "Cursor [" + taak + ", " + (uitnodigingsgebiedContext != null ? uitnodigingsgebiedContext + ", " : "") + "Cover?=" + heeftGebiedCapaciteitOver() + ", " + "closed="
			+ cursorClosed + ", cc=" + cursorCount + ", " + "tc=" + (huidigeTaak + 1) + "]";
	}
}
