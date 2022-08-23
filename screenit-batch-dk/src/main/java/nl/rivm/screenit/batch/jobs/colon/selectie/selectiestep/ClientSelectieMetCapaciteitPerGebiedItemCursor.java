package nl.rivm.screenit.batch.jobs.colon.selectie.selectiestep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-dk
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.batch.jobs.colon.selectie.selectiestep.ColonClientSelectieContext.CohortUitnodiging;
import nl.rivm.screenit.batch.jobs.colon.selectie.selectiestep.ColonClientSelectieContext.ProjectGroupUitnodiging;
import nl.rivm.screenit.batch.jobs.colon.selectie.selectiestep.ColonClientSelectieContext.UitnodigingsTaak;
import nl.rivm.screenit.batch.service.impl.ColonUitnodigingsgebiedSelectieContext;
import nl.rivm.screenit.dao.colon.impl.ColonRestrictions;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Gemeente;
import nl.rivm.screenit.model.UitnodigingsGebied;
import nl.rivm.screenit.model.colon.ClientCategorieEntry;
import nl.rivm.screenit.model.colon.UitnodigingCohortGeboortejaren;
import nl.rivm.screenit.model.colon.enums.ColonUitnodigingCategorie;

import org.apache.commons.lang.NotImplementedException;
import org.hibernate.Criteria;
import org.hibernate.ScrollMode;
import org.hibernate.ScrollableResults;

@Slf4j
public class ClientSelectieMetCapaciteitPerGebiedItemCursor implements Iterator<ClientCategorieEntry>
{

	private ColonUitnodigingCategorie uitnodigingscategorie = ColonUitnodigingCategorie.U2;

	private ColonUitnodigingsgebiedSelectieContext uitnodigingsgebiedContext;

	private ScrollableResults cursor;

	private boolean cursorClosed = true;

	private int cursorCount;

	private int huidigeTaak = 0;

	private final List<ColonClientSelectieContext.UitnodigingsTaak> taken = new ArrayList<>();

	private Set<Integer> verwerkteGeboortjaren = new HashSet<>();

	private List<Long> exclusieGroepIds = new ArrayList<>();

	private final ColonClientSelectieContext selectieContext;

	private UitnodigingsGebied cachedUitnodigingsgebied = null;

	private final LocalDate vandaag;

	public ClientSelectieMetCapaciteitPerGebiedItemCursor(ColonClientSelectieContext selectieContext, ColonUitnodigingsgebiedSelectieContext gebiedContext, LocalDate vandaag)
	{
		this.selectieContext = selectieContext;
		this.uitnodigingsgebiedContext = gebiedContext;
		this.vandaag = vandaag;
		synchronized (selectieContext.exclusieGroepIds)
		{
			this.exclusieGroepIds.addAll(selectieContext.exclusieGroepIds);
		}
		synchronized (selectieContext.taken)
		{
			this.taken.addAll(selectieContext.taken);
		}
		maakOfVerversCursor();
	}

	@Override
	public boolean hasNext()
	{
		if (cursorClosed)
		{
			return false;
		}
		boolean hasNext = true;
		while (!huidigeGebiedHeeftCapaciteitOver() || !cursor.next())
		{
			if (LOG.isDebugEnabled())
			{
				LOG.debug((cursorClosed ? "" : "cursorNext?=" + cursor.next() + ", ") + this);
			}
			if (cursorClosed)
			{
				hasNext = false;
				break;
			}
			boolean gebiedKlaar = false;
			if (huidigeGebiedHeeftCapaciteitOver())
			{
				if (uitnodigingscategorie == ColonUitnodigingCategorie.U2)
				{
					if (LOG.isDebugEnabled())
					{
						UitnodigingsGebied uitnodigingsgebied = getUitnodigingsgebied();
						LOG.debug("Categorie: " + uitnodigingscategorie.name() + ", Gebied: " + uitnodigingsgebied.getNaam() + ". # clienten geselecteerd: " + cursorCount);
					}
					cursorCount = 0;
					uitnodigingscategorie = ColonUitnodigingCategorie.U1;
					prepareTaak();
				}
				else if (uitnodigingscategorie == ColonUitnodigingCategorie.U1)
				{
					if (heeftMeerTaken())
					{
						huidigeTaak++;
						prepareTaak();
					}
					else
					{
						gebiedKlaar = true;
					}
				}
			}
			else
			{
				gebiedKlaar = true;
			}
			if (gebiedKlaar)
			{
				if (LOG.isDebugEnabled())
				{
					UitnodigingsGebied uitnodigingsgebied = getUitnodigingsgebied();
					LOG.debug("Categorie: " + uitnodigingscategorie.name() + ", Gebied: " + uitnodigingsgebied.getNaam() + ". # clienten geselecteerd: " + cursorCount);
				}
				hasNext = false;
				break;
			}
			close();
			maakOfVerversCursor();
		}
		if (LOG.isDebugEnabled())
		{
			LOG.debug("hasNext=" + hasNext + ", " + this);
		}
		return hasNext;
	}

	UitnodigingsGebied getUitnodigingsgebied()
	{
		if (cachedUitnodigingsgebied == null)
		{
			cachedUitnodigingsgebied = selectieContext.getHibernateSession().load(UitnodigingsGebied.class,
				uitnodigingsgebiedContext.getUitnodigingsgebiedId());
		}
		return cachedUitnodigingsgebied;
	}

	private void prepareTaak()
	{
		if (LOG.isDebugEnabled())
		{
			UitnodigingsGebied uitnodigingsgebied = getUitnodigingsgebied();
			LOG.debug("Categorie: " + uitnodigingscategorie.name() + ", Gebied: " + uitnodigingsgebied.getNaam() + ". Prepare taak " + taken.get(huidigeTaak));
		}
		close();

		UitnodigingsTaak uitnodigingsTaak = taken.get(huidigeTaak);
		uitnodigingsgebiedContext.setTotaalAntalClienten(uitnodigingsTaak.getMaxAantalClienten(uitnodigingsgebiedContext));

	}

	private boolean heeftMeerTaken()
	{
		return huidigeTaak + 1 < taken.size();
	}

	private boolean huidigeGebiedHeeftCapaciteitOver()
	{
		return uitnodigingsgebiedContext != null && uitnodigingsgebiedContext.isGenoegUitnodigingscapaciteitOver();
	}

	@Override
	public ClientCategorieEntry next()
	{

		if (!cursorClosed)
		{
			Object[] data = cursor.get();
			Client client = (Client) data[0];

			Gemeente gemeente = client.getPersoon().getGbaAdres().getGbaGemeente();

			List<Client> clientenOpAdres = selectieContext.clientDao.getClientenOpAdres(client.getPersoon().getGbaAdres(), selectieContext.minimaleLeeftijd,
				selectieContext.maximaleLeeftijd, selectieContext.uitnodigingsInterval);
			Client andereClient = ColonRestrictions.getAndereClient(clientenOpAdres, client);

			boolean geenAndereClientMetZelfdeAdresEnActieveIfobt = clientenOpAdres.size() != 2
				|| !ColonRestrictions.isIfobtActief(andereClient, selectieContext.uitgenodigdeClientIds)
				|| ColonRestrictions.isWachttijdOpPakketVerstreken(andereClient, selectieContext.wachttijdVerzendenPakket, selectieContext.uitgenodigdeClientIds, vandaag);
			if (geenAndereClientMetZelfdeAdresEnActieveIfobt)
			{
				cursorCount++;

				uitnodigingsgebiedContext.consumeUitnodigingscapaciteit();

				selectieContext.uitgenodigdeClientIds.add(client.getId());

				UitnodigingsTaak uitnodigingsTaak = taken.get(huidigeTaak);
				Long projectGroepId = null;
				if (uitnodigingsTaak instanceof ProjectGroupUitnodiging && ColonUitnodigingCategorie.U1.equals(uitnodigingscategorie))
				{
					projectGroepId = ((ProjectGroupUitnodiging) uitnodigingsTaak).projectGroupId;
				}
				LOG.info("clientId (" + client.getId() + ") geselecteerd.");
				return new ClientCategorieEntry(client.getId(), uitnodigingscategorie, gemeente.getScreeningOrganisatie().getId(), projectGroepId);
			}
			else if (andereClient != null)
			{
				uitnodigingsgebiedContext.tweeClientenOpEenAdres();
				LOG.info("clientId (" + client.getId() + ") overgeslagen omdat ook een ander clientId (" + andereClient.getId()
					+ ") op zelfde adres met lopende FIT woont");
			}
			else
			{
				LOG.info("clientId (" + client.getId() + ") helemaal overgeslagen.");
			}
		}
		return null;
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

		UitnodigingsGebied uitnodigingsgebied = getUitnodigingsgebied();

		Long projectGroupId = null;
		List<Integer> geboorteJaren = null;
		if (taken.size() > huidigeTaak && uitnodigingscategorie == ColonUitnodigingCategorie.U1)
		{
			UitnodigingsTaak uitnodigingsTaak = taken.get(huidigeTaak);

			if (uitnodigingsTaak instanceof ProjectGroupUitnodiging)
			{
				projectGroupId = ((ProjectGroupUitnodiging) uitnodigingsTaak).projectGroupId;
			}
			if (uitnodigingsTaak instanceof CohortUitnodiging)
			{
				Integer cohortJaar = ((CohortUitnodiging) uitnodigingsTaak).cohortJaar;
				geboorteJaren = selectieContext.uitnodigingsDao.getUitnodigingCohort(cohortJaar).getGeboortejaren().stream()
					.map(UitnodigingCohortGeboortejaren::getGeboortejaren)
					.collect(Collectors.toList());
				geboorteJaren.removeAll(verwerkteGeboortjaren);
				verwerkteGeboortjaren.addAll(geboorteJaren);
				if (geboorteJaren.isEmpty())
				{
					geboorteJaren.add(4000);
				}
			}
		}

		Criteria crit;

		switch (uitnodigingscategorie)
		{
		case U1:
			crit = ColonRestrictions.getQueryVooraankondigen(selectieContext.getHibernateSession(), uitnodigingsgebied, geboorteJaren, false,
				selectieContext.minimaleLeeftijd, selectieContext.maximaleLeeftijd, projectGroupId, exclusieGroepIds, vandaag);
			break;
		case U2:
			crit = ColonRestrictions.getQueryU2(selectieContext.getHibernateSession(), uitnodigingsgebied, selectieContext.minimaleLeeftijd,
				selectieContext.maximaleLeeftijd, vandaag);
			break;
		default:
			throw new NotImplementedException("Niet bekende categorie");
		}
		cursor = crit.setFetchSize(selectieContext.fetchSize).scroll(ScrollMode.FORWARD_ONLY);
		cursorClosed = false;

	}

	@Override
	public void remove()
	{
		throw new UnsupportedOperationException("remove is not supported");
	}

	@Override
	public String toString()
	{
		return "Cursor [" + (uitnodigingscategorie != null ? "UC=" + uitnodigingscategorie + ", " : "")
			+ (uitnodigingsgebiedContext != null ? uitnodigingsgebiedContext + ", " : "") + "Cover?=" + huidigeGebiedHeeftCapaciteitOver() + ", "
			+ "closed=" + cursorClosed + ", count=" + cursorCount + ", " + "cHT=" + huidigeTaak
			+ (huidigeTaak >= 0 && huidigeTaak < taken.size() ? " (" + taken.get(huidigeTaak) + ")" : "") + "]";
	}
}
