package nl.rivm.screenit.batch.jobs.colon.selectie.selectiestep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-dk
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.persistence.criteria.JoinType;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.batch.jobs.colon.selectie.SelectieConstants;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ScreeningRonde_;
import nl.rivm.screenit.model.colon.ClientCategorieEntry;
import nl.rivm.screenit.model.colon.enums.ColonUitnodigingCategorie;
import nl.rivm.screenit.repository.algemeen.ClientRepository;
import nl.rivm.screenit.service.colon.ColonBaseFITService;
import nl.rivm.screenit.specification.ExtendedSpecification;
import nl.rivm.screenit.specification.colon.ColonUitnodigingBaseSpecification;

import org.apache.commons.lang.NotImplementedException;
import org.hibernate.ScrollableResults;
import org.hibernate.Session;
import org.springframework.batch.item.ExecutionContext;

import static nl.rivm.screenit.specification.colon.ColonUitnodigingBaseSpecification.laatsteScreeningRondeJoin;

@Slf4j
public class ClientSelectieItemCursor implements ClientSelectieItemIterator
{
	private static final String UITNODIGINGSCAT = "key.uitnodigingscat";

	private ColonUitnodigingCategorie colonUitnodigingCategorie = ColonUitnodigingCategorie.U3;

	private ScrollableResults cursor;

	private boolean cursorClosed = true;

	private int cursorCount;

	private final Session hibernateSession;

	private final int fetchSize;

	private ExecutionContext context;

	private final List<Long> uitgenodigdeClientIds;

	private final LocalDate vandaag;

	private final ColonBaseFITService fitService;

	private final ClientRepository clientRepository;

	public ClientSelectieItemCursor(Session hibernateSession, int fetchSize, ExecutionContext context, List<Long> uitgenodigdeClientIds, ColonBaseFITService fitService,
		LocalDate vandaag, ClientRepository clientRepository)
	{
		this.hibernateSession = hibernateSession;
		this.fetchSize = fetchSize;
		this.context = context;
		this.uitgenodigdeClientIds = uitgenodigdeClientIds;
		this.fitService = fitService;
		this.vandaag = vandaag;
		this.clientRepository = clientRepository;

		initialiseCursor(context);
	}

	@Override
	public boolean hasNext()
	{

		return !cursorClosed && !cursor.isLast() || colonUitnodigingCategorie != ColonUitnodigingCategorie.U6;
	}

	@Override
	public ClientCategorieEntry next()
	{
		if (hasNext())
		{

			if (cursor.next())
			{
				Object[] data = cursor.get();
				Client client = (Client) data[0];

				var persoon = client.getPersoon();
				var adres = persoon.getGbaAdres();
				var gemeente = adres.getGbaGemeente();
				if (gemeente.getScreeningOrganisatie() == null)
				{
					if (!context.containsKey(SelectieConstants.GEMEENTE_ZONDER_SCREENING_ORGANISATIES))
					{
						Map<Long, String> map = new HashMap<>();
						context.put(SelectieConstants.GEMEENTE_ZONDER_SCREENING_ORGANISATIES, map);
					}
					Map<Long, String> map = (Map<Long, String>) context.get(SelectieConstants.GEMEENTE_ZONDER_SCREENING_ORGANISATIES);
					map.put(gemeente.getId(), gemeente.getNaam());
					context.put(SelectieConstants.GEMEENTE_ZONDER_SCREENING_ORGANISATIES, map);
					LOG.warn("Gemeente {} ({}) niet is gekoppeld aan een screeningsorganisatie. Daardoor kon er geen uitnodiging worden verstuurd naar Geen uitnodiging verstuurd "
						+ "naar client (id {})", gemeente.getNaam(), gemeente.getId(), client.getId());
					return next();
				}

				var andereClientMetActieveFit = fitService.getAndereClientOpZelfdeAdresEnActieveFit(client, uitgenodigdeClientIds);

				if (andereClientMetActieveFit == null)
				{
					cursorCount++;

					if (LOG.isDebugEnabled() && cursor.isLast())
					{
						LOG.debug("Categorie: {} aantal clienten geselecteerd: {}", colonUitnodigingCategorie.name(), cursorCount);
					}
					uitgenodigdeClientIds.add(client.getId());

					return new ClientCategorieEntry(client.getId(), colonUitnodigingCategorie, adres.getGbaGemeente().getScreeningOrganisatie().getId());
				}
				else
				{
					LOG.info("Client(id: '{}') overgeslagen omdat ook een andere client (id: '{}') op zelfde adres met lopende FIT woont", client.getId(),
						andereClientMetActieveFit.getId());
					return next();
				}
			}
			else
			{

				updateCursor();
				return next();
			}
		}
		return null;
	}

	@Override
	public void clear()
	{
		this.hibernateSession.clear();
	}

	@Override
	public void close()
	{
		cursorClosed = true;
		if (cursor != null)
		{
			this.cursor.close();
		}
	}

	private void updateCursor()
	{
		cursorClosed = true;
		cursorCount = 0;
		cursor.close();

		if (colonUitnodigingCategorie != ColonUitnodigingCategorie.U6)
		{
			if (ColonUitnodigingCategorie.U3.equals(colonUitnodigingCategorie))
			{
				colonUitnodigingCategorie = ColonUitnodigingCategorie.U4;
			}
			else if (ColonUitnodigingCategorie.U4.equals(colonUitnodigingCategorie))
			{
				colonUitnodigingCategorie = ColonUitnodigingCategorie.U6;
			}
			cursorClosed = false;
			setCursor();
		}
	}

	private void setCursor()
	{
		ExtendedSpecification<Client> specification = switch (colonUitnodigingCategorie)
		{
			case U3 -> ColonUitnodigingBaseSpecification.getSpecificationU3(vandaag);
			case U4 -> ColonUitnodigingBaseSpecification.getSpecificationU4(vandaag);
			case U6 -> ColonUitnodigingBaseSpecification.getSpecificationU6(vandaag);
			default -> throw new NotImplementedException("Niet bekende categorie");
		};
		cursor = clientRepository.findWith(specification,
				q -> q.sortBy((r, cb) ->
				{
					var orders = new ArrayList<>(ColonUitnodigingBaseSpecification.getU1OrderByList(r, cb));
					orders.add(0, cb.asc(laatsteScreeningRondeJoin(r, JoinType.INNER).get(ScreeningRonde_.statusDatum)));
					return orders;
				}))
			.setScrollFetchSize(fetchSize)
			.scroll(-1);
	}

	@Override
	public void saveState(ExecutionContext context)
	{
		context.put(UITNODIGINGSCAT, colonUitnodigingCategorie);
	}

	private void initialiseCursor(ExecutionContext context)
	{
		if (context.containsKey(UITNODIGINGSCAT))
		{
			colonUitnodigingCategorie = (ColonUitnodigingCategorie) context.get(UITNODIGINGSCAT);
		}

		setCursor();
	}

	@Override
	public void remove()
	{
		throw new UnsupportedOperationException("remove is not supported");

	}
}
