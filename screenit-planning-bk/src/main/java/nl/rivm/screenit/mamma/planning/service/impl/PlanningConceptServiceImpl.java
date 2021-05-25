package nl.rivm.screenit.mamma.planning.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-planning-bk
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Collection;
import java.util.NavigableMap;
import java.util.Set;

import nl.rivm.screenit.datasource.DataSourceRouter;
import nl.rivm.screenit.exceptions.DryRunException;
import nl.rivm.screenit.exceptions.OpslaanAfsprakenBuitenStandplaatsPeriodeException;
import nl.rivm.screenit.exceptions.OpslaanVerwijderenTijdBlokException;
import nl.rivm.screenit.mamma.planning.index.PlanningBlokIndex;
import nl.rivm.screenit.mamma.planning.index.PlanningScreeningsEenheidIndex;
import nl.rivm.screenit.mamma.planning.index.PlanningScreeningsOrganisatieIndex;
import nl.rivm.screenit.mamma.planning.model.PlanningBlok;
import nl.rivm.screenit.mamma.planning.model.PlanningDag;
import nl.rivm.screenit.mamma.planning.model.PlanningScreeningsEenheid;
import nl.rivm.screenit.mamma.planning.model.PlanningScreeningsOrganisatie;
import nl.rivm.screenit.mamma.planning.model.PlanningStandplaatsPeriode;
import nl.rivm.screenit.mamma.planning.model.PlanningWeek;
import nl.rivm.screenit.mamma.planning.service.PlanningConceptOpslaanService;
import nl.rivm.screenit.mamma.planning.service.PlanningConceptService;
import nl.rivm.screenit.mamma.planning.wijzigingen.PlanningDoorrekenenManager;
import nl.rivm.screenit.mamma.planning.wijzigingen.PlanningWijzigingen;
import nl.rivm.screenit.mamma.planning.wijzigingen.PlanningWijzigingenRoute;

import org.hibernate.SessionFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS)
public class PlanningConceptServiceImpl implements PlanningConceptService
{
	private static final Logger LOG = LoggerFactory.getLogger(PlanningConceptServiceImpl.class);

	@Autowired
	private PlanningConceptOpslaanService conceptOpslaanService;

	@Autowired
	SessionFactory sessionFactory;

	@Override
	public void herhalen(PlanningScreeningsEenheid screeningsEenheidVan, PlanningScreeningsEenheid screeningsEenheidNaar, PlanningWeek teHerhalenWeek, LocalDate herhalenVanaf,
		LocalDate herhalenTotEnMet)
	{
		herhaal(teHerhalenWeek, screeningsEenheidNaar, herhalenVanaf, herhalenTotEnMet);

		if (herhalenTotEnMet == null)
		{
			long wekenVerschil = ChronoUnit.WEEKS.between(teHerhalenWeek.getDatum(), herhalenVanaf);
			if (wekenVerschil == 1 && screeningsEenheidVan.equals(screeningsEenheidNaar))
			{

				screeningsEenheidNaar.setHerhalingsWeek(screeningsEenheidNaar.getWeek(teHerhalenWeek.getDatum()));
			}
			else
			{

				screeningsEenheidNaar.setHerhalingsWeek(screeningsEenheidNaar.getWeek(herhalenVanaf));
			}
		}
		else if (screeningsEenheidNaar.getHerhalingsWeek() != null)
		{
			screeningsEenheidNaar.setHerhalingsWeek(screeningsEenheidNaar.getWeek(herhalenTotEnMet.plusWeeks(1)));
		}
	}

	private void herhaal(PlanningWeek teHerhalenWeek, PlanningScreeningsEenheid naarScreeningsEenheid, LocalDate herhalenVanaf, LocalDate herhalenTotEnMet)
	{
		NavigableMap<LocalDate, PlanningWeek> weekNavigableMapNaar = naarScreeningsEenheid.getWeekNavigableMap();
		if (herhalenTotEnMet == null)
		{
			herhalenTotEnMet = weekNavigableMapNaar.lastKey();
		}

		if (herhalenVanaf.compareTo(herhalenTotEnMet) <= 0)
		{
			PlanningWijzigingenRoute wijzigingenRoute = PlanningWijzigingen.getWijzigingenRoute(naarScreeningsEenheid);

			Set<PlanningBlok> naarScreeningsEenheidBlokSet = naarScreeningsEenheid.getBlokSet();

			Collection<PlanningWeek> naarWeekCollection = weekNavigableMapNaar.subMap(herhalenVanaf, true, herhalenTotEnMet, true).values();

			ArrayList<PlanningBlok> deletedBlokList = new ArrayList<>();
			ArrayList<PlanningBlok> changedBlokList = new ArrayList<>();

			for (PlanningWeek naarWeek : naarWeekCollection)
			{
				for (int i = 0; i < 7; i++)
				{
					PlanningDag naarDag = naarWeek.getDagList().get(i);
					PlanningDag teHerhalenDag = teHerhalenWeek.getDagList().get(i);

					Set<PlanningBlok> naarDagBlokSet = naarDag.getBlokSet();
					deletedBlokList.addAll(naarDagBlokSet);
					naarScreeningsEenheidBlokSet.removeAll(naarDagBlokSet);
					naarDagBlokSet.clear();

					for (PlanningBlok teHerhalenBlok : teHerhalenDag.getBlokSet())
					{
						PlanningBlok naarBlok = new PlanningBlok(null,
							teHerhalenBlok.getVanaf(),
							teHerhalenBlok.getTot(),
							teHerhalenBlok.getAantalOnderzoeken(),
							teHerhalenBlok.getCapaciteitBlokType(),
							teHerhalenBlok.getOpmerkingen(),
							teHerhalenBlok.isMinderValideAfspraakMogelijk());

						changedBlokList.add(naarBlok);

						naarBlok.setScreeningsEenheid(naarScreeningsEenheid);
						naarScreeningsEenheidBlokSet.add(naarBlok);

						naarBlok.setDag(naarDag);
						naarDagBlokSet.add(naarBlok);

						PlanningBlokIndex.put(naarBlok);

						wijzigingenRoute.getBlokSet().add(naarBlok);
					}

					wijzigingenRoute.getDagSet().add(naarDag);
				}

				wijzigingenRoute.getWeekSet().add(naarWeek);
			}

			PlanningBlokIndex.deleted(naarScreeningsEenheid, deletedBlokList);
			PlanningBlokIndex.changed(naarScreeningsEenheid, changedBlokList);

			PlanningStandplaatsPeriode standplaatsPeriode = naarScreeningsEenheid.getDagNavigableMap().get(herhalenVanaf).getStandplaatsPeriode();
			if (standplaatsPeriode != null)
			{
				wijzigingenRoute.setVanafStandplaatsPeriode(standplaatsPeriode);
			}
		}
	}

	@Override
	public void herhalen(LocalDate herhalenVanaf)
	{
		for (PlanningScreeningsEenheid screeningsEenheid : PlanningScreeningsEenheidIndex.getScreeningsEenheden())
		{
			PlanningWeek herhalingsWeek = screeningsEenheid.getHerhalingsWeek();
			herhaal(herhalingsWeek, screeningsEenheid, herhalenVanaf, null);
		}
	}

	@Override
	public void opslaan()
	{
		DataSourceRouter.useReadWrite();

		PlanningDoorrekenenManager.run();

		for (PlanningScreeningsOrganisatie screeningsOrganisatie : PlanningScreeningsOrganisatieIndex.getScreeningsOrganisaties())
		{
			try
			{
				conceptOpslaanService.opslaan(screeningsOrganisatie.getId(), false);
			}
			catch (OpslaanVerwijderenTijdBlokException | OpslaanAfsprakenBuitenStandplaatsPeriodeException | DryRunException e)
			{
				LOG.error("Niet mogelijk om het concept op te slaan voor so: " + screeningsOrganisatie.getId(), e);
				throw new IllegalStateException(e);
			}
		}
	}
}
