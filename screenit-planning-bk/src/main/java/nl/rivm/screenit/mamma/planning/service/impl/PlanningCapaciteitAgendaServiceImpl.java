package nl.rivm.screenit.mamma.planning.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-planning-bk
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.time.LocalTime;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.NavigableMap;
import java.util.Set;
import java.util.stream.Collectors;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.dto.mamma.planning.PlanningCapaciteitBlokDto;
import nl.rivm.screenit.exceptions.OpslaanVerwijderenTijdBlokException;
import nl.rivm.screenit.mamma.planning.controller.PlanningMapper;
import nl.rivm.screenit.mamma.planning.index.PlanningBlokIndex;
import nl.rivm.screenit.mamma.planning.index.PlanningScreeningsEenheidIndex;
import nl.rivm.screenit.mamma.planning.model.PlanningBlok;
import nl.rivm.screenit.mamma.planning.model.PlanningConstanten;
import nl.rivm.screenit.mamma.planning.model.PlanningDag;
import nl.rivm.screenit.mamma.planning.model.PlanningScreeningsEenheid;
import nl.rivm.screenit.mamma.planning.model.PlanningStandplaatsPeriode;
import nl.rivm.screenit.mamma.planning.model.PlanningWeek;
import nl.rivm.screenit.mamma.planning.service.PlanningCapaciteitAgendaService;
import nl.rivm.screenit.mamma.planning.service.PlanningCapaciteitBlokService;
import nl.rivm.screenit.mamma.planning.wijzigingen.PlanningWijzigingen;
import nl.rivm.screenit.mamma.planning.wijzigingen.PlanningWijzigingenRoute;
import nl.rivm.screenit.util.DateUtil;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.google.common.base.Preconditions;
import com.google.common.collect.Range;

@Transactional(propagation = Propagation.SUPPORTS)
@Service
@Slf4j
@AllArgsConstructor
public class PlanningCapaciteitAgendaServiceImpl implements PlanningCapaciteitAgendaService
{

	private final PlanningCapaciteitBlokService capaciteitBlokService;

	@Override
	public void herhalen(PlanningScreeningsEenheid bronScreeningsEenheid, PlanningScreeningsEenheid doelScreeningsEenheid, PlanningWeek teHerhalenWeek, LocalDate herhalenVanaf,
		LocalDate herhalenTotEnMet)
	{
		herhaal(teHerhalenWeek, doelScreeningsEenheid, herhalenVanaf, herhalenTotEnMet);

		if (herhalenTotEnMet == null)
		{
			long wekenVerschil = ChronoUnit.WEEKS.between(teHerhalenWeek.getDatum(), herhalenVanaf);
			if (wekenVerschil == 1 && bronScreeningsEenheid.equals(doelScreeningsEenheid))
			{

				doelScreeningsEenheid.setHerhalingsWeek(doelScreeningsEenheid.getWeek(teHerhalenWeek.getDatum()));
			}
			else
			{

				doelScreeningsEenheid.setHerhalingsWeek(doelScreeningsEenheid.getWeek(herhalenVanaf));
			}
		}
		else if (doelScreeningsEenheid.getHerhalingsWeek() != null)
		{
			doelScreeningsEenheid.setHerhalingsWeek(doelScreeningsEenheid.getWeek(herhalenTotEnMet.plusWeeks(1)));
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
	public PlanningDag kopieerDag(PlanningScreeningsEenheid bronScreeningsEenheid, PlanningScreeningsEenheid doelScreeningsEenheid, LocalDate bronDate, LocalTime bronVanTijd,
		LocalTime bronTotTijd, LocalDate doelDate) throws OpslaanVerwijderenTijdBlokException
	{
		var doelDag = doelScreeningsEenheid.getDag(doelDate);

		Preconditions.checkArgument(!doelDate.isBefore(PlanningConstanten.prognoseVanafDatum), "doelDag mag niet voor prognoseVanafDatum liggen");

		var bronDagBlokken = capaciteitBlokService.getCapaciteitsBlokkenVanDag(bronScreeningsEenheid, bronDate)
			.stream().filter(blok -> heeftOverlap(bronVanTijd, bronTotTijd, blok.getVanaf(), blok.getTot())).collect(Collectors.toList());

		var vanTijd = bronDagBlokken.stream().min(Comparator.comparing(PlanningBlok::getVanaf)).map(PlanningBlok::getVanaf).orElse(bronVanTijd);
		var totTijd = bronDagBlokken.stream().max(Comparator.comparing(PlanningBlok::getTot)).map(PlanningBlok::getTot).orElse(bronTotTijd);

		if (bronVanTijd.isBefore(vanTijd))
		{
			vanTijd = bronVanTijd;
		}
		if (bronTotTijd.isAfter(totTijd))
		{
			totTijd = bronTotTijd;
		}

		if (doelDag != null)
		{
			var blokkenOpDoeldag = Set.copyOf(doelDag.getBlokSet());
			for (var blok : blokkenOpDoeldag)
			{
				if (heeftOverlap(vanTijd, totTijd, blok.getVanaf(), blok.getTot()))
				{
					capaciteitBlokService.verwijderBlok(blok);
				}
			}
		}
		else
		{
			doelDag = new PlanningDag(doelDate, doelScreeningsEenheid);
			doelScreeningsEenheid.putDag(doelDate, doelDag);
		}
		for (var blok : bronDagBlokken)
		{
			var nieuwBlok = kopieerBlok(blok, doelScreeningsEenheid, doelDate);
			capaciteitBlokService.maakBlok(nieuwBlok);
		}
		return doelDag;
	}

	protected boolean heeftOverlap(LocalTime vanTijdA, LocalTime totTijdA, LocalTime vanTijdB, LocalTime totTijdB)
	{
		var rangeA = Range.open(vanTijdA, totTijdA);
		var rangeB = Range.open(vanTijdB, totTijdB);

		return rangeB.contains(rangeA.lowerEndpoint()) || rangeB.contains(rangeA.upperEndpoint()) || rangeA.encloses(rangeB);
	}

	private PlanningCapaciteitBlokDto kopieerBlok(PlanningBlok bronBlok, PlanningScreeningsEenheid doelScreeningsEenheid, LocalDate doelDate)
	{
		var nieuwBlok = PlanningMapper.from(bronBlok);
		var huidigeVanafDatumTijd = DateUtil.toLocalDateTime(bronBlok.getDateVanaf());
		var huidigeTotDatumTijd = DateUtil.toLocalDateTime(bronBlok.getDateTot());

		nieuwBlok.vanaf = DateUtil.toUtilDate(huidigeVanafDatumTijd.with(doelDate));
		nieuwBlok.tot = DateUtil.toUtilDate(huidigeTotDatumTijd.with(doelDate));
		nieuwBlok.id = null;
		nieuwBlok.conceptId = null;
		nieuwBlok.screeningsEenheidId = doelScreeningsEenheid.getId();

		return nieuwBlok;
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
}
