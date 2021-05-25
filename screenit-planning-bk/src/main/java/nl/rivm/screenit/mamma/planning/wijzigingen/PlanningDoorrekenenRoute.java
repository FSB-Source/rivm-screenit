package nl.rivm.screenit.mamma.planning.wijzigingen;

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

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDate;
import java.time.temporal.ChronoUnit;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.NavigableSet;
import java.util.Set;

import nl.rivm.screenit.mamma.planning.index.PlanningBlokkadeIndex;
import nl.rivm.screenit.mamma.planning.model.PlanningBenodigdJaar;
import nl.rivm.screenit.mamma.planning.model.PlanningBeschikbaar;
import nl.rivm.screenit.mamma.planning.model.PlanningBlok;
import nl.rivm.screenit.mamma.planning.model.PlanningBlokkade;
import nl.rivm.screenit.mamma.planning.model.PlanningConstanten;
import nl.rivm.screenit.mamma.planning.model.PlanningDag;
import nl.rivm.screenit.mamma.planning.model.PlanningMelding;
import nl.rivm.screenit.mamma.planning.model.PlanningScreeningsEenheid;
import nl.rivm.screenit.mamma.planning.model.PlanningScreeningsOrganisatie;
import nl.rivm.screenit.mamma.planning.model.PlanningStandplaats;
import nl.rivm.screenit.mamma.planning.model.PlanningStandplaatsPeriode;
import nl.rivm.screenit.mamma.planning.model.PlanningStandplaatsRonde;
import nl.rivm.screenit.mamma.planning.model.PlanningWeek;
import nl.rivm.screenit.model.mamma.enums.MammaCapaciteitBlokType;
import nl.rivm.screenit.model.mamma.enums.MammaMeldingNiveau;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

enum PlanningDoorrekenenRoute
{
	;

	private static final Logger LOG = LoggerFactory.getLogger(PlanningDoorrekenenRoute.class);

	static void run(PlanningBlok blok)
	{
		LOG.trace("run blok: " + blok.getId());

		BigDecimal totaal = new BigDecimal(blok.getAantalOnderzoeken());
		MammaCapaciteitBlokType blokType = blok.getCapaciteitBlokType();

		PlanningScreeningsOrganisatie screeningsOrganisatie = blok.getScreeningsEenheid().getScreeningsOrganisatie();
		if (blokType == MammaCapaciteitBlokType.TEHUIS)
		{
			totaal = totaal.multiply(screeningsOrganisatie.getFactorDubbeleTijd());
		}

		PlanningBeschikbaar beschikbaar = blok.getBeschikbaar();
		beschikbaar.clear();
		beschikbaar.add(totaal, blokType);
	}

	static void run(PlanningDag dag)
	{
		LOG.trace("run dag: " + dag.getDatum());

		PlanningBeschikbaar beschikbaar = dag.getBeschikbaar();
		beschikbaar.clear();

		for (PlanningBlok blok : dag.getBlokSet())
		{
			beschikbaar.add(blok.getBeschikbaar());
		}
	}

	static void run(PlanningWeek week)
	{
		LOG.trace("run week: " + week.getDatum());

		PlanningBeschikbaar beschikbaar = week.getBeschikbaar();
		beschikbaar.clear();

		for (PlanningDag dag : week.getDagList())
		{
			beschikbaar.add(dag.getBeschikbaar());
		}
	}

	static void run(PlanningStandplaatsPeriode standplaatsPeriode)
	{
		LOG.debug("run standplaatsPeriode: " + standplaatsPeriode.getId() + " volgnr" + standplaatsPeriode.getScreeningsEenheidVolgNr());

		PlanningScreeningsEenheid screeningsEenheid = standplaatsPeriode.getScreeningsEenheid();
		Map<LocalDate, Set<PlanningBlokkade>> screeningsOrganisatieBlokkadeDatumMap = PlanningBlokkadeIndex.getBlokkadeDatumMap(screeningsEenheid.getScreeningsOrganisatie());
		Map<LocalDate, Set<PlanningBlokkade>> screeningsEenheidBlokkadeDatumMap = PlanningBlokkadeIndex.getBlokkadeDatumMap(screeningsEenheid);

		DoorrekenenStandplaatsPeriodeContext context = new DoorrekenenStandplaatsPeriodeContext(standplaatsPeriode);
		Iterator<PlanningStandplaatsPeriode> standplaatsPeriodeIterator = screeningsEenheid.getStandplaatsPeriodeNavigableSet().tailSet(standplaatsPeriode, false)
			.iterator();

		for (LocalDate datum = standplaatsPeriode.getVanaf(); datum.compareTo(PlanningConstanten.plannenTotEnMetDatum) <= 0; datum = datum.plusDays(1))
		{
			PlanningDag dag = context.screeningsEenheid.getDagNavigableMap().get(datum);
			dag.setStandplaatsPeriode(standplaatsPeriode);

			dag.getBlokkadeSet().clear();
			addBlokkadeSet(dag, screeningsOrganisatieBlokkadeDatumMap);
			addBlokkadeSet(dag, screeningsEenheidBlokkadeDatumMap);
			addBlokkadeSet(dag, context.standplaatsBlokkadeDatumMap);

			if (standplaatsPeriode != null)
			{
				if (!standplaatsPeriode.gesplitst() && !context.oudCorrectieToegepast && !datum.isBefore(context.jaarovergang))
				{
					PlanningBenodigdJaar benodigdEersteJaarStandplaatsRonde = context.standplaats.getBenodigd().get(context.jaarEersteStandplaatsPeriode);
					BigDecimal benodigdStandplaatsTotaal = benodigdEersteJaarStandplaatsRonde.getTotaal();
					BigDecimal benodigdStandplaatsTehuis = benodigdEersteJaarStandplaatsRonde.getTotaalTehuis();
					if (context.jaar > context.jaarEersteStandplaatsPeriode)
					{
						PlanningBenodigdJaar benodigdJaar = context.standplaats.getBenodigd().get(context.jaar);
						benodigdStandplaatsTotaal = benodigdStandplaatsTotaal.add(benodigdJaar.getNieuw());
						benodigdStandplaatsTehuis = benodigdStandplaatsTehuis.add(benodigdJaar.getNieuwTehuis());
					}

					BigDecimal benodigdVoorJaarovergangTotaal = BigDecimal.ZERO;
					BigDecimal benodigdVoorJaarovergangTehuis = BigDecimal.ZERO;
					for (PlanningStandplaatsPeriode sp : context.standplaatsRonde.getStandplaatsPeriodeNavigableSet())
					{
						benodigdVoorJaarovergangTotaal = benodigdVoorJaarovergangTotaal.add(sp.getBeschikbaarVoorJaarovergangTotaal());
						benodigdVoorJaarovergangTehuis = benodigdVoorJaarovergangTehuis.add(sp.getBeschikbaarVoorJaarovergangTehuis());
					}

					context.benodigdTotaalRestant = context.benodigdTotaalRestant
						.subtract(oudCorrectie(benodigdStandplaatsTotaal, benodigdVoorJaarovergangTotaal, benodigdEersteJaarStandplaatsRonde.getOud()));

					context.benodigdTehuisRestant = context.benodigdTehuisRestant
						.subtract(oudCorrectie(benodigdStandplaatsTehuis, benodigdVoorJaarovergangTehuis, benodigdEersteJaarStandplaatsRonde.getOudTehuis()));

					context.oudCorrectieToegepast = true;
				}

				boolean benodigdRestantNegatief = false;
				if (dag.getBlokkadeSet().isEmpty())
				{
					BigDecimal dagBeschikbaarTotaal = dag.getBeschikbaar().getTotaal();
					BigDecimal dagBeschikbaarTehuis = dag.getBeschikbaar().getTotaalTehuis();
					context.benodigdTotaalRestant = context.benodigdTotaalRestant.subtract(dagBeschikbaarTotaal);
					context.benodigdTehuisRestant = context.benodigdTehuisRestant.subtract(dag.getBeschikbaar().getTotaalTehuis());
					benodigdRestantNegatief = context.benodigdTotaalRestant.compareTo(BigDecimal.ZERO) <= 0;
					standplaatsPeriode.add(datum.toEpochDay(), dagBeschikbaarTotaal, dagBeschikbaarTehuis, datum.isBefore(context.jaarovergang));
				}
				else
				{
					standplaatsPeriode.getBlokkadeNavigableSet().addAll(dag.getBlokkadeSet());
				}

				if (standplaatsPeriode.getPrognose() && benodigdRestantNegatief || !standplaatsPeriode.getPrognose() && standplaatsPeriode.getTotEnMet().equals(datum))
				{
					context.standplaatsPeriodeTotEnMet = datum;
					if (context.jaar != context.standplaatsPeriodeTotEnMet.getYear())
					{
						for (int j = context.jaar + 1; j <= datum.getYear(); j++)
						{
							PlanningBenodigdJaar benodigdJaar = context.standplaats.getBenodigd().get(j);
							context.benodigdTotaalRestant = context.benodigdTotaalRestant.add(benodigdJaar.getNieuw());
							context.benodigdTehuisRestant = context.benodigdTehuisRestant.add(benodigdJaar.getNieuwTehuis());
							if (context.isEersteStandplaatsRonde)
							{
								benodigdJaar = context.standplaats.getTransport().get(j);
								context.benodigdTotaalRestant = context.benodigdTotaalRestant.add(benodigdJaar.getNieuw());
								context.benodigdTehuisRestant = context.benodigdTehuisRestant.add(benodigdJaar.getNieuwTehuis());
							}
							else
							{
								BigDecimal eersteOnderzoekCorrectieRestant = benodigdJaar.getEersteOnderzoekCorrectie()
									.subtract(context.standplaats.getBenodigd().get(context.jaar).getEersteOnderzoekCorrectie());
								context.benodigdTotaalRestant = context.benodigdTotaalRestant.add(eersteOnderzoekCorrectieRestant);
							}
						}
						context.jaar = datum.getYear();
						benodigdRestantNegatief = context.benodigdTotaalRestant.compareTo(BigDecimal.ZERO) <= 0;
					}

					if (benodigdRestantNegatief || !standplaatsPeriode.getPrognose())
					{
						bepaalMeldingen(context, dag);

						standplaatsPeriode.setTotEnMet(context.standplaatsPeriodeTotEnMet);
						if (standplaatsPeriode.gesplitst())
						{
							standplaatsPeriode.unlock();
						}

						if (standplaatsPeriodeIterator.hasNext())
						{
							standplaatsPeriode = standplaatsPeriodeIterator.next();
							context = new DoorrekenenStandplaatsPeriodeContext(standplaatsPeriode, context.standplaatsPeriodeTotEnMet.plusDays(1));
						}
						else
						{
							standplaatsPeriode = null;
						}
					}
				}
			}
		}

		if (standplaatsPeriode != null)
		{
			standplaatsPeriode.setTotEnMet(standplaatsPeriode.getVanaf());
			List<PlanningMelding> meldingList = context.standplaatsRonde.getMeldingList();
			meldingList.clear();
			meldingList.add(new PlanningMelding("Er zijn te weinig onderzoeken beschikbaar om een prognose te maken", MammaMeldingNiveau.PROBLEEM));
			while (standplaatsPeriodeIterator.hasNext())
			{
				PlanningStandplaatsPeriode vorigeStandplaatsPeriode = standplaatsPeriode;

				if (standplaatsPeriode.gesplitst())
				{
					standplaatsPeriode.unlock();
				}
				standplaatsPeriode = standplaatsPeriodeIterator.next();
				standplaatsPeriode.setVanaf(vorigeStandplaatsPeriode.getTotEnMet().plusDays(1));
				standplaatsPeriode.setTotEnMet(standplaatsPeriode.getVanaf());
				meldingList = standplaatsPeriode.getStandplaatsRonde().getMeldingList();
				meldingList.clear();
				meldingList.add(new PlanningMelding("Zie voorgaande standplaatsperiode", MammaMeldingNiveau.PROBLEEM));
			}
		}
	}

	private static void addBlokkadeSet(PlanningDag dag, Map<LocalDate, Set<PlanningBlokkade>> blokkadeDatumMap)
	{
		if (blokkadeDatumMap != null)
		{
			Set<PlanningBlokkade> blokkadeSet = blokkadeDatumMap.get(dag.getDatum());
			if (blokkadeSet != null)
			{
				dag.getBlokkadeSet().addAll(blokkadeSet);
			}
		}
	}

	private static void bepaalMeldingen(DoorrekenenStandplaatsPeriodeContext context, PlanningDag laatsteDag)
	{
		PlanningScreeningsOrganisatie screeningsOrganisatie = context.standplaatsRonde.getStandplaatsPeriodeNavigableSet().iterator().next().getScreeningsEenheid()
			.getScreeningsOrganisatie();

		List<PlanningMelding> meldingList = context.standplaatsRonde.getMeldingList();
		meldingList.clear();

		BigDecimal aantalBasisOnderzoeken = context.benodigdTotaalRestant.abs().setScale(1, BigDecimal.ROUND_UP);
		BigDecimal aantalTehuisOnderzoekenBasis = context.benodigdTehuisRestant.abs().setScale(1, BigDecimal.ROUND_UP);
		BigDecimal aantalTehuisOnderzoeken = context.benodigdTehuisRestant.abs().divide(screeningsOrganisatie.getFactorDubbeleTijd(), 1, BigDecimal.ROUND_UP);

		meldingList.add(new PlanningMelding(
			aantalBasisOnderzoeken + " " + (context.benodigdTotaalRestant.compareTo(BigDecimal.ZERO) < 0 ? "te veel" : "te weinig"),
			bepaalMeldingNiveau(context.benodigdTotaalRestant, laatsteDag.getBeschikbaar().getTotaal())));

		meldingList.add(new PlanningMelding(
			aantalTehuisOnderzoeken + " tehuis " + (context.benodigdTehuisRestant.compareTo(BigDecimal.ZERO) < 0 ? "te veel" : "te weinig")
				+ " (" + aantalTehuisOnderzoekenBasis + " regulier)",
			bepaalMeldingNiveau(context.benodigdTehuisRestant, laatsteDag.getBeschikbaar().getTotaalTehuis())));
	}

	private static MammaMeldingNiveau bepaalMeldingNiveau(BigDecimal benodigdRestant, BigDecimal beschikbaarLaatsteDag)
	{
		if (benodigdRestant.compareTo(BigDecimal.ZERO) <= 0)
		{
			if (benodigdRestant.add(beschikbaarLaatsteDag).compareTo(BigDecimal.ZERO) >= 0)
			{
				return MammaMeldingNiveau.INFO;
			}
			else
			{
				return MammaMeldingNiveau.WAARSCHUWING;
			}
		}
		else
		{
			return MammaMeldingNiveau.PROBLEEM;
		}
	}

	private static BigDecimal oudCorrectie(BigDecimal benodigd, BigDecimal benodigdVoorJaarovergang, BigDecimal benodigdOud)
	{
		if (benodigd.compareTo(BigDecimal.ZERO) != 0)
		{
			BigDecimal percentageOud = benodigdOud.divide(benodigd, 6, RoundingMode.HALF_UP);
			BigDecimal benodigdTotaalNaJaarovergang = benodigd.subtract(benodigdVoorJaarovergang);

			return benodigdTotaalNaJaarovergang.multiply(percentageOud);
		}
		else
		{
			return BigDecimal.ZERO;
		}

	}

	static void run(PlanningStandplaatsRonde standplaatsRonde)
	{
		LOG.debug("run standplaatsRonde: " + standplaatsRonde.getId());

		standplaatsRonde.setNiveau(MammaMeldingNiveau.INFO);
		for (PlanningMelding melding : standplaatsRonde.getMeldingList())
		{
			if (standplaatsRonde.getNiveau().compareTo(melding.getNiveau()) < 0)
			{
				standplaatsRonde.setNiveau(melding.getNiveau());
				if (standplaatsRonde.getNiveau() == MammaMeldingNiveau.PROBLEEM)
				{
					break;
				}
			}
		}

		BigDecimal somGewogenDatum = BigDecimal.ZERO;
		BigDecimal beschikbaarTotaal = BigDecimal.ZERO;
		for (PlanningStandplaatsPeriode standplaatsPeriode : standplaatsRonde.getStandplaatsPeriodeNavigableSet())
		{
			somGewogenDatum = somGewogenDatum.add(standplaatsPeriode.getSomGewogenDatum());
			beschikbaarTotaal = beschikbaarTotaal.add(standplaatsPeriode.getBeschikbaarTotaal());
		}

		BigDecimal gewogenGemiddeldeDatum = somGewogenDatum.divide(
			beschikbaarTotaal.compareTo(BigDecimal.ZERO) == 0 ? new BigDecimal(standplaatsRonde.getStandplaatsPeriodeNavigableSet().size()) : beschikbaarTotaal, 0,
			BigDecimal.ROUND_HALF_UP);
		long epochDay = gewogenGemiddeldeDatum.longValue();
		if (epochDay != 0)
		{
			int wekenVanTevorenUitnodigen = standplaatsRonde.getStandplaats().getScreeningsOrganisatie().getWekenVanTevorenUitnodigen();
			standplaatsRonde.setGewogenGemiddeldeDatum(LocalDate.ofEpochDay(epochDay - wekenVanTevorenUitnodigen * 7));
		}
		else
		{

			standplaatsRonde.setGewogenGemiddeldeDatum(standplaatsRonde.getStandplaatsPeriodeNavigableSet().first().getVanaf());
		}

		standplaatsRonde.setBeschikbaarTotaal(beschikbaarTotaal);

		LocalDate vorigeGewogenGemiddeldeDatum = null;
		PlanningStandplaats standplaats = standplaatsRonde.getStandplaats();
		if (standplaatsRonde.getId() != null)
		{
			PlanningStandplaatsRonde vorigeStandplaatsRonde = standplaats.getStandplaatsRondeNavigableSet().lower(standplaatsRonde);
			if (vorigeStandplaatsRonde != null)
			{
				vorigeGewogenGemiddeldeDatum = vorigeStandplaatsRonde.getGewogenGemiddeldeDatum();
			}
		}
		if (vorigeGewogenGemiddeldeDatum == null)
		{
			vorigeGewogenGemiddeldeDatum = standplaats.getVorigeGewogenGemiddeldeDatum();
		}
		if (vorigeGewogenGemiddeldeDatum != null)
		{
			standplaatsRonde.setInterval(new BigDecimal(ChronoUnit.DAYS.between(vorigeGewogenGemiddeldeDatum, standplaatsRonde.getGewogenGemiddeldeDatum())));
		}
		else
		{
			standplaatsRonde.setInterval(null);
		}
	}

	static void run(PlanningScreeningsEenheid screeningsEenheid)
	{
		LOG.debug("run screeningsEenheid: " + screeningsEenheid.getId());

		Set<PlanningStandplaatsRonde> standplaatsRondeSet = new HashSet<>();

		screeningsEenheid.setNiveau(MammaMeldingNiveau.INFO);
		for (PlanningStandplaatsPeriode standplaatsPeriode : screeningsEenheid.getStandplaatsPeriodeNavigableSet())
		{
			PlanningStandplaatsRonde standplaatsRonde = standplaatsPeriode.getStandplaatsRonde();
			standplaatsRondeSet.add(standplaatsRonde);
			if (screeningsEenheid.getNiveau().compareTo(standplaatsRonde.getNiveau()) < 0)
			{
				screeningsEenheid.setNiveau(standplaatsRonde.getNiveau());
			}
		}

		BigDecimal somGewogenInterval = BigDecimal.ZERO;
		BigDecimal beschikbaarTotaal = BigDecimal.ZERO;
		for (PlanningStandplaatsRonde standplaatsRonde : standplaatsRondeSet)
		{
			BigDecimal interval = standplaatsRonde.getInterval();
			if (interval != null)
			{
				somGewogenInterval = somGewogenInterval.add(interval.multiply(standplaatsRonde.getBeschikbaarTotaal()));
				beschikbaarTotaal = beschikbaarTotaal.add(standplaatsRonde.getBeschikbaarTotaal());
			}
		}
		if (!(beschikbaarTotaal.compareTo(BigDecimal.ZERO) == 0))
		{
			screeningsEenheid.setInterval(somGewogenInterval.divide(beschikbaarTotaal, 5, BigDecimal.ROUND_HALF_UP));
		}
		else
		{
			screeningsEenheid.setInterval(null);
		}
	}
}

class DoorrekenenStandplaatsPeriodeContext
{
	PlanningScreeningsEenheid screeningsEenheid;

	PlanningStandplaatsRonde standplaatsRonde;

	PlanningStandplaats standplaats;

	PlanningStandplaatsPeriode eersteStandplaatsPeriode;

	int jaarEersteStandplaatsPeriode;

	int jaar;

	boolean oudCorrectieToegepast;

	BigDecimal benodigdTotaalRestant;

	BigDecimal benodigdTehuisRestant;

	boolean isEersteStandplaatsRonde;

	NavigableSet<PlanningStandplaatsPeriode> voorgaandeStandplaatsPeriodeSet;

	Map<LocalDate, Set<PlanningBlokkade>> standplaatsBlokkadeDatumMap;

	LocalDate jaarovergang;

	LocalDate standplaatsPeriodeTotEnMet;

	DoorrekenenStandplaatsPeriodeContext(PlanningStandplaatsPeriode standplaatsPeriode)
	{
		PlanningStandplaatsPeriode vorigeStandplaatsPeriode = standplaatsPeriode.getScreeningsEenheid().getStandplaatsPeriodeNavigableSet().lower(standplaatsPeriode);
		LocalDate vanaf = vorigeStandplaatsPeriode != null ? vorigeStandplaatsPeriode.getTotEnMet().plusDays(1) : standplaatsPeriode.getVanaf();
		init(standplaatsPeriode, vanaf);
	}

	DoorrekenenStandplaatsPeriodeContext(PlanningStandplaatsPeriode standplaatsPeriode, LocalDate vanaf)
	{
		init(standplaatsPeriode, vanaf);
	}

	private void init(PlanningStandplaatsPeriode standplaatsPeriode, LocalDate vanaf)
	{
		screeningsEenheid = standplaatsPeriode.getScreeningsEenheid();
		standplaatsRonde = standplaatsPeriode.getStandplaatsRonde();
		standplaats = standplaatsRonde.getStandplaats();
		standplaatsPeriode.setVanaf(vanaf);

		eersteStandplaatsPeriode = standplaatsRonde.getStandplaatsPeriodeNavigableSet().first();
		jaarEersteStandplaatsPeriode = eersteStandplaatsPeriode.getVanaf()
			.minusWeeks(screeningsEenheid.getScreeningsOrganisatie().getWekenVanTevorenUitnodigen()).getYear();
		jaar = jaarEersteStandplaatsPeriode;
		oudCorrectieToegepast = false;

		PlanningBenodigdJaar benodigdJaar = standplaats.getBenodigd().get(jaar);
		benodigdTotaalRestant = benodigdJaar.getTotaal();
		benodigdTehuisRestant = benodigdJaar.getTotaalTehuis();

		isEersteStandplaatsRonde = standplaatsRonde.equals(standplaats.getStandplaatsRondeNavigableSet().first());
		if (isEersteStandplaatsRonde)
		{
			benodigdJaar = standplaats.getTransport().get(jaar);
			benodigdTotaalRestant = benodigdTotaalRestant.add(benodigdJaar.getTotaal());
			benodigdTehuisRestant = benodigdTehuisRestant.add(benodigdJaar.getTotaalTehuis());
		}
		else
		{
			benodigdTotaalRestant = benodigdTotaalRestant.add(benodigdJaar.getEersteOnderzoekCorrectie());
		}

		voorgaandeStandplaatsPeriodeSet = standplaatsPeriode.getStandplaatsRonde().getStandplaatsPeriodeNavigableSet().headSet(standplaatsPeriode, false);
		if (!voorgaandeStandplaatsPeriodeSet.isEmpty())
		{
			voorgaandeStandplaatsPeriodeSet.last().await();

			for (PlanningStandplaatsPeriode voorgaandeStandplaatsPeriode : voorgaandeStandplaatsPeriodeSet)
			{
				benodigdTotaalRestant = benodigdTotaalRestant.subtract(voorgaandeStandplaatsPeriode.getBeschikbaarTotaal());
				benodigdTehuisRestant = benodigdTehuisRestant.subtract(voorgaandeStandplaatsPeriode.getBeschikbaarTehuis());
			}
		}

		standplaatsBlokkadeDatumMap = PlanningBlokkadeIndex.getBlokkadeDatumMap(standplaats);
		standplaatsPeriode.clear();
		standplaatsPeriode.getBlokkadeNavigableSet().clear();

		jaarovergang = LocalDate.of(jaar + 1, 1, 1)
			.plusWeeks(screeningsEenheid.getScreeningsOrganisatie().getWekenVanTevorenUitnodigen());
	}
}
