package nl.rivm.screenit.mamma.planning.controller;

/*-
 * ========================LICENSE_START=================================
 * screenit-planning-bk
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.util.stream.Collectors;

import nl.rivm.screenit.dto.mamma.planning.PlanningBlokkadeDto;
import nl.rivm.screenit.dto.mamma.planning.PlanningCapaciteitBlokDto;
import nl.rivm.screenit.dto.mamma.planning.PlanningMeldingenDto;
import nl.rivm.screenit.dto.mamma.planning.PlanningScreeningsEenheidMetaDataDto;
import nl.rivm.screenit.dto.mamma.planning.PlanningStandplaatsPeriodeDto;
import nl.rivm.screenit.mamma.planning.index.PlanningScreeningsEenheidIndex;
import nl.rivm.screenit.mamma.planning.index.PlanningScreeningsOrganisatieIndex;
import nl.rivm.screenit.mamma.planning.index.PlanningStandplaatsIndex;
import nl.rivm.screenit.mamma.planning.model.PlanningBlok;
import nl.rivm.screenit.mamma.planning.model.PlanningBlokkade;
import nl.rivm.screenit.mamma.planning.model.PlanningMelding;
import nl.rivm.screenit.mamma.planning.model.PlanningScreeningsEenheid;
import nl.rivm.screenit.mamma.planning.model.PlanningScreeningsOrganisatie;
import nl.rivm.screenit.mamma.planning.model.PlanningStandplaatsPeriode;
import nl.rivm.screenit.mamma.planning.model.PlanningStandplaatsRonde;
import nl.rivm.screenit.util.DateUtil;

public class PlanningMapper
{
	private static final BigDecimal DAGEN_PER_MAAND = new BigDecimal(365 * 3 + 366).divide(new BigDecimal(12 * 4));

	public static PlanningBlokkade from(PlanningBlokkadeDto blokkadeDto)
	{
		PlanningBlokkade blokkade = new PlanningBlokkade(
			blokkadeDto.id,
			blokkadeDto.blokkadeType,
			null,
			null,
			null,
			DateUtil.toLocalDate(blokkadeDto.vanaf),
			DateUtil.toLocalDate(blokkadeDto.totEnMet));

		switch (blokkade.getBlokkadeType())
		{
		case SCREENINGS_ORGANISATIE:
			blokkade.setScreeningsOrganisatie(PlanningScreeningsOrganisatieIndex.get(blokkadeDto.screeningsOrganisatieId));
			break;
		case SCREENINGS_EENHEID:
			blokkade.setScreeningsEenheid(PlanningScreeningsEenheidIndex.get(blokkadeDto.screeningsEenheidId));
			break;
		case STANDPLAATS:
			blokkade.setStandplaats(PlanningStandplaatsIndex.get(blokkadeDto.standplaatsId));
			break;
		}

		return blokkade;
	}

	public static PlanningCapaciteitBlokDto from(PlanningBlok blok)
	{
		PlanningCapaciteitBlokDto dto = new PlanningCapaciteitBlokDto();
		dto.screeningsEenheidId = blok.getScreeningsEenheid().getId();
		dto.aantalOnderzoeken = blok.getAantalOnderzoeken();
		dto.blokType = blok.getCapaciteitBlokType();
		dto.vanaf = blok.getDateVanaf();
		dto.tot = blok.getDateTot();
		dto.opmerkingen = blok.getOpmerkingen();
		dto.conceptId = blok.getConceptId();
		dto.minderValideAfspraakMogelijk = blok.isMinderValideAfspraakMogelijk();
		return dto;
	}

	public static PlanningStandplaatsPeriodeDto from(PlanningStandplaatsPeriode standplaatsPeriode)
	{
		PlanningStandplaatsRonde standplaatsRonde = standplaatsPeriode.getStandplaatsRonde();

		PlanningStandplaatsPeriodeDto standplaatsPeriodeDto = new PlanningStandplaatsPeriodeDto();
		standplaatsPeriodeDto.afspraakDrempel = standplaatsRonde.getAfspraakDrempel();
		standplaatsPeriodeDto.screeningsEenheidVolgNr = standplaatsPeriode.getScreeningsEenheidVolgNr();
		standplaatsPeriodeDto.standplaatsId = standplaatsRonde.getStandplaats().getId();
		standplaatsPeriodeDto.vanaf = standplaatsPeriode.getVanaf();
		standplaatsPeriodeDto.totEnMet = standplaatsPeriode.getTotEnMet();
		standplaatsPeriodeDto.blokkadeIds = standplaatsPeriode.getBlokkadeNavigableSet().stream().map(planningBlokkade -> planningBlokkade.getId()).collect(Collectors.toList());
		standplaatsPeriodeDto.conceptId = standplaatsPeriode.getConceptId();
		standplaatsPeriodeDto.initieelIntervalMaanden = intervalToMonth(standplaatsRonde.getIntieelInterval());
		standplaatsPeriodeDto.intervalMaanden = intervalToMonth(standplaatsRonde.getInterval());
		standplaatsPeriodeDto.prognose = standplaatsPeriode.getPrognose();
		standplaatsPeriodeDto.id = standplaatsPeriode.getId();
		standplaatsPeriodeDto.standplaatsRondeVolgNr = standplaatsPeriode.getStandplaatsRondeVolgNr();
		standplaatsPeriodeDto.gesplitst = standplaatsPeriode.gesplitst();
		standplaatsPeriodeDto.achtervangStandplaatsId = standplaatsRonde.getAchtervangStandplaats() != null
			? standplaatsRonde.getAchtervangStandplaats().getId()
			: null;
		standplaatsPeriodeDto.minderValideUitwijkStandplaatsId = standplaatsRonde.getMinderValideUitwijkStandplaats() != null
			? standplaatsRonde.getMinderValideUitwijkStandplaats().getId()
			: null;
		standplaatsPeriodeDto.minderValideUitnodigenVanaf = standplaatsRonde.getMinderValideUitnodigenVanaf();

		standplaatsPeriodeDto.afspraakcapaciteitBeschikbaarVoorIds = standplaatsRonde.getAfspraakcapaciteitBeschikbaarVoor().stream()
			.map(PlanningScreeningsOrganisatie::getId)
			.collect(Collectors.toList());

		PlanningMeldingenDto meldingenDto = new PlanningMeldingenDto();
		for (PlanningMelding melding : standplaatsRonde.getMeldingList())
		{
			PlanningMeldingenDto.PlanningMeldingDto meldingDto = new PlanningMeldingenDto.PlanningMeldingDto();
			meldingDto.tekst = melding.getTekst();
			meldingDto.niveau = melding.getNiveau();
			meldingenDto.niveau = standplaatsRonde.getNiveau();
			meldingenDto.meldingen.add(meldingDto);
		}
		standplaatsPeriodeDto.meldingenDto = meldingenDto;

		return standplaatsPeriodeDto;
	}

	public static PlanningScreeningsEenheidMetaDataDto from(PlanningScreeningsEenheid screeningsEenheid)
	{
		PlanningScreeningsEenheidMetaDataDto metaDataDto = new PlanningScreeningsEenheidMetaDataDto();
		metaDataDto.niveau = screeningsEenheid.getNiveau();
		metaDataDto.initieelIntervalMaanden = intervalToMonth(screeningsEenheid.getInitieelInterval());
		metaDataDto.intervalMaanden = intervalToMonth(screeningsEenheid.getInterval());
		metaDataDto.herhalingsWeek = screeningsEenheid.getHerhalingsWeek() != null ? screeningsEenheid.getHerhalingsWeek().getDatum() : null;
		return metaDataDto;
	}

	private static BigDecimal intervalToMonth(BigDecimal intervalInDays)
	{
		return intervalInDays != null ? intervalInDays.divide(DAGEN_PER_MAAND, 5, BigDecimal.ROUND_HALF_UP) : null;
	}

}
