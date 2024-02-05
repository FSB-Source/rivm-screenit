package nl.rivm.screenit.service.mamma.afspraakzoeken;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.Duration;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.temporal.ChronoUnit;

import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.dto.mamma.afspraken.MammaCapaciteitBlokDto;
import nl.rivm.screenit.dto.mamma.afspraken.MammaScreeningsEenheidDto;
import nl.rivm.screenit.util.TimeRange;
import nl.rivm.screenit.util.mamma.MammaPlanningUtil;

import static nl.rivm.screenit.Constants.BK_TIJDVAK_MIN;
import static nl.rivm.screenit.Constants.BK_TIJDVAK_SEC;

@Slf4j
public class MammaKandidaatAfspraak extends MammaRationaal
{
	@Getter
	private final MammaCapaciteitBlokDto capaciteitBlokDto;

	private final MammaScreeningsEenheidDto screeningsEenheidDto;

	private final LocalTime minimaleAfspraakVanaf;

	private final LocalTime maximaleAfspraakTot;

	@Getter
	private final BigDecimal benodigdeCapaciteit;

	@Getter
	private final boolean minderValide;

	private final boolean dubbeleTijd;

	@Getter
	private final LocalDate datum;

	@Getter
	private final LocalTime vanaf;

	private LocalTime tot;

	@Getter
	@Setter
	private boolean geldigeAfspraak;

	private boolean afgesplitsteKandidatenZijnOngeldig;

	public MammaKandidaatAfspraak(MammaCapaciteitBlokDto capaciteitBlok, TimeRange mogelijkeAfspraakPeriode, TimeRange afspraakPeriode, BigDecimal benodigdeCapaciteit,
		MammaScreeningsEenheidDto screeningsEenheid, boolean minderValide, boolean dubbeleTijd)
	{
		capaciteitBlokDto = capaciteitBlok;
		minimaleAfspraakVanaf = mogelijkeAfspraakPeriode.getVanaf();
		maximaleAfspraakTot = mogelijkeAfspraakPeriode.getTot();
		datum = capaciteitBlok.vanaf.toLocalDate();
		vanaf = afspraakPeriode.getVanaf();
		tot = afspraakPeriode.getTot().isAfter(mogelijkeAfspraakPeriode.getTot()) ? mogelijkeAfspraakPeriode.getTot() : afspraakPeriode.getTot();
		this.minderValide = minderValide;
		this.dubbeleTijd = dubbeleTijd;
		this.benodigdeCapaciteit = benodigdeCapaciteit;
		screeningsEenheidDto = screeningsEenheid;
		geldigeAfspraak = geldigeDuurEnPeriode(vanaf, tot);
	}

	private boolean geldigeDuurEnPeriode(LocalTime vanaf, LocalTime tot)
	{
		return tot.isAfter(vanaf) 
			&& (!minderValide || geldigeDuurEnPeriodeVoorMindervalideAfspraak(vanaf, tot))
			&& (!dubbeleTijd || geldigeDuurVoorDubbeleTijdAfspraak(vanaf, tot));
	}

	private BigDecimal getDuurInSeconden()
	{
		return BigDecimal.valueOf(Duration.between(vanaf, tot).getSeconds());
	}

	public MammaKandidaatAfspraak splitsNieuweKandidaatAfspraak(BigDecimal nieuweBenodigdeCapaciteit, BigDecimal nieuweFactor, boolean nieuweKandidaatIsMinderValide,
		boolean nieuweKandidaatIsDubbeleTijd)
	{
		MammaKandidaatAfspraak nieuweKandidaat;

		if (!getDuurInSeconden().equals(BK_TIJDVAK_SEC))
		{
			var nieuweOnafgerondeVanaf = nieuwVanafNaSplitsingNaarVerhoudingBenodigdeCapaciteit(nieuweBenodigdeCapaciteit);
			var maximaleBegintijdInBlok = maximaleBegintijdInBlok(nieuweFactor);
			if (maximaleBegintijdInBlok.isBefore(nieuweOnafgerondeVanaf))
			{
				nieuweKandidaat = maakNieuweKandidaat(maximaleBegintijdInBlok, nieuweBenodigdeCapaciteit, nieuweKandidaatIsMinderValide, nieuweKandidaatIsDubbeleTijd);
			}
			else
			{
				nieuweKandidaat = maakNieuweKandidaatInPassendTijdvak(nieuweOnafgerondeVanaf, nieuweBenodigdeCapaciteit, nieuweKandidaatIsMinderValide,
					nieuweKandidaatIsDubbeleTijd);
			}
		}
		else
		{
			nieuweKandidaat = maakNieuweKandidaatInKleinstMogelijkeTijdvak(nieuweBenodigdeCapaciteit, nieuweKandidaatIsMinderValide, nieuweKandidaatIsDubbeleTijd);
		}

		LOG.debug(
			"Kandidaatafspraak {}-{} MV={}, DT={}, geldig={}, afgesplitsteOngeldig={} gesplitst naar {}-{}, MV={}, DT={}, geldig={}, afgesplitsteOngeldig={}, capaciteitBlokId={}",
			vanaf, tot, minderValide, dubbeleTijd, geldigeAfspraak, afgesplitsteKandidatenZijnOngeldig, nieuweKandidaat.vanaf, nieuweKandidaat.tot, nieuweKandidaat.minderValide,
			nieuweKandidaat.dubbeleTijd, nieuweKandidaat.geldigeAfspraak, nieuweKandidaat.afgesplitsteKandidatenZijnOngeldig, capaciteitBlokDto.id);

		tot = nieuweKandidaat.vanaf;
		return nieuweKandidaat;
	}

	private LocalTime nieuwVanafNaSplitsingNaarVerhoudingBenodigdeCapaciteit(BigDecimal nieuweKandidaatBenodigdeCapaciteit)
	{
		var somBenodigdeCapaciteit = benodigdeCapaciteit.add(nieuweKandidaatBenodigdeCapaciteit);
		var vermenigvuldigingsFactor = benodigdeCapaciteit.divide(somBenodigdeCapaciteit, 5, RoundingMode.HALF_UP);
		var nieuweDuurInSeconden = getDuurInSeconden().multiply(vermenigvuldigingsFactor);
		return vanaf.plusSeconds(nieuweDuurInSeconden.longValue());
	}

	private LocalTime maximaleBegintijdInBlok(BigDecimal nieuweKandidaatFactor)
	{
		var maximaleBegintijdInBlok = maximaleAfspraakTot.minusMinutes(MammaPlanningUtil.minimumTijdvak(nieuweKandidaatFactor));
		if (maximaleBegintijdInBlok.isBefore(minimaleAfspraakVanaf))
		{
			maximaleBegintijdInBlok = minimaleAfspraakVanaf;
		}
		return maximaleBegintijdInBlok;
	}

	private MammaKandidaatAfspraak maakNieuweKandidaat(LocalTime nieuweVanaf, BigDecimal nieuweBenodigdeCapaciteit, boolean nieuweIsMinderValide, boolean nieuwIsDubbeleTijd)
	{
		var nieuweKandidaat = new MammaKandidaatAfspraak(capaciteitBlokDto, TimeRange.of(minimaleAfspraakVanaf, maximaleAfspraakTot),
			TimeRange.of(nieuweVanaf, tot), nieuweBenodigdeCapaciteit, screeningsEenheidDto, nieuweIsMinderValide, nieuwIsDubbeleTijd);
		controleerDatNieuweOptieVoorEindtijdBlokBegint(nieuweKandidaat);
		controleerDatNieuweMinderValideOfDubbeleTijdOptieNietGelijkValtMetBestaandeAfspraak(nieuweKandidaat);
		controleerDatBestaandeAfspraakGeldigBlijft(nieuweKandidaat);
		return nieuweKandidaat;
	}

	private void controleerDatNieuweOptieVoorEindtijdBlokBegint(MammaKandidaatAfspraak nieuweKandidaat)
	{
		nieuweKandidaat.geldigeAfspraak &= maximaleAfspraakTot.isAfter(nieuweKandidaat.vanaf);
	}

	private void controleerDatNieuweMinderValideOfDubbeleTijdOptieNietGelijkValtMetBestaandeAfspraak(MammaKandidaatAfspraak nieuweKandidaat)
	{
		if (screeningsEenheidDto.isEnkeleMammograaf() && (nieuweKandidaat.minderValide || nieuweKandidaat.dubbeleTijd))
		{
			nieuweKandidaat.geldigeAfspraak &= nieuweKandidaat.vanaf.isAfter(vanaf);
		}
	}

	private void controleerDatBestaandeAfspraakGeldigBlijft(MammaKandidaatAfspraak nieuweKandidaat)
	{
		nieuweKandidaat.afgesplitsteKandidatenZijnOngeldig = afgesplitsteKandidatenZijnOngeldig;
		nieuweKandidaat.geldigeAfspraak &= !afgesplitsteKandidatenZijnOngeldig;

		if (geldigeAfspraak && (minderValide || dubbeleTijd))
		{

			var nieuweKandidaatHoudtOudeGeldig = !screeningsEenheidDto.isEnkeleMammograaf() || geldigeDuurEnPeriode(vanaf, nieuweKandidaat.vanaf);
			nieuweKandidaat.geldigeAfspraak &= nieuweKandidaatHoudtOudeGeldig;

			nieuweKandidaat.afgesplitsteKandidatenZijnOngeldig |= !nieuweKandidaatHoudtOudeGeldig;
		}
	}

	private boolean geldigeDuurEnPeriodeVoorMindervalideAfspraak(LocalTime kandidaatVanaf, LocalTime kandidaatTot)
	{
		return geldigeMinderValideAfspraakDuur(kandidaatVanaf, kandidaatTot) && afspraakValtBinnenMindervalidePeriode(kandidaatVanaf);
	}

	private boolean geldigeMinderValideAfspraakDuur(LocalTime kandidaatVanaf, LocalTime kandidaatTot)
	{
		return !screeningsEenheidDto.isEnkeleMammograaf()
			|| Duration.between(kandidaatVanaf, kandidaatTot).toMinutes() >= screeningsEenheidDto.getDuurMinderValideAfspraak().getMinuten();
	}

	private boolean afspraakValtBinnenMindervalidePeriode(LocalTime kandidaatVanaf)
	{
		var minimaleTotEnMet = kandidaatVanaf.plusMinutes(screeningsEenheidDto.getDuurMinderValideAfspraak().getMinuten());
		var kandidaatPeriode = TimeRange.of(kandidaatVanaf, minimaleTotEnMet);

		return kandidaatPeriode.valtVolledigBinnen(screeningsEenheidDto.getMinderValidePeriode1())
			|| kandidaatPeriode.valtVolledigBinnen(screeningsEenheidDto.getMinderValidePeriode2());
	}

	private boolean geldigeDuurVoorDubbeleTijdAfspraak(LocalTime kandidaatVanaf, LocalTime kandidaatTot)
	{
		return !screeningsEenheidDto.isEnkeleMammograaf()
			|| Duration.between(kandidaatVanaf, kandidaatTot).toMinutes() >= 10;
	}

	private MammaKandidaatAfspraak maakNieuweKandidaatInPassendTijdvak(LocalTime nieuweOnafgerondeVanaf, BigDecimal nieuweBenodigdeCapaciteit, boolean nieuweIsMinderValide,
		boolean nieuweIsDubbeleTijd)
	{
		var kandidaatVroegeTijdvak = maakNieuweKandidaat(rondAfNaarBeginTijdvak(nieuweOnafgerondeVanaf), nieuweBenodigdeCapaciteit, nieuweIsMinderValide, nieuweIsDubbeleTijd);
		var kandidaatLateTijdvak = maakNieuweKandidaat(kandidaatVroegeTijdvak.vanaf.plusMinutes(BK_TIJDVAK_MIN), nieuweBenodigdeCapaciteit, nieuweIsMinderValide,
			nieuweIsDubbeleTijd);
		return kiesBesteKandidaat(nieuweOnafgerondeVanaf, kandidaatVroegeTijdvak, kandidaatLateTijdvak);
	}

	private static LocalTime rondAfNaarBeginTijdvak(LocalTime nieuweKandidaatVanaf)
	{
		var minuten = nieuweKandidaatVanaf.truncatedTo(ChronoUnit.MINUTES).getMinute();
		return nieuweKandidaatVanaf.withMinute(minuten - minuten % BK_TIJDVAK_MIN).withSecond(0);
	}

	private MammaKandidaatAfspraak kiesBesteKandidaat(LocalTime nieuweOnafgerondeVanaf, MammaKandidaatAfspraak kandidaatVroegeTijdvak, MammaKandidaatAfspraak kandidaatLateTijdvak)
	{
		if (kandidaatVroegeTijdvak.geldigeAfspraak && !kandidaatLateTijdvak.geldigeAfspraak)
		{
			return kandidaatVroegeTijdvak;
		}
		else if (!kandidaatVroegeTijdvak.geldigeAfspraak && kandidaatLateTijdvak.geldigeAfspraak)
		{
			return kandidaatLateTijdvak;
		}
		return kiesDichtstbijzijndeKandidaat(nieuweOnafgerondeVanaf, kandidaatVroegeTijdvak, kandidaatLateTijdvak);
	}

	private MammaKandidaatAfspraak kiesDichtstbijzijndeKandidaat(LocalTime onafgerondeVanaf, MammaKandidaatAfspraak kandidaatVroegeTijdvak,
		MammaKandidaatAfspraak kandidaatLateTijdvak)
	{
		return Duration.between(kandidaatVroegeTijdvak.vanaf, onafgerondeVanaf).compareTo(Duration.between(onafgerondeVanaf, kandidaatLateTijdvak.vanaf)) <= 0 ?
			kandidaatVroegeTijdvak : kandidaatLateTijdvak;
	}

	private MammaKandidaatAfspraak maakNieuweKandidaatInKleinstMogelijkeTijdvak(BigDecimal nieuweBenodigdeCapaciteit, boolean nieuweKandidaatIsMinderValide,
		boolean nieuweKandidaatIsDubbeleTijd)
	{
		var nieuweKandidaat = maakNieuweKandidaat(vanaf, nieuweBenodigdeCapaciteit, nieuweKandidaatIsMinderValide, nieuweKandidaatIsDubbeleTijd);

		nieuweKandidaat.geldigeAfspraak &= !nieuweKandidaatIsMinderValide && !nieuweKandidaatIsDubbeleTijd;
		return nieuweKandidaat;
	}

	public LocalDateTime getDatumTijd()
	{
		return getDatum().atTime(getVanaf());
	}

	@Override
	public BigDecimal getDeeltal()
	{
		return benodigdeCapaciteit;
	}

	@Override
	public BigDecimal getDeler()
	{
		return getDuurInSeconden();
	}
}
