package nl.rivm.screenit.service.mamma.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.math.BigDecimal;
import java.time.Duration;
import java.time.LocalDate;
import java.time.LocalTime;
import java.time.temporal.ChronoUnit;

import nl.rivm.screenit.dto.mamma.afspraken.MammaCapaciteitBlokDto;
import nl.rivm.screenit.dto.mamma.afspraken.MammaScreeningsEenheidDto;
import nl.rivm.screenit.util.TimeRange;
import nl.rivm.screenit.util.mamma.MammaPlanningUtil;

import static nl.rivm.screenit.Constants.BK_TIJDVAK_MIN;
import static nl.rivm.screenit.Constants.BK_TIJDVAK_SEC;

public class MammaKandidaatAfspraak extends MammaRationaal
{
	private final MammaCapaciteitBlokDto capaciteitBlokDto;

	private final LocalTime minimaleAfspraakVanaf;

	private final LocalTime maximaleAfspraakTot;

	private final LocalDate datum;

	private final LocalTime vanaf;

	private LocalTime tot;

	private BigDecimal duur;

	private final BigDecimal benodigdeCapaciteit;

	private boolean valideAfspraak;

	private boolean maaktParentAfspraakOngeldig;

	private boolean minderValide;

	private MammaScreeningsEenheidDto screeningsEenheid;

	MammaKandidaatAfspraak(MammaCapaciteitBlokDto capaciteitBlok, LocalTime minimaleAfspraakVanaf, LocalTime maximaleAfspraakTot, LocalDate datum, LocalTime vanaf, LocalTime tot,
		BigDecimal benodigdeCapaciteit,
		MammaScreeningsEenheidDto screeningsEenheid, boolean minderValide)
	{
		this.capaciteitBlokDto = capaciteitBlok;
		this.minimaleAfspraakVanaf = minimaleAfspraakVanaf;
		this.maximaleAfspraakTot = maximaleAfspraakTot;
		this.datum = datum;
		this.vanaf = vanaf;
		this.tot = tot.isAfter(this.maximaleAfspraakTot) ? this.maximaleAfspraakTot : tot;
		this.minderValide = minderValide;
		bepaalDuur();
		this.benodigdeCapaciteit = benodigdeCapaciteit;
		this.screeningsEenheid = screeningsEenheid;

		valideAfspraak = !minderValide || voldoetAanMinderValideEisen(vanaf, tot, screeningsEenheid);
	}

	private void bepaalDuur()
	{
		duur = BigDecimal.valueOf(Duration.between(vanaf, tot).getSeconds());
	}

	MammaKandidaatAfspraak getKandidaatAfspraak(BigDecimal benodigdeCapaciteit, BigDecimal factor, boolean minderValide)
	{
		LocalTime vanaf = null;
		boolean isValideAfspraak = true;
		boolean maaktParentAfspraakOngeldig = false;

		if (!duur.equals(BK_TIJDVAK_SEC))
		{
			BigDecimal somDuur = this.duur;
			BigDecimal somBenodigdeCapaciteit = this.benodigdeCapaciteit.add(benodigdeCapaciteit);
			BigDecimal vermenigvuldigingsFactor = this.benodigdeCapaciteit.divide(somBenodigdeCapaciteit, 5, BigDecimal.ROUND_HALF_UP);

			this.duur = somDuur.multiply(vermenigvuldigingsFactor);
			vanaf = this.vanaf.plusSeconds(this.duur.longValue());

			LocalTime capaciteitBlokCeiling = maximaleAfspraakTot.minusMinutes(MammaPlanningUtil.minimumTijdvak(factor));
			if (capaciteitBlokCeiling.isBefore(minimaleAfspraakVanaf))
			{
				capaciteitBlokCeiling = minimaleAfspraakVanaf;
			}
			if (capaciteitBlokCeiling.isBefore(vanaf))
			{
				vanaf = capaciteitBlokCeiling;
				if (this.minderValide && this.valideAfspraak)
				{
					isValideAfspraak = voldoetAanMinderValideEisen(this.vanaf, vanaf, screeningsEenheid);
					if (!isValideAfspraak)
					{

						maaktParentAfspraakOngeldig = true;
					}
				}
				if (minderValide)
				{
					isValideAfspraak &= voldoetAanMinderValideEisen(vanaf, this.tot, screeningsEenheid) && vanaf.isAfter(this.vanaf);
				}
				isValideAfspraak = !this.maaktParentAfspraakOngeldig && isValideAfspraak;
			}
			else
			{
				int minute = vanaf.truncatedTo(ChronoUnit.MINUTES).getMinute();
				LocalTime floor = vanaf.withMinute(minute - minute % BK_TIJDVAK_MIN).withSecond(0);
				LocalTime ceiling = floor.plusMinutes(BK_TIJDVAK_MIN);

				boolean floorValide = true;
				boolean ceilingValide = true;
				if (this.minderValide && this.valideAfspraak)
				{

					floorValide = voldoetAanMinderValideEisen(this.vanaf, floor, screeningsEenheid);
					ceilingValide = voldoetAanMinderValideEisen(this.vanaf, ceiling, screeningsEenheid);
					if (!floorValide && !ceilingValide)
					{

						maaktParentAfspraakOngeldig = true;
					}
				}
				if (minderValide)
				{

					floorValide &= voldoetAanMinderValideEisen(floor, this.tot, screeningsEenheid);
					ceilingValide &= voldoetAanMinderValideEisen(ceiling, this.tot, screeningsEenheid);
				}

				if (floorValide && !ceilingValide)
				{
					vanaf = floor;
				}
				else if (!floorValide && ceilingValide)
				{
					vanaf = ceiling;
				}
				else
				{

					vanaf = Duration.between(floor, vanaf).compareTo(Duration.between(vanaf, ceiling)) <= 0 ? floor : ceiling;
				}

				isValideAfspraak = !this.maaktParentAfspraakOngeldig && (floorValide || ceilingValide) && maximaleAfspraakTot.isAfter(vanaf);
			}
		}
		else
		{

			vanaf = this.vanaf;
			if (minderValide)
			{

				isValideAfspraak = false;
			}
			else
			{
				isValideAfspraak = !this.maaktParentAfspraakOngeldig;
			}
		}
		MammaKandidaatAfspraak kandidaatAfspraak = new MammaKandidaatAfspraak(capaciteitBlokDto, minimaleAfspraakVanaf, maximaleAfspraakTot, datum, vanaf, this.tot,
			benodigdeCapaciteit, screeningsEenheid, minderValide);
		kandidaatAfspraak.valideAfspraak = isValideAfspraak;
		kandidaatAfspraak.maaktParentAfspraakOngeldig = maaktParentAfspraakOngeldig;

		this.tot = vanaf;
		bepaalDuur();

		return kandidaatAfspraak;
	}

	@Override
	BigDecimal getDeeltal()
	{
		return benodigdeCapaciteit;
	}

	@Override
	BigDecimal getDeler()
	{
		return duur;
	}

	public MammaCapaciteitBlokDto getCapaciteitBlokDto()
	{
		return capaciteitBlokDto;
	}

	public LocalDate getDatum()
	{
		return datum;
	}

	public LocalTime getVanaf()
	{
		return vanaf;
	}

	public BigDecimal getBenodigdeCapaciteit()
	{
		return benodigdeCapaciteit;
	}

	public boolean isValideAfspraak()
	{
		return valideAfspraak;
	}

	public void setValideAfspraak(boolean valideAfspraak)
	{
		this.valideAfspraak = valideAfspraak;
	}

	public boolean isMinderValide()
	{
		return minderValide;
	}

	private static boolean voldoetAanMinderValideEisen(LocalTime afspraakVanaf, LocalTime afspraakTot, MammaScreeningsEenheidDto screeningsEenheid)
	{
		TimeRange minderValidePeriode1 = TimeRange.of(screeningsEenheid.minderValidePeriode1Vanaf, screeningsEenheid.minderValidePeriode1TotEnMet);
		TimeRange minderValidePeriode2 = TimeRange.of(screeningsEenheid.minderValidePeriode2Vanaf, screeningsEenheid.minderValidePeriode2TotEnMet);

		boolean valideDuur = screeningsEenheid.meerdereMammografen
			|| Duration.between(afspraakVanaf, afspraakTot).toMinutes() >= screeningsEenheid.duurMinderValideAfspraak.getMinuten();
		LocalTime minimaleTotEnMet = afspraakVanaf.plus(screeningsEenheid.duurMinderValideAfspraak.getMinuten(), ChronoUnit.MINUTES);

		TimeRange afspraakTimeRange = TimeRange.of(afspraakVanaf, minimaleTotEnMet);
		return valideDuur &&
			(afspraakTimeRange.valtVolledigBinnen(minderValidePeriode1) || afspraakTimeRange.valtVolledigBinnen(minderValidePeriode2));
	}
}
