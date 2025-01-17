package nl.rivm.screenit.dto.mamma.afspraken;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.io.Serializable;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.Objects;

public class MammaKandidaatAfspraakDto implements Serializable, Comparable<MammaKandidaatAfspraakDto>
{
	private final Long capaciteitBlokId;

	private final LocalDate datum;

	private final LocalTime tijd;

	private final Long standplaatsPeriodeId;

	private final Double afstand;

	public static final Double ONBEKENDE_AFSTAND = -1.0;

	public MammaKandidaatAfspraakDto(Long capaciteitBlokId, LocalDate datum, LocalTime tijd, Long standplaatsPeriodeId, Double afstand)
	{
		this.capaciteitBlokId = capaciteitBlokId;
		this.datum = datum;
		this.tijd = tijd;
		this.standplaatsPeriodeId = standplaatsPeriodeId;
		this.afstand = afstand;
	}

	public Long getCapaciteitBlokId()
	{
		return capaciteitBlokId;
	}

	public LocalDate getDatum()
	{
		return datum;
	}

	public LocalTime getTijd()
	{
		return tijd;
	}

	public Long getStandplaatsPeriodeId()
	{
		return standplaatsPeriodeId;
	}

	public Double getAfstand()
	{
		return afstand;
	}

	@Override
	public int compareTo(MammaKandidaatAfspraakDto kandidaatAfspraakDto)
	{
		int compareTo = this.afstand.compareTo(kandidaatAfspraakDto.afstand);
		if (compareTo == 0)
		{
			compareTo = this.datum.compareTo(kandidaatAfspraakDto.datum);
			if (compareTo == 0)
			{
				compareTo = this.tijd.compareTo(kandidaatAfspraakDto.tijd);
			}
		}
		return compareTo;
	}

	@Override
	public boolean equals(Object o)
	{
		if (this == o)
		{
			return true;
		}
		if (o == null || getClass() != o.getClass())
		{
			return false;
		}
		MammaKandidaatAfspraakDto that = (MammaKandidaatAfspraakDto) o;
		return Objects.equals(capaciteitBlokId, that.capaciteitBlokId) &&
			Objects.equals(datum, that.datum) &&
			Objects.equals(tijd, that.tijd) &&
			Objects.equals(standplaatsPeriodeId, that.standplaatsPeriodeId);
	}

	@Override
	public int hashCode()
	{
		return Objects.hash(capaciteitBlokId, datum, tijd, standplaatsPeriodeId);
	}

	public boolean isAfstandOnbekend()
	{
		return afstand.equals(ONBEKENDE_AFSTAND);
	}

	public LocalDateTime getDatumTijd()
	{
		return getDatum().atTime(getTijd());
	}
}
