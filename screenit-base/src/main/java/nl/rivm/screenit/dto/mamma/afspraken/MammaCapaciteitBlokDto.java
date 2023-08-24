package nl.rivm.screenit.dto.mamma.afspraken;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.io.Serializable;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.ArrayList;
import java.util.List;

import javax.persistence.Transient;

import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode;
import nl.rivm.screenit.model.mamma.enums.MammaCapaciteitBlokType;

public class MammaCapaciteitBlokDto implements Serializable
{
	public Long id;

	public LocalDateTime vanaf;

	public LocalTime tot;

	public BigDecimal vrijeCapaciteit;

	public BigDecimal beschikbareCapaciteit;

	public List<MammaAfspraakDto> afspraakDtos = new ArrayList<>();

	public MammaCapaciteitBlokType blokType;

	public Integer aantalOnderzoeken;

	@Transient
	public MammaStandplaatsPeriode standplaatsPeriode;

	public boolean minderValideAfspraakMogelijk;

}
