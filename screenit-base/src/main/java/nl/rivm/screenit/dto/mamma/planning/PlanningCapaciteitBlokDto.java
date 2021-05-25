package nl.rivm.screenit.dto.mamma.planning;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.util.Date;

import nl.rivm.screenit.model.mamma.enums.MammaCapaciteitBlokType;

public class PlanningCapaciteitBlokDto extends PlanningConceptEntiteitDto
{
	private static final long serialVersionUID = 1L;

	public Long id;

	public Date vanaf;

	public Date tot;

	public Integer aantalOnderzoeken;

	public String opmerkingen;

	public MammaCapaciteitBlokType blokType;

	public Long screeningsEenheidId;

	public boolean minderValideAfspraakMogelijk;

}
