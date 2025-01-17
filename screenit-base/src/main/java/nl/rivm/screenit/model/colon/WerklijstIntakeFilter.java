
package nl.rivm.screenit.model.colon;

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
import java.util.Date;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.colon.enums.ColonAfspraakStatus;

@Getter
@Setter
public class WerklijstIntakeFilter implements Serializable
{

	private ColonAfspraakStatus status;

	private Date vanaf;

	private Date totEnMet;

	private ConclusieTypeFilter conclusieTypeFilter;

	private String bsn;

	private Date geboortedatum;

	private Boolean eersteKeerZoeken = true;

	private Integer maxLeeftijd;

	private Integer interval;

}
