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
import java.math.BigInteger;
import java.util.Date;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import nl.rivm.screenit.model.colon.enums.ColonAfspraakslotStatus;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
public class ColonAfspraakslotListViewWrapper implements Serializable
{

	private Date startDatum;

	private Date eindDatum;

	private String kamer;

	private Boolean capaciteitMeeBepaald;

	private ColonAfspraakslotStatus status;

	private Long afspraakslotId;

	private Long kamerId;

	public void setAfspraakslotId(BigInteger afspraakslotId)
	{
		this.afspraakslotId = afspraakslotId.longValue();
	}

	public void setKamerId(BigInteger kamerId)
	{
		this.kamerId = kamerId.longValue();
	}

}
