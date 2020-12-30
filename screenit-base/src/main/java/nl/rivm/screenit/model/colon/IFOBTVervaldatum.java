
package nl.rivm.screenit.model.colon;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.Table;

import nl.rivm.screenit.model.AbstractHoudbaarheid;

@Entity
@Table(schema = "colon")
public class IFOBTVervaldatum extends AbstractHoudbaarheid
{

	private static final long serialVersionUID = 1L;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private IFOBTType type;

	public IFOBTType getType()
	{
		return type;
	}

	public void setType(IFOBTType type)
	{
		this.type = type;
	}
}
