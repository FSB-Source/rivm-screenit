
package nl.rivm.screenit.model;

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

import nl.rivm.screenit.model.helper.HibernateMagicNumber;

import org.hibernate.envers.Audited;

@Entity
@Audited
public class ZorgInstelling extends Instelling
{

	private static final long serialVersionUID = 1L;

	@Column(nullable = true, length = HibernateMagicNumber.L255)
	private String fqdn;

	public String getFqdn()
	{
		return fqdn;
	}

	public void setFqdn(String fqdn)
	{
		this.fqdn = fqdn;
	}
}
