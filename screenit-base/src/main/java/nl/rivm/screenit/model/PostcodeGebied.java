
package nl.rivm.screenit.model;

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

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;
import javax.persistence.Transient;

import nl.rivm.screenit.model.helper.HibernateMagicNumber;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.envers.Audited;

@Entity
@Table(schema = "colon")
@Audited
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
public class PostcodeGebied extends AbstractHibernateObject
{

	private static final long serialVersionUID = 1L;

	@Column(length = HibernateMagicNumber.L6)
	private String vanPostcode;

	@Column(length = HibernateMagicNumber.L6)
	private String totPostcode;

	public String getVanPostcode()
	{
		return vanPostcode;
	}

	public void setVanPostcode(String vanPostcode)
	{
		if (vanPostcode != null)
		{
			this.vanPostcode = vanPostcode.toUpperCase();
		}
		else
		{
			this.vanPostcode = null;
		}
	}

	public String getTotPostcode()
	{
		return totPostcode;
	}

	public void setTotPostcode(String totPostcode)
	{
		if (totPostcode != null)
		{
			this.totPostcode = totPostcode.toUpperCase();
		}
		else
		{
			this.totPostcode = null;
		}
	}

	@Transient
	public boolean bevatPostcode(String postcode)
	{
		return vanPostcode.compareToIgnoreCase(postcode) <= 0 && totPostcode.compareToIgnoreCase(postcode) >= 0;
	}

	@Transient
	public boolean overlaps(PostcodeGebied postcodeGebied)
	{
		return this.bevatPostcode(postcodeGebied.getVanPostcode()) || this.bevatPostcode(postcodeGebied.getTotPostcode());
	}
}
