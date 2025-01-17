package nl.rivm.screenit.model;

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

import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.Table;

import nl.rivm.screenit.model.enums.HuisartsBerichtType;
import nl.rivm.screenit.model.helper.HibernateMagicNumber;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.envers.Audited;

@Entity
@Audited
@Table(schema = "algemeen")
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
public class HuisartsBerichtTemplate extends AbstractHibernateObject
{

	private static final long serialVersionUID = 1L;

	@Enumerated(EnumType.STRING)
	private HuisartsBerichtType berichtType;

	private Date aangepast;

	@Column(length = HibernateMagicNumber.L5000)
	private String berichtInhoud;

	public HuisartsBerichtType getBerichtType()
	{
		return berichtType;
	}

	public void setBerichtType(HuisartsBerichtType berichtType)
	{
		this.berichtType = berichtType;
	}

	public String getBerichtInhoud()
	{
		return berichtInhoud;
	}

	public void setBerichtInhoud(String berichtInhoud)
	{
		this.berichtInhoud = berichtInhoud;
	}

	public Date getAangepast()
	{
		return aangepast;
	}

	public void setAangepast(Date aangepast)
	{
		this.aangepast = aangepast;
	}
}
