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
import javax.persistence.Index;
import javax.persistence.Inheritance;
import javax.persistence.InheritanceType;
import javax.persistence.Table;

import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.annotations.Proxy;

@Entity
@Table(schema = "algemeen", indexes = @Index(name = "functie_actiefIndex", columnList = "actief") )
@Proxy(lazy = true)
@Inheritance(strategy = InheritanceType.SINGLE_TABLE)
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "organisatie.cache")
public class Functie extends AbstractHibernateObject implements INaam
{

	public static final String PATHOLOOG = "Patholoog";

	public static final String ENDOSCOPIST = "Endoscopist";

	private static final long serialVersionUID = 1L;

	private String functie;

	@Column(nullable = false)
	private Boolean actief;

	public Boolean getActief()
	{
		return this.actief;
	}

	public void setActief(Boolean actief)
	{
		this.actief = actief;
	}

	public String getFunctie()
	{
		return this.functie;
	}

	public void setFunctie(String functie)
	{
		this.functie = functie;
	}

	@Override
	public String getNaam()
	{
		return this.functie;
	}
}
