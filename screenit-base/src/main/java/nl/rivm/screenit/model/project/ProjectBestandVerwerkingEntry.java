package nl.rivm.screenit.model.project;

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

import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.annotations.Cascade;
import org.hibernate.annotations.CascadeType;

@Entity
@Table(schema = "algemeen")
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
public class ProjectBestandVerwerkingEntry extends AbstractHibernateObject
{

	private static final long serialVersionUID = 1L;

	@Cascade({ CascadeType.ALL })
	@ManyToOne(fetch = FetchType.EAGER, optional = false)
	private ProjectBestandVerwerking verwerking;

	private Integer regelNummer;

	private String melding;

	public ProjectBestandVerwerking getVerwerking()
	{
		return verwerking;
	}

	public void setVerwerking(ProjectBestandVerwerking verwerking)
	{
		this.verwerking = verwerking;
	}

	public Integer getRegelNummer()
	{
		return regelNummer;
	}

	public void setRegelNummer(Integer regelNummer)
	{
		this.regelNummer = regelNummer;
	}

	public String getMelding()
	{
		return melding;
	}

	public void setMelding(String melding)
	{
		this.melding = melding;
	}

}
