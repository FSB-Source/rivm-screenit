
package nl.rivm.screenit.model.colon;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;

@Entity
@Table(schema = "colon")
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
public class SKMLExternSchema extends AbstractHibernateObject
{

	private static final long serialVersionUID = 1L;

	@Column(nullable = false)
	private Integer jaar;

	@Column(nullable = false)
	private Integer ronde;

	@Column(nullable = false)
	private String letter;

	@Temporal(TemporalType.TIMESTAMP)
	@Column(nullable = false)
	private Date deadline;

	@Column(nullable = false)
	private Boolean actief;

	public Integer getJaar()
	{
		return jaar;
	}

	public void setJaar(Integer jaar)
	{
		this.jaar = jaar;
	}

	public Integer getRonde()
	{
		return ronde;
	}

	public void setRonde(Integer ronde)
	{
		this.ronde = ronde;
	}

	public String getLetter()
	{
		return letter;
	}

	public void setLetter(String letter)
	{
		this.letter = letter;
	}

	public Date getDeadline()
	{
		return deadline;
	}

	public void setDeadline(Date deadline)
	{
		this.deadline = deadline;
	}

	public Boolean getActief()
	{
		return actief;
	}

	public void setActief(Boolean actief)
	{
		this.actief = actief;
	}
}
