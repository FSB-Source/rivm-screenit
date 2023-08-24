package nl.rivm.screenit.model.colon;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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
import javax.persistence.FetchType;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

import nl.rivm.screenit.model.InstellingGebruiker;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.annotations.Cascade;
import org.hibernate.annotations.CascadeType;

@Entity
@Table(schema = "algemeen")
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
public class AntedateerRange extends AbstractHibernateObject
{

	private static final long serialVersionUID = 1L;

	@Column(nullable = false)
	@Temporal(TemporalType.TIMESTAMP)
	private Date vanaf;

	@Column(nullable = true)
	@Temporal(TemporalType.TIMESTAMP)
	private Date tot;

	@Column(nullable = false)
	@Temporal(TemporalType.TIMESTAMP)
	private Date vervangendeDate;

	@ManyToOne(fetch = FetchType.LAZY, optional = false)
	private InstellingGebruiker gestartDoor;

	@ManyToOne(fetch = FetchType.LAZY, optional = true)
	private InstellingGebruiker gestoptDoor;

	@ManyToOne(fetch = FetchType.LAZY, optional = false)
	@Cascade(CascadeType.SAVE_UPDATE)
	private IFobtLaboratorium ifobtLab;

	public Date getVanaf()
	{
		return vanaf;
	}

	public void setVanaf(Date vanaf)
	{
		this.vanaf = vanaf;
	}

	public Date getTot()
	{
		return tot;
	}

	public void setTot(Date tot)
	{
		this.tot = tot;
	}

	public Date getVervangendeDate()
	{
		return vervangendeDate;
	}

	public void setVervangendeDate(Date vervangendeDate)
	{
		this.vervangendeDate = vervangendeDate;
	}

	public IFobtLaboratorium getIfobtLab()
	{
		return ifobtLab;
	}

	public void setIfobtLab(IFobtLaboratorium ifobtLab)
	{
		this.ifobtLab = ifobtLab;
	}

	public InstellingGebruiker getGestartDoor()
	{
		return gestartDoor;
	}

	public void setGestartDoor(InstellingGebruiker gestartDoor)
	{
		this.gestartDoor = gestartDoor;
	}

	public InstellingGebruiker getGestoptDoor()
	{
		return gestoptDoor;
	}

	public void setGestoptDoor(InstellingGebruiker gestoptDoor)
	{
		this.gestoptDoor = gestoptDoor;
	}
}
