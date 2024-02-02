package nl.rivm.screenit.model.vragenlijsten;

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

import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

import nl.rivm.screenit.model.IActief;
import nl.rivm.screenit.model.INaam;
import nl.rivm.screenit.model.SingleTableHibernateObject;
import nl.rivm.screenit.model.formulieren.ScreenitFormulierInstantie;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.annotations.Cascade;
import org.hibernate.annotations.CascadeType;

@Entity
@Table(schema = "algemeen")
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
public abstract class Vragenlijst extends SingleTableHibernateObject implements IActief, INaam
{

	private static final long serialVersionUID = 1L;

	private Boolean actief;

	private String naam;

	@Temporal(TemporalType.TIMESTAMP)
	private Date laatstGewijzigd;

	@ManyToOne(fetch = FetchType.EAGER, cascade = { javax.persistence.CascadeType.PERSIST, javax.persistence.CascadeType.MERGE })
	@Cascade(CascadeType.SAVE_UPDATE)
	private ScreenitFormulierInstantie formulierInstantie;

	@Override
	public Boolean getActief()
	{
		return actief;
	}

	@Override
	public void setActief(Boolean actief)
	{
		this.actief = actief;
	}

	@Override
	public String getNaam()
	{
		return naam;
	}

	public void setNaam(String naam)
	{
		this.naam = naam;
	}

	public Date getLaatstGewijzigd()
	{
		return laatstGewijzigd;
	}

	public void setLaatstGewijzigd(Date laatstGewijzigd)
	{
		this.laatstGewijzigd = laatstGewijzigd;
	}

	public ScreenitFormulierInstantie getFormulierInstantie()
	{
		return formulierInstantie;
	}

	public void setFormulierInstantie(ScreenitFormulierInstantie formulierInstantie)
	{
		this.formulierInstantie = formulierInstantie;
	}
}
