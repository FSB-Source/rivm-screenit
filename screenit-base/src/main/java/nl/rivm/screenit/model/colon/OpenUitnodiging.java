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

import java.util.Date;

import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.ManyToOne;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

import nl.rivm.screenit.model.enums.OpenUitnodigingUitslag;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.envers.Audited;

@Entity
@Table(schema = "colon")
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "organisatie.cache")
@Audited
public class OpenUitnodiging extends AbstractHibernateObject
{

	private static final long serialVersionUID = 1L;

	@OneToOne(optional = false)
	private ColonBrief uitnodigingsBrief;

	@Enumerated(EnumType.STRING)
	private OpenUitnodigingUitslag uitslag;

	@Temporal(TemporalType.TIMESTAMP)
	private Date datum;

	@OneToOne(optional = false)
	private ColonIntakeAfspraak oudeAfspraak;

	@OneToOne(optional = true)
	private ColonIntakeAfspraak afspraak;

	@OneToOne(optional = true)
	private ColonAfmelding afmelding;

	@ManyToOne(optional = false)
	private ColonScreeningRonde ronde;

	public ColonBrief getUitnodigingsBrief()
	{
		return uitnodigingsBrief;
	}

	public void setUitnodigingsBrief(ColonBrief uitnodigingsBrief)
	{
		this.uitnodigingsBrief = uitnodigingsBrief;
	}

	public OpenUitnodigingUitslag getUitslag()
	{
		return uitslag;
	}

	public void setUitslag(OpenUitnodigingUitslag uitslag)
	{
		this.uitslag = uitslag;
	}

	public ColonIntakeAfspraak getAfspraak()
	{
		return afspraak;
	}

	public void setAfspraak(ColonIntakeAfspraak afspraak)
	{
		this.afspraak = afspraak;
	}

	public ColonAfmelding getAfmelding()
	{
		return afmelding;
	}

	public void setAfmelding(ColonAfmelding afmelding)
	{
		this.afmelding = afmelding;
	}

	public Date getDatum()
	{
		return datum;
	}

	public void setDatum(Date datum)
	{
		this.datum = datum;
	}

	public ColonIntakeAfspraak getOudeAfspraak()
	{
		return oudeAfspraak;
	}

	public void setOudeAfspraak(ColonIntakeAfspraak oudeAfspraak)
	{
		this.oudeAfspraak = oudeAfspraak;
	}

	public ColonScreeningRonde getRonde()
	{
		return ronde;
	}

	public void setRonde(ColonScreeningRonde ronde)
	{
		this.ronde = ronde;
	}

}
