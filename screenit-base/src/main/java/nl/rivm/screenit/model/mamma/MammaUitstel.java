package nl.rivm.screenit.model.mamma;

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
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.ManyToOne;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;
import javax.persistence.UniqueConstraint;

import nl.rivm.screenit.model.mamma.enums.MammaUitstelGeannuleerdReden;
import nl.rivm.screenit.model.mamma.enums.MammaUitstelReden;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.envers.Audited;

@Entity
@Table(
	schema = "mamma",
	name = "uitstel",
	uniqueConstraints = {
		@UniqueConstraint(columnNames = "uitnodiging") })
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "mamma.cache")
@Audited
public class MammaUitstel extends AbstractHibernateObject
{

	private static final long serialVersionUID = 1L;

	@ManyToOne(optional = false, fetch = FetchType.LAZY)
	private MammaScreeningRonde screeningRonde;

	@OneToOne(optional = true, fetch = FetchType.LAZY)
	private MammaUitnodiging uitnodiging;

	@Temporal(TemporalType.TIMESTAMP)
	@Column(nullable = false)
	private Date gemaaktOp;

	@Temporal(TemporalType.TIMESTAMP)
	@Column(nullable = true)
	private Date geannuleerdOp;

	@Column(nullable = true)
	@Enumerated(EnumType.STRING)
	private MammaUitstelGeannuleerdReden geannuleerdReden;

	@Temporal(TemporalType.DATE)
	@Column(nullable = false)
	private Date streefDatum;

	@ManyToOne(optional = false, fetch = FetchType.LAZY)
	private MammaStandplaats standplaats;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private MammaUitstelReden uitstelReden;

	public MammaScreeningRonde getScreeningRonde()
	{
		return screeningRonde;
	}

	public void setScreeningRonde(MammaScreeningRonde screeningRonde)
	{
		this.screeningRonde = screeningRonde;
	}

	public Date getGemaaktOp()
	{
		return gemaaktOp;
	}

	public void setGemaaktOp(Date gemaaktOp)
	{
		this.gemaaktOp = gemaaktOp;
	}

	public Date getStreefDatum()
	{
		return streefDatum;
	}

	public void setStreefDatum(Date streefDatum)
	{
		this.streefDatum = streefDatum;
	}

	public Date getGeannuleerdOp()
	{
		return geannuleerdOp;
	}

	public void setGeannuleerdOp(Date geannuleerdOp)
	{
		this.geannuleerdOp = geannuleerdOp;
	}

	public MammaStandplaats getStandplaats()
	{
		return standplaats;
	}

	public void setStandplaats(MammaStandplaats standplaats)
	{
		this.standplaats = standplaats;
	}

	public MammaUitnodiging getUitnodiging()
	{
		return uitnodiging;
	}

	public void setUitnodiging(MammaUitnodiging uitnodiging)
	{
		this.uitnodiging = uitnodiging;
	}

	public MammaUitstelGeannuleerdReden getGeannuleerdReden()
	{
		return geannuleerdReden;
	}

	public void setGeannuleerdReden(MammaUitstelGeannuleerdReden geannuleerdReden)
	{
		this.geannuleerdReden = geannuleerdReden;
	}

	public MammaUitstelReden getUitstelReden()
	{
		return uitstelReden;
	}

	public void setUitstelReden(MammaUitstelReden uitstelReden)
	{
		this.uitstelReden = uitstelReden;
	}
}
