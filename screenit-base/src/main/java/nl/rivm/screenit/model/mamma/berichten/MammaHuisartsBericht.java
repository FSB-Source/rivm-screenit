package nl.rivm.screenit.model.mamma.berichten;

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

import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.Index;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

import nl.rivm.screenit.model.EnovationHuisarts;
import nl.rivm.screenit.model.HuisartsBericht;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.enums.MammaHuisartsBerichtStatus;

import org.hibernate.envers.Audited;
import org.hibernate.envers.NotAudited;

@Entity
@Table(
	schema = "mamma",
	name = "huisarts_bericht",
	indexes = { @Index(name = "idx_MAMMA_HUISARTS_BERICHT_STATUS", columnList = "status") })
@Audited
public class MammaHuisartsBericht extends HuisartsBericht
{

	@ManyToOne(optional = false, fetch = FetchType.LAZY)
	@NotAudited
	private MammaBeoordeling beoordeling;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private MammaHuisartsBerichtStatus status;

	@Column(nullable = false)
	@Temporal(TemporalType.TIMESTAMP)
	private Date statusDatum;

	@Temporal(TemporalType.TIMESTAMP)
	private Date verzendDatum;

	@ManyToOne(fetch = FetchType.LAZY, optional = false)
	@NotAudited
	private EnovationHuisarts huisarts;

	public MammaBeoordeling getBeoordeling()
	{
		return beoordeling;
	}

	public void setBeoordeling(MammaBeoordeling beoordeling)
	{
		this.beoordeling = beoordeling;
	}

	public MammaHuisartsBerichtStatus getStatus()
	{
		return status;
	}

	public void setStatus(MammaHuisartsBerichtStatus status)
	{
		this.status = status;
	}

	public Date getStatusDatum()
	{
		return statusDatum;
	}

	public void setStatusDatum(Date statusDatum)
	{
		this.statusDatum = statusDatum;
	}

	public EnovationHuisarts getHuisarts()
	{
		return huisarts;
	}

	public void setHuisarts(EnovationHuisarts huisarts)
	{
		this.huisarts = huisarts;
	}

	public Date getVerzendDatum()
	{
		return verzendDatum;
	}

	public void setVerzendDatum(Date verzendDatum)
	{
		this.verzendDatum = verzendDatum;
	}
}
