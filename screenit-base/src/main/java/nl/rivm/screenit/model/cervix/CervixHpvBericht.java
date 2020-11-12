
package nl.rivm.screenit.model.cervix;

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

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.Lob;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

import nl.rivm.screenit.model.BMHKLaboratorium;
import nl.rivm.screenit.model.berichten.enums.BerichtStatus;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.annotations.Type;
import org.hibernate.envers.Audited;
import org.hibernate.envers.NotAudited;

@Entity
@Table(schema = "cervix", name = "hpv_bericht")
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "cervix.cache")
@Audited
public class CervixHpvBericht extends AbstractHibernateObject
{
	
	private static final long serialVersionUID = 1L;

	@Column(nullable = false, unique = true)
	private String messageId;

	@Column(nullable = false)
	@Temporal(TemporalType.TIMESTAMP)
	private Date ontvangen;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private BerichtStatus status;

	@Column(nullable = false)
	@Temporal(TemporalType.TIMESTAMP)
	private Date statusDatum;

	@Lob
	@Type(type = "org.hibernate.type.TextType")
	@Column(nullable = false)
	@NotAudited
	private String hl7Bericht;

	@ManyToOne(fetch = FetchType.LAZY, optional = false)
	private BMHKLaboratorium laboratorium;

	@Column(nullable = false)
	private String instrumentId;

	@NotAudited
	@OneToMany(mappedBy = "hpvBericht", fetch = FetchType.LAZY)
	private List<CervixHpvBeoordeling> hpvBeoordelingen = new ArrayList<CervixHpvBeoordeling>();

	public Date getOntvangen()
	{
		return ontvangen;
	}

	public void setOntvangen(Date ontvangen)
	{
		this.ontvangen = ontvangen;
	}

	public BerichtStatus getStatus()
	{
		return status;
	}

	public void setStatus(BerichtStatus status)
	{
		this.status = status;
	}

	public String getHl7Bericht()
	{
		return hl7Bericht;
	}

	public void setHl7Bericht(String hl7Bericht)
	{
		this.hl7Bericht = hl7Bericht;
	}

	public String getMessageId()
	{
		return messageId;
	}

	public void setMessageId(String messageId)
	{
		this.messageId = messageId;
	}

	public Date getStatusDatum()
	{
		return statusDatum;
	}

	public void setStatusDatum(Date statusDatum)
	{
		this.statusDatum = statusDatum;
	}

	public BMHKLaboratorium getLaboratorium()
	{
		return laboratorium;
	}

	public void setLaboratorium(BMHKLaboratorium laboratorium)
	{
		this.laboratorium = laboratorium;
	}

	public String getInstrumentId()
	{
		return instrumentId;
	}

	public void setInstrumentId(String instrumentId)
	{
		this.instrumentId = instrumentId;
	}

	public List<CervixHpvBeoordeling> getHpvBeoordelingen()
	{
		return hpvBeoordelingen;
	}

	public void setHpvBeoordelingen(List<CervixHpvBeoordeling> hpvBeoordelingen)
	{
		this.hpvBeoordelingen = hpvBeoordelingen;
	}

}
