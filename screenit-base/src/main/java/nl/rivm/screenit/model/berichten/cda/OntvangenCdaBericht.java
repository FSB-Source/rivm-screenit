package nl.rivm.screenit.model.berichten.cda;

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
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.Index;
import javax.persistence.Lob;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

import nl.rivm.screenit.model.berichten.enums.BerichtStatus;
import nl.rivm.screenit.model.berichten.enums.BerichtType;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.annotations.Type;

@Entity(name = "cda_ontvangenbericht")
@Table(schema = "gedeeld", indexes = { @Index(name = "idx_ontvangenbericht_status", columnList = "status") })
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "verslag.cache")
public class OntvangenCdaBericht extends AbstractHibernateObject
{
	private static final long serialVersionUID = 1L;

	@Temporal(TemporalType.TIMESTAMP)
	private Date ontvangen;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private BerichtStatus status;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private BerichtType berichtType;

	@Column
	private String berichtId;

	@Column
	private String setId;

	@Column
	private Long versie;

	@Lob
	@Type(type = "org.hibernate.type.TextType")
	private String xmlBericht;

	@Column
	private String projectVersion;

	public Date getOntvangen()
	{
		return this.ontvangen;
	}

	public void setOntvangen(Date ontvangen)
	{
		this.ontvangen = ontvangen;
	}

	public BerichtType getBerichtType()
	{
		return berichtType;
	}

	public void setBerichtType(BerichtType berichtType)
	{
		this.berichtType = berichtType;
	}

	public String getXmlBericht()
	{
		return xmlBericht;
	}

	public void setXmlBericht(String xmlBericht)
	{
		this.xmlBericht = xmlBericht;
	}

	public void setBerichtId(String berichtId)
	{
		this.berichtId = berichtId;
	}

	public String getBerichtId()
	{
		return berichtId;
	}

	public String getSetId()
	{
		return setId;
	}

	public void setSetId(String setId)
	{
		this.setId = setId;
	}

	public Long getVersie()
	{
		return versie;
	}

	public void setVersie(Long versie)
	{
		this.versie = versie;
	}

	public BerichtStatus getStatus()
	{
		return status;
	}

	public void setStatus(BerichtStatus status)
	{
		this.status = status;
	}

	public String getProjectVersion()
	{
		return projectVersion;
	}

	public void setProjectVersion(String projectVersion)
	{
		this.projectVersion = projectVersion;
	}
}
