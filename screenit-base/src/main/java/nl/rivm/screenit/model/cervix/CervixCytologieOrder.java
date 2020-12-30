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

import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.Index;
import javax.persistence.Lob;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

import nl.rivm.screenit.model.cervix.enums.CervixCytologieOrderStatus;
import nl.rivm.screenit.model.cervix.enums.CervixCytologieReden;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.annotations.Type;
import org.hibernate.envers.Audited;

@Entity
@Table(schema = "cervix", name = "cytologie_order", indexes = { @Index(name = "idx_CERVIX_CYTOLOGIE_ORDER_STATUS", columnList = "status") })
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
@Audited
public class CervixCytologieOrder extends AbstractHibernateObject
{

	private static final long serialVersionUID = 1L;

	@OneToOne(mappedBy = "cytologieOrder", optional = false)
	private CervixUitstrijkje uitstrijkje;

	@Enumerated(value = EnumType.STRING)
	@Column(nullable = false)
	private CervixCytologieOrderStatus status;

	@Column(nullable = false)
	@Temporal(TemporalType.TIMESTAMP)
	private Date statusDatum;

	@Enumerated(value = EnumType.STRING)
	@Column(nullable = false)
	private CervixCytologieReden cytologieReden;

	@Lob
	@Type(type = "org.hibernate.type.TextType")
	@Column(nullable = false)
	private String hl7Bericht;

	public CervixUitstrijkje getUitstrijkje()
	{
		return uitstrijkje;
	}

	public void setUitstrijkje(CervixUitstrijkje uitstrijkje)
	{
		this.uitstrijkje = uitstrijkje;
	}

	public CervixCytologieOrderStatus getStatus()
	{
		return status;
	}

	public void setStatus(CervixCytologieOrderStatus status)
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

	public CervixCytologieReden getCytologieReden()
	{
		return cytologieReden;
	}

	public void setCytologieReden(CervixCytologieReden cytologieReden)
	{
		this.cytologieReden = cytologieReden;
	}

	public Date getStatusDatum()
	{
		return statusDatum;
	}

	public void setStatusDatum(Date statusDatum)
	{
		this.statusDatum = statusDatum;
	}
}
