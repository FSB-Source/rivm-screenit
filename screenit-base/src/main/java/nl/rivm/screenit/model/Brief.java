
package nl.rivm.screenit.model;

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
import javax.persistence.ManyToOne;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;
import javax.persistence.Transient;

import nl.rivm.screenit.model.cervix.CervixBrief;
import nl.rivm.screenit.model.colon.ColonBrief;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.mamma.MammaBrief;

import org.hibernate.Hibernate;
import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.envers.Audited;

import static org.hibernate.envers.RelationTargetAuditMode.NOT_AUDITED;

@Entity
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
@Audited
public abstract class Brief extends TablePerClassHibernateObject
{
	
	private static final long serialVersionUID = 1L;

	@ManyToOne(optional = true, fetch = FetchType.LAZY)
	@Audited(targetAuditMode = NOT_AUDITED)
	private BriefDefinitie briefDefinitie;

	@Enumerated(EnumType.STRING)
	private BriefType briefType;

	@Temporal(TemporalType.TIMESTAMP)
	private Date creatieDatum;

	private boolean gegenereerd;

	@Column(nullable = false)
	private boolean vervangen = false;

	@Column(nullable = false)
	private boolean tegenhouden = false;

	private String templateNaam;

	public Date getCreatieDatum()
	{
		return creatieDatum;
	}

	public void setCreatieDatum(Date creatieDatum)
	{
		this.creatieDatum = creatieDatum;
	}

	public boolean isGegenereerd()
	{
		return gegenereerd;
	}

	public void setGegenereerd(boolean gegenereerd)
	{
		this.gegenereerd = gegenereerd;
	}

	public String getTemplateNaam()
	{
		return templateNaam;
	}

	public void setTemplateNaam(String templateNaam)
	{
		this.templateNaam = templateNaam;
	}

	public boolean isVervangen()
	{
		return vervangen;
	}

	public void setVervangen(boolean vervangen)
	{
		this.vervangen = vervangen;
	}

	public boolean isTegenhouden()
	{
		return tegenhouden;
	}

	public void setTegenhouden(boolean tegenhouden)
	{
		this.tegenhouden = tegenhouden;
	}

	public BriefType getBriefType()
	{
		return briefType;
	}

	public void setBriefType(BriefType briefType)
	{
		this.briefType = briefType;
	}

	public abstract MergedBrieven getMergedBrieven();

	public abstract void setMergedBrieven(MergedBrieven mergedBrieven);

	@Transient
	public Bevolkingsonderzoek getBevolkingsonderzoek()
	{
		Class aClass = Hibernate.getClass(this);
		Bevolkingsonderzoek bevolkingsonderzoek = null;
		if (aClass == ColonBrief.class)
		{
			bevolkingsonderzoek = Bevolkingsonderzoek.COLON;
		}
		else if (aClass == CervixBrief.class)
		{
			bevolkingsonderzoek = Bevolkingsonderzoek.CERVIX;
		}
		else if (aClass == MammaBrief.class)
		{
			bevolkingsonderzoek = Bevolkingsonderzoek.MAMMA;
		}
		return bevolkingsonderzoek;
	}

	public BriefDefinitie getBriefDefinitie()
	{
		return briefDefinitie;
	}

	public void setBriefDefinitie(BriefDefinitie briefDefinitie)
	{
		this.briefDefinitie = briefDefinitie;
	}
}
