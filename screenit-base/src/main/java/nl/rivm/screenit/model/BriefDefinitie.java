
package nl.rivm.screenit.model;

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

import javax.annotation.CheckForNull;
import javax.annotation.Nullable;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;
import javax.persistence.Transient;
import javax.persistence.UniqueConstraint;

import nl.rivm.screenit.model.enums.BriefType;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;

@Entity
@Table(schema = "algemeen", uniqueConstraints = @UniqueConstraint(name = "uc_brief_definitie_type_laatst_gewijzigd", columnNames = { "briefType", "laatstGewijzigd" }))
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
public class BriefDefinitie extends AbstractHibernateObject implements IDocument
{

	private static final long serialVersionUID = 1L;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private BriefType briefType;

	@ManyToOne(optional = false)
	private UploadDocument document;

	@Column(nullable = false)
	@Temporal(TemporalType.TIMESTAMP)
	private Date laatstGewijzigd;

	@Transient
	private Date geldigTot = null;

	@ManyToOne(fetch = FetchType.LAZY, optional = true)
	private InstellingGebruiker uploader;

	private String formulierNummer;

	@Transient
	private int volgnummer;

	public BriefType getBriefType()
	{
		return briefType;
	}

	public void setBriefType(BriefType briefType)
	{
		this.briefType = briefType;
	}

	@Override
	public UploadDocument getDocument()
	{
		return document;
	}

	@Override
	public void setDocument(UploadDocument document)
	{
		this.document = document;
	}

	@Override
	public Date getLaatstGewijzigd()
	{
		return laatstGewijzigd;
	}

	@Override
	public void setLaatstGewijzigd(Date laatstGewijzigd)
	{
		this.laatstGewijzigd = laatstGewijzigd;
	}

	public InstellingGebruiker getUploader()
	{
		return uploader;
	}

	public void setUploader(InstellingGebruiker uploader)
	{
		this.uploader = uploader;
	}

	public String getFormulierNummer()
	{
		return formulierNummer;
	}

	public void setFormulierNummer(String formulierNummer)
	{
		this.formulierNummer = formulierNummer;
	}

	public int getVolgnummer()
	{
		return volgnummer;
	}

	public void setVolgnummer(int volgnummer)
	{
		this.volgnummer = volgnummer;
	}

	@CheckForNull
	public Date getGeldigTot()
	{
		return geldigTot;
	}

	public void setGeldigTot(@Nullable Date geldigTot)
	{
		this.geldigTot = geldigTot;
	}
}
