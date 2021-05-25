
package nl.rivm.screenit.model.formulieren;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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
import javax.persistence.FetchType;
import javax.persistence.ManyToOne;
import javax.persistence.OneToOne;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.UploadDocument;
import nl.topicuszorg.formulieren2.persistence.instantie.FormulierInstantieImpl;

@Entity
public class ScreenitFormulierInstantie extends FormulierInstantieImpl
{

	private static final long serialVersionUID = 1L;

	@Temporal(TemporalType.TIMESTAMP)
	private Date creatieDatum;

	@Enumerated(EnumType.STRING)
	private TypeFormulier typeFormulier;

	@ManyToOne(fetch = FetchType.LAZY)
	private InstellingGebruiker uploader;

	@ManyToOne(fetch = FetchType.LAZY)
	private InstellingGebruiker templateUploader;

	@OneToOne(fetch = FetchType.LAZY)
	private UploadDocument templateVanGebruiker;

	public Date getCreatieDatum()
	{
		return creatieDatum;
	}

	public void setCreatieDatum(Date creatieDatum)
	{
		this.creatieDatum = creatieDatum;
	}

	public TypeFormulier getTypeFormulier()
	{
		return typeFormulier;
	}

	public void setTypeFormulier(TypeFormulier typeFormulier)
	{
		this.typeFormulier = typeFormulier;
	}

	public InstellingGebruiker getUploader()
	{
		return uploader;
	}

	public void setUploader(InstellingGebruiker uploader)
	{
		this.uploader = uploader;
	}

	public UploadDocument getTemplateVanGebruiker()
	{
		return templateVanGebruiker;
	}

	public void setTemplateVanGebruiker(UploadDocument templateVanGebruiker)
	{
		this.templateVanGebruiker = templateVanGebruiker;
	}

	public InstellingGebruiker getTemplateUploader()
	{
		return templateUploader;
	}

	public void setTemplateUploader(InstellingGebruiker templateUploader)
	{
		this.templateUploader = templateUploader;
	}
}
