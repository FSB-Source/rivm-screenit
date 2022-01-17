
package nl.rivm.screenit.model;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.io.File;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;
import javax.persistence.Transient;

import nl.topicuszorg.documentupload.model.IUploadDocument;
import nl.topicuszorg.documentupload.services.UploadDocumentService;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Proxy;
import org.hibernate.envers.Audited;

@Audited
@Entity(name = "doc_upload_document")
@Proxy(lazy = true)
@Table(schema = "gedeeld")
public class UploadDocument extends AbstractHibernateObject implements IUploadDocument
{

	private static final long serialVersionUID = 1L;

	@Column(nullable = false)
	private Boolean actief;

	private String naam;

	private String path;

	private String contentType;

	@Transient
	private File file;

	public UploadDocument(File file, String naam, String contentType, boolean isActief)
	{
		this.setFile(file);
		this.setNaam(naam != null ? naam : file.getName());
		this.setContentType(contentType);
		this.setActief(isActief);
	}

	public UploadDocument()
	{

	}

	public String getNaam()
	{
		return this.naam;
	}

	public void setNaam(String naam)
	{
		this.naam = naam;
	}

	public String getPath()
	{
		return this.path;
	}

	public void setPath(String path)
	{
		this.path = path;
	}

	@Deprecated
	public File getFile()
	{
		return this.file;
	}

	public void setFile(File file)
	{
		this.file = file;
	}

	public String getContentType()
	{
		return this.contentType;
	}

	public void setContentType(String contentType)
	{
		this.contentType = contentType;
	}

	public Boolean getActief()
	{
		return this.actief;
	}

	public void setActief(Boolean actief)
	{
		this.actief = actief;
	}

}
