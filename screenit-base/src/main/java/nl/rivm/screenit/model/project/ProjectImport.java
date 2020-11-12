
package nl.rivm.screenit.model.project;

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

import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.OneToOne;
import javax.persistence.Table;

import nl.rivm.screenit.model.UploadDocument;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;

@Entity
@Table(schema = "algemeen")
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
@Deprecated
public class ProjectImport extends ProjectSelectie
{

	private static final long serialVersionUID = 1L;

	@OneToOne(fetch = FetchType.LAZY, optional = false)
	private ProjectGroep groep;

	@OneToOne
	private UploadDocument uploadDocument;

	private Boolean skipFouten;

	public UploadDocument getUploadDocument()
	{
		return uploadDocument;
	}

	public void setUploadDocument(UploadDocument uploadDocument)
	{
		this.uploadDocument = uploadDocument;
	}

	public ProjectGroep getGroep()
	{
		return groep;
	}

	public void setGroep(ProjectGroep groep)
	{
		this.groep = groep;
	}

	public Boolean getSkipFouten()
	{
		return skipFouten;
	}

	public void setSkipFouten(Boolean skipFouten)
	{
		this.skipFouten = skipFouten;
	}

}
