package nl.rivm.screenit.main.model.mamma;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.model.UploadDocument;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.model.IDetachable;
import org.apache.wicket.model.IModel;

public class MammaPalgaUploadDto implements IDetachable
{
	private IModel<UploadDocument> importDocument;

	private String zipWachtwoord;

	public UploadDocument getImportDocument()
	{
		return ModelUtil.nullSafeGet(importDocument);
	}

	public void setImportDocument(UploadDocument importDocument)
	{
		this.importDocument = ModelUtil.sModel(importDocument);
	}

	public String getZipWachtwoord()
	{
		return zipWachtwoord;
	}

	public void setZipWachtwoord(String zipWachtwoord)
	{
		this.zipWachtwoord = zipWachtwoord;
	}

	@Override
	public void detach()
	{
		ModelUtil.nullSafeDetach(importDocument);
	}
}
