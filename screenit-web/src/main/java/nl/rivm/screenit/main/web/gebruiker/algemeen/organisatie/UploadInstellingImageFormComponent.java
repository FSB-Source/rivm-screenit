package nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import nl.rivm.screenit.main.web.component.panels.UploadImageFormComponent;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.enums.FileStoreLocation;

import org.apache.wicket.model.IModel;

public class UploadInstellingImageFormComponent extends UploadImageFormComponent<Instelling>
{

	private static final long serialVersionUID = 1L;

	public UploadInstellingImageFormComponent(String id, IModel<Instelling> model, final UploadInstellingImageType whatToUpload)
	{
		super(id, model, whatToUpload, FileStoreLocation.SCREENINGORGANISATIE_AFBEELDINGEN, false, true);
	}
}
