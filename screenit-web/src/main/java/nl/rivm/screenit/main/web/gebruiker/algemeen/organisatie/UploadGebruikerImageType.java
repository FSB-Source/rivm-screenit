package nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie;

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

import nl.rivm.screenit.main.web.component.panels.UploadImageType;
import nl.rivm.screenit.main.web.component.validator.FileValidator;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.enums.FileType;

import org.apache.wicket.markup.html.form.upload.FileUpload;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.util.ListModel;

public enum UploadGebruikerImageType implements UploadImageType<Gebruiker>
{

	HANDTEKENING("handtekening.naam")
	{

		@Override
		public UploadDocument getUploadDocument(IModel<Gebruiker> model)
		{
			return model.getObject().getHandtekening();
		}

		@Override
		public UploadDocument setUploadDocument(IModel<Gebruiker> model)
		{
			UploadDocument uploadDocument = createNieuwUploadDocument();
			model.getObject().setHandtekening(uploadDocument);
			return uploadDocument;
		}

		@Override
		public FileValidator getValidator()
		{
			return HANDTEKENING_FILE_VALIDATOR;
		}

		@Override
		public void setContentType(UploadDocument nieuwUploadDocument, ListModel<FileUpload> fileUploads)
		{
			nieuwUploadDocument.setContentType(fileUploads.getObject().get(0).getContentType());
		}

	};

	private static final FileValidator HANDTEKENING_FILE_VALIDATOR = new FileValidator(FileType.JPG);

	private final String labelnaam;

	UploadGebruikerImageType(String labelnaam)
	{
		this.labelnaam = labelnaam;
	}

	@Override
	public String getLabelnaam()
	{
		return labelnaam;
	}

	private static UploadDocument createNieuwUploadDocument()
	{
		UploadDocument uploadDocument = new UploadDocument();
		uploadDocument.setActief(true);
		return uploadDocument;
	}
}
