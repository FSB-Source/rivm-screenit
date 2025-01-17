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
import nl.rivm.screenit.model.BMHKLaboratorium;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.enums.FileType;

import org.apache.wicket.markup.html.form.upload.FileUpload;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.util.ListModel;

public enum UploadInstellingImageType implements UploadImageType<Instelling>
{

	DK_SO_LOGO_INPAKCENTRUM("logo.naam")
	{

		@Override
		public UploadDocument getUploadDocument(IModel<Instelling> model)
		{
			return getScreningOrganisatie(model).getLogo();
		}

		@Override
		public UploadDocument setUploadDocument(IModel<Instelling> model)
		{
			UploadDocument uploadDocument = createNieuwUploadDocument();
			getScreningOrganisatie(model).setLogo(uploadDocument);
			return uploadDocument;
		}

		@Override
		public FileValidator getValidator()
		{
			return DK_SO_LOGO_INPAKCENTRUM_FILE_VALIDATOR;
		}

		@Override
		public void setContentType(UploadDocument nieuwUploadDocument, ListModel<FileUpload> fileUploads)
		{
			nieuwUploadDocument.setContentType("application/eps");
		}

	},
	DK_SO_LOGO_BRIEF("logoBrief.naam")
	{
		@Override
		public UploadDocument getUploadDocument(IModel<Instelling> model)
		{
			return getScreningOrganisatie(model).getLogoBrief();
		}

		@Override
		public UploadDocument setUploadDocument(IModel<Instelling> model)
		{
			UploadDocument uploadDocument = createNieuwUploadDocument();
			getScreningOrganisatie(model).setLogoBrief(uploadDocument);
			return uploadDocument;
		}

		@Override
		public FileValidator getValidator()
		{
			return LOGO_BRIEF_FILE_VALIDATOR;
		}

		@Override
		public void setContentType(UploadDocument nieuwUploadDocument, ListModel<FileUpload> fileUploads)
		{
			nieuwUploadDocument.setContentType("application/jpeg");
		}

	},
	DK_SO_BESTUUR("bestuurSign.naam")
	{
		@Override
		public UploadDocument getUploadDocument(IModel<Instelling> model)
		{
			return getScreningOrganisatie(model).getBestuurSign();
		}

		@Override
		public UploadDocument setUploadDocument(IModel<Instelling> model)
		{
			UploadDocument uploadDocument = createNieuwUploadDocument();
			getScreningOrganisatie(model).setBestuurSign(uploadDocument);
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

	},
	DK_SO_RCMDL("rcmdlSign.naam")
	{
		@Override
		public UploadDocument getUploadDocument(IModel<Instelling> model)
		{
			return getScreningOrganisatie(model).getRcmdlSign();
		}

		@Override
		public UploadDocument setUploadDocument(IModel<Instelling> model)
		{
			UploadDocument uploadDocument = createNieuwUploadDocument();
			getScreningOrganisatie(model).setRcmdlSign(uploadDocument);
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

	},
	DK_SO_KWALITEITS_LOGO("kwaliteitslogo.naam")
	{

		@Override
		public UploadDocument getUploadDocument(IModel<Instelling> model)
		{
			return getScreningOrganisatie(model).getKwaliteitslogo();
		}

		@Override
		public UploadDocument setUploadDocument(IModel<Instelling> model)
		{
			UploadDocument uploadDocument = createNieuwUploadDocument();
			getScreningOrganisatie(model).setKwaliteitslogo(uploadDocument);
			return uploadDocument;
		}

		@Override
		public FileValidator getValidator()
		{
			return LOGO_BRIEF_FILE_VALIDATOR;
		}

		@Override
		public void setContentType(UploadDocument nieuwUploadDocument, ListModel<FileUpload> fileUploads)
		{
			nieuwUploadDocument.setContentType("application/jpeg");
		}
	},

	BMHK_HANDTEKENING_MEDISCH_MICROBIOLOOG("handtekeningMedischMircobioloog.naam")
	{
		@Override
		public UploadDocument getUploadDocument(IModel<Instelling> model)
		{
			return getBMHKLaboratorium(model).getHandtekeningMedischMircobioloog();
		}

		@Override
		public UploadDocument setUploadDocument(IModel<Instelling> model)
		{
			UploadDocument uploadDocument = createNieuwUploadDocument();
			getBMHKLaboratorium(model).setHandtekeningMedischMircobioloog(uploadDocument);
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
	},

	BMHK_HANDTEKENING_PATHOLOOG("handtekeningPatholoog.naam")
	{

		@Override
		public UploadDocument getUploadDocument(IModel<Instelling> model)
		{
			return getBMHKLaboratorium(model).getHandtekeningPatholoog();
		}

		@Override
		public UploadDocument setUploadDocument(IModel<Instelling> model)
		{
			UploadDocument uploadDocument = createNieuwUploadDocument();
			getBMHKLaboratorium(model).setHandtekeningPatholoog(uploadDocument);
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

	private static final FileValidator DK_SO_LOGO_INPAKCENTRUM_FILE_VALIDATOR = new FileValidator(FileType.EPS);

	private static final FileValidator LOGO_BRIEF_FILE_VALIDATOR = new FileValidator(FileType.JPG);

	private static final FileValidator HANDTEKENING_FILE_VALIDATOR = new FileValidator(FileType.JPG);

	private final String labelnaam;

	UploadInstellingImageType(String labelnaam)
	{
		this.labelnaam = labelnaam;
	}

	@Override
	public String getLabelnaam()
	{
		return labelnaam;
	}

	private static ScreeningOrganisatie getScreningOrganisatie(IModel<Instelling> model)
	{
		return (ScreeningOrganisatie) model.getObject();
	}

	private static BMHKLaboratorium getBMHKLaboratorium(IModel<Instelling> model)
	{
		return (BMHKLaboratorium) model.getObject();
	}

	private static UploadDocument createNieuwUploadDocument()
	{
		UploadDocument uploadDocument = new UploadDocument();
		uploadDocument.setActief(true);
		return uploadDocument;
	}
}
