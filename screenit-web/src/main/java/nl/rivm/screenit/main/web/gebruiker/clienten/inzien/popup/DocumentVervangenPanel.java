package nl.rivm.screenit.main.web.gebruiker.clienten.inzien.popup;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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
import java.util.List;

import nl.rivm.screenit.main.web.component.ConfirmingIndicatingAjaxSubmitLink;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.component.validator.FileValidator;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.enums.FileType;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.upload.FileUpload;
import org.apache.wicket.markup.html.form.upload.FileUploadField;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.util.ListModel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public abstract class DocumentVervangenPanel extends Panel
{
	private static final Logger LOG = LoggerFactory.getLogger(DocumentVervangenPanel.class);

	private IModel<List<FileUpload>> files = new ListModel<>();

	private UploadDocument uploadDocument;

	private File tempFile;

	protected DocumentVervangenPanel(String id)
	{
		super(id);
		createVervangenPanel();
	}

	private void createVervangenPanel()
	{
		final BootstrapDialog dialog = new BootstrapDialog("dialog");
		add(dialog);

		Form uploadForm = new Form<>("uploadForm");

		FileUploadField upload = new FileUploadField("fileUpload", files);
		upload.add(new FileValidator(FileType.PDF));
		upload.setRequired(true);
		uploadForm.add(upload);

		uploadForm.add(new ConfirmingIndicatingAjaxSubmitLink<Void>("doorgaan", dialog, "vervangen.document")
		{
			@Override
			protected boolean skipConfirmation()
			{
				if (files.getObject().size() == 1)
				{
					FileUpload fileUpload = files.getObject().get(0);
					try
					{
						tempFile = fileUpload.writeToTempFile();
						tempFile.deleteOnExit();
						maakUploadDocument(fileUpload);
					}
					catch (Exception e)
					{
						LOG.error("Fout bij uploaden van een formulier: ", e);
						error(getString("error.onbekend"));
						return true;
					}
				}
				else
				{
					LOG.error("Er mag maar 1 bestand geuploaded worden als formulier");
					error(getString("error.onjuistaantalfiles"));
					return true;

				}

				return super.skipConfirmation();
			}

			@Override
			public void onNoClick(AjaxRequestTarget target)
			{
				super.onNoClick(target);
				tempFile.delete();
			}

			@Override
			public void onCloseClick(AjaxRequestTarget target)
			{
				super.onCloseClick(target);
				tempFile.delete();
			}

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				uploadDocument.setFile(tempFile);
				vervangDocument(uploadDocument, target);
			}
		});

		add(uploadForm);
	}

	protected abstract void vervangDocument(UploadDocument uploadDocument, AjaxRequestTarget target);

	private void maakUploadDocument(FileUpload fileUpload)
	{
		uploadDocument = new UploadDocument();
		uploadDocument.setActief(Boolean.TRUE);
		uploadDocument.setContentType(fileUpload.getContentType());
		uploadDocument.setNaam(fileUpload.getClientFileName());
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(files);
	}
}
