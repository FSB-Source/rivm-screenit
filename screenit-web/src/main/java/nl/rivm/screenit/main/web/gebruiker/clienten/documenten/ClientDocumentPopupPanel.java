package nl.rivm.screenit.main.web.gebruiker.clienten.documenten;

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

import java.util.List;

import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.main.web.component.validator.FileValidator;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.enums.FileType;
import nl.rivm.screenit.service.ClientService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.form.AjaxSubmitLink;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.upload.FileUpload;
import org.apache.wicket.markup.html.form.upload.FileUploadField;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.util.ListModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public abstract class ClientDocumentPopupPanel extends GenericPanel<UploadDocument>
{

	@SpringBean
	private ClientService clientService;

	private static final long serialVersionUID = 1L;

	private final IModel<Client> selectedClientModel;

	public ClientDocumentPopupPanel(String id, final IModel<UploadDocument> model, IModel<Client> selectedClient, final WebMarkupContainer documentenContainer)
	{
		super(id, model);
		this.selectedClientModel = selectedClient;

		final IModel<List<FileUpload>> files = new ListModel<>();

		Form<UploadDocument> uploadForm = new ScreenitForm<>("uploadForm", model);
		uploadForm.add(new FileUploadField("fileUpload", files)
			.setRequired(true)
			.add(new FileValidator(FileType.PDF)));
		uploadForm.add(new AjaxSubmitLink("toevoegen")
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{

				UploadDocument nieuwDocument = (UploadDocument) getForm().getModelObject();
				nieuwDocument.setActief(Boolean.TRUE);
				try
				{
					FileUpload fileUpload = files.getObject().get(0);
					nieuwDocument.setFile(fileUpload.writeToTempFile());
					nieuwDocument.setNaam(fileUpload.getClientFileName());
					nieuwDocument.setContentType(fileUpload.getContentType());
					clientService.saveDocumentForClient(nieuwDocument, ModelUtil.nullSafeGet(selectedClientModel));
					close(target);
					info("Document is succesvol geupload");
				}
				catch (Exception e)
				{
					error("Document kon niet worden geupload");
				}

				target.add(documentenContainer);

			}

		});
		add(uploadForm);
	}

	@Override
	protected void detachModel()
	{
		super.detachModel();
		ModelUtil.nullSafeDetach(selectedClientModel);
	}

	public abstract void close(AjaxRequestTarget target);

}
