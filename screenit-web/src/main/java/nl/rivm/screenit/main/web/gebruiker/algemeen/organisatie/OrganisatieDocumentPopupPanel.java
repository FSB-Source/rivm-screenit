package nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.util.List;

import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.service.InstellingService;
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

public abstract class OrganisatieDocumentPopupPanel extends GenericPanel<UploadDocument>
{

	@SpringBean
	private InstellingService instellingService;

	private static final long serialVersionUID = 1L;

	private final IModel<Instelling> selectedOrganisatieModel;

	public OrganisatieDocumentPopupPanel(String id, final IModel<UploadDocument> model, IModel<Instelling> selectedOrganisatie, final WebMarkupContainer documentenContainer)
	{
		super(id, model);
		this.selectedOrganisatieModel = selectedOrganisatie;

		final IModel<List<FileUpload>> files = new ListModel<>();

		Form<UploadDocument> uploadForm = new ScreenitForm<>("uploadForm", model);
		uploadForm.add(new FileUploadField("fileUpload", files).setRequired(true));
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
					nieuwDocument.setFile(files.getObject().get(0).writeToTempFile());
					nieuwDocument.setNaam(files.getObject().get(0).getClientFileName());
					nieuwDocument.setContentType(files.getObject().get(0).getContentType());
					instellingService.saveDocumentForInstelling(nieuwDocument, ModelUtil.nullSafeGet(selectedOrganisatieModel));
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
		ModelUtil.nullSafeDetach(selectedOrganisatieModel);
	}

	public abstract void close(AjaxRequestTarget target);

}
