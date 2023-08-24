package nl.rivm.screenit.main.web.component.panels;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.enums.FileStoreLocation;
import nl.rivm.screenit.service.UploadDocumentService;
import nl.topicuszorg.hibernate.object.model.HibernateObject;

import org.apache.commons.collections.CollectionUtils;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.form.AjaxSubmitLink;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.upload.FileUpload;
import org.apache.wicket.markup.html.form.upload.FileUploadField;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.model.util.ListModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

@Slf4j
public class UploadImageFormComponent<T extends HibernateObject> extends GenericPanel<T>
{
	@SpringBean
	private UploadDocumentService uploadDocumentService;

	private final ListModel<FileUpload> fileUploads = new ListModel<>();

	private final Label labelBestandsNaam;

	private final FileStoreLocation fileStoreLocation;

	private final UploadImageType<T> whatToUpload;

	private final AjaxSubmitLink verwijderButton;

	public UploadImageFormComponent(String id, IModel<T> model, final UploadImageType<T> whatToUpload, final FileStoreLocation fileStoreLocation, boolean magVerwijderdWorden,
		boolean directUploaden)
	{
		super(id, model);
		this.whatToUpload = whatToUpload;
		this.fileStoreLocation = fileStoreLocation;

		final Form<Instelling> form = new Form<>("uploadForm");
		add(form);
		form.add(new FileUploadField("fileUpload", fileUploads).add(whatToUpload.getValidator()));

		labelBestandsNaam = new Label("uploadFieldNaam", new PropertyModel<>(model, whatToUpload.getLabelnaam()))
		{
			@Override
			protected void onConfigure()
			{
				super.onConfigure();
				UploadDocument document = whatToUpload.getUploadDocument(UploadImageFormComponent.this.getModel());
				setVisible(document != null && document.getActief());
			}
		};
		labelBestandsNaam.setOutputMarkupId(true);
		form.add(labelBestandsNaam);

		boolean isBestaande = model.getObject().getId() != null;
		form.add(new AjaxSubmitLink("uploaden")
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				uploadImage(target);
			}

		}.setVisible(directUploaden && isBestaande));

		verwijderButton = new AjaxSubmitLink("verwijderen")
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				IModel<T> model = UploadImageFormComponent.this.getModel();
				UploadDocument uploadDocument = whatToUpload.getUploadDocument(model);
				uploadDocument.setActief(false);
				labelBestandsNaam.setVisible(false);
				target.add(labelBestandsNaam);
				target.add(this);
				warn("Om het bestand definitief te verwijderen moet de opslaan knop gebruikt worden.");
			}

			@Override
			protected void onConfigure()
			{
				super.onConfigure();
				UploadDocument document = whatToUpload.getUploadDocument(UploadImageFormComponent.this.getModel());
				setVisible(magVerwijderdWorden && isBestaande && document != null && document.getActief());
			}

		};
		form.add(verwijderButton);

	}

	public boolean hasFile()
	{
		return CollectionUtils.isNotEmpty(fileUploads.getObject());
	}

	public void uploadImage(AjaxRequestTarget target)
	{
		if (hasFile())
		{
			IModel<T> model = UploadImageFormComponent.this.getModel();
			UploadDocument oudUploadDocument = whatToUpload.getUploadDocument(model);
			UploadDocument nieuwUploadDocument = whatToUpload.setUploadDocument(model);

			whatToUpload.setContentType(nieuwUploadDocument, fileUploads);
			createFileInTmp(target, nieuwUploadDocument);
			if (oudUploadDocument != null)
			{
				uploadDocumentService.delete(oudUploadDocument);
			}
			target.add(labelBestandsNaam);
			target.add(verwijderButton);
		}
		else
		{
			error("Er is geen bestand aanwezig!");
		}
	}

	private void createFileInTmp(AjaxRequestTarget target, UploadDocument uploadDocument)
	{
		try
		{
			uploadDocument.setFile(fileUploads.getObject().get(0).writeToTempFile());
			uploadDocument.setNaam(fileUploads.getObject().get(0).getClientFileName());
			uploadDocumentService.saveOrUpdate(uploadDocument, fileStoreLocation, (Long) getModel().getObject().getId());
			if (target != null)
			{
				target.add(labelBestandsNaam);
				info("De file is succesvol geupload!");
			}
		}
		catch (Exception e)
		{
			error("Upload van file mislukt");
			LOG.error("Upload van file mislukt", e);
			return;
		}
	}
}
