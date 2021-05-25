package nl.rivm.screenit.main.web.component.table;

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

import java.io.File;
import java.util.List;

import nl.rivm.screenit.main.web.component.validator.FileValidator;
import nl.rivm.screenit.model.IDocument;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.service.FileService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
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

public abstract class UploadDocumentPanel<T extends IDocument> extends GenericPanel<T>
{

	private static final long serialVersionUID = 1L;

	private FileValidator fileValidator;

	private IModel<List<FileUpload>> files = new ListModel<>();

	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private FileService fileService;

	private WebMarkupContainer refreshContainer;

	@SpringBean
	private ICurrentDateSupplier dateSupplier;

	public UploadDocumentPanel(String id, IModel<T> model, WebMarkupContainer refreshContainer, FileValidator validator)
	{
		super(id, model);
		this.fileValidator = validator;
		this.refreshContainer = refreshContainer;

		Form<Void> uploadForm = new Form<>("uploadForm");

		FileUploadField field = new FileUploadField("fileUpload", files);
		uploadForm.add(field);

		if (getFileValidator() != null)
		{
			field.add(getFileValidator());
		}
		uploadForm.add(new AjaxSubmitLink("uploaden")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onSubmit(AjaxRequestTarget target)
			{
				if (getFiles().getObject() != null && getFiles().getObject().size() == 1)
				{
					try
					{

						FileUpload fileUpload = getFiles().getObject().get(0);
						T modelObject = UploadDocumentPanel.this.getModelObject();

						UploadDocument oldDocument = modelObject.getDocument();
						oldDocument.setActief(false);
						hibernateService.saveOrUpdate(oldDocument);

						UploadDocument newDocument = new UploadDocument();
						File file = fileUpload.writeToTempFile();
						newDocument.setFile(file);
						newDocument.setNaam(fileUpload.getClientFileName());
						newDocument.setContentType(fileUpload.getContentType());
						newDocument.setActief(true);

						saveUploadDocument(target, modelObject, newDocument);
						target.add(UploadDocumentPanel.this.refreshContainer);
					}
					catch (Exception e)
					{
						error("Bestand kon niet worden geupload");
					}
				}
			}
		});
		add(uploadForm);
	}

	protected abstract void saveUploadDocument(AjaxRequestTarget target, T modelObject, UploadDocument document);

	public FileValidator getFileValidator()
	{
		return fileValidator;
	}

	public void setFileValidator(FileValidator fileValidator)
	{
		this.fileValidator = fileValidator;
	}

	public IModel<List<FileUpload>> getFiles()
	{
		return files;
	}

	public void setFiles(IModel<List<FileUpload>> files)
	{
		this.files = files;
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(files);
	}
}
