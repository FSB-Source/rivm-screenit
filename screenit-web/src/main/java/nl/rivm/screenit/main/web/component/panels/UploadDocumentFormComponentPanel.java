package nl.rivm.screenit.main.web.component.panels;

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

import java.util.List;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.validator.FileValidator;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.service.UploadDocumentService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.upload.FileUpload;
import org.apache.wicket.markup.html.form.upload.FileUploadField;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.util.ListModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

@Slf4j
public class UploadDocumentFormComponentPanel extends GenericPanel<UploadDocument>
{

	private final IModel<List<FileUpload>> files = new ListModel<>();

	@SpringBean
	private UploadDocumentService uploadDocumentService;

	private boolean verwijderd = false;

	public UploadDocumentFormComponentPanel(String id, IModel<UploadDocument> model, FileValidator validator)
	{
		super(id, new CompoundPropertyModel<>(model));

		Label naamLabel = new Label("naam")
		{
			@Override
			protected void onConfigure()
			{
				super.onConfigure();
				UploadDocument document = UploadDocumentFormComponentPanel.this.getModelObject();
				setVisible(document != null && !Boolean.FALSE.equals(document.getActief()));
			}
		};
		naamLabel.setOutputMarkupId(true);
		add(naamLabel);
		FileUploadField field = new FileUploadField("fileUpload", files);
		add(field);

		if (validator != null)
		{
			field.add(validator);
		}

		add(new IndicatingAjaxLink<Void>("verwijderen")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				UploadDocumentFormComponentPanel.this.getModelObject().setActief(false);
				target.add(naamLabel, this);
				info(getString("bestand.verwijderd"));
				verwijderd = true;
			}

			@Override
			protected void onConfigure()
			{
				super.onConfigure();
				UploadDocument document = UploadDocumentFormComponentPanel.this.getModelObject();
				setVisible(document != null && !Boolean.FALSE.equals(document.getActief()));
			}

		}.setOutputMarkupId(true));
	}

	public UploadDocument getUploadDocumentFromSelectedFile()
	{
		UploadDocument newDocument = null;
		if (files.getObject() != null && files.getObject().size() == 1)
		{

			try
			{
				newDocument = ScreenitSession.get().fileUploadToUploadDocument(files.getObject().get(0));
			}
			catch (Exception e)
			{
				LOG.error("Bestand kon niet worden geupload", e);
				error(getString("bestand.upload.fout"));
			}
		}
		return newDocument;
	}

	public boolean isVerwijderd()
	{
		return verwijderd;
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(files);
	}
}
