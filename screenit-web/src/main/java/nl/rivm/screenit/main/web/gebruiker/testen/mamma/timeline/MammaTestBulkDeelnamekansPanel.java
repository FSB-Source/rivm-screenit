package nl.rivm.screenit.main.web.gebruiker.testen.mamma.timeline;

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

import nl.rivm.screenit.main.service.mamma.MammaTestTimelineService;
import nl.rivm.screenit.main.web.component.validator.FileValidator;
import nl.rivm.screenit.model.enums.FileType;

import org.apache.commons.collections.CollectionUtils;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxButton;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.upload.FileUpload;
import org.apache.wicket.markup.html.form.upload.FileUploadField;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.util.ListModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class MammaTestBulkDeelnamekansPanel extends Panel
{
	private static final long serialVersionUID = 1L;

	private static final Logger LOGGER = LoggerFactory.getLogger(MammaTestBulkDeelnamekansPanel.class);

	private IModel<List<FileUpload>> filesUploaded;

	@SpringBean
	private MammaTestTimelineService testTimelineService;

	public MammaTestBulkDeelnamekansPanel(String id)
	{
		super(id);

		Form<Void> form = new Form<>("deelnamekansenForm");
		add(form);

		filesUploaded = new ListModel<>();
		form.add(new FileUploadField("deelnamekansenFile", filesUploaded).add(new FileValidator(FileType.CSV)));

		IndicatingAjaxButton uploadDeelnamekansen = new IndicatingAjaxButton("uploadDeelnamekansen")
		{
			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				LOGGER.info("Upload deelnamekansen");
				List<FileUpload> fileUploads = filesUploaded.getObject();
				if (CollectionUtils.isNotEmpty(fileUploads))
				{
					try
					{
						FileUpload fileUpload = fileUploads.get(0);
						String message = testTimelineService.setDeelnamekansen(fileUpload.getInputStream());
						if (message.contains("Succesvol"))
						{
							info(message);
						}
						else
						{
							error(message);
						}
						LOGGER.info("Deelnamekansen " + fileUpload.getClientFileName() + " uploaded.");
					}
					catch (Exception e)
					{
						LOGGER.error("Fout bij uploaden deelnamekansen.", e);
					}
				}
			}

			@Override
			protected void onError(AjaxRequestTarget target)
			{
				LOGGER.info("Error upload deelnamekansen");
			}

		};
		form.add(uploadDeelnamekansen);
	}
}
