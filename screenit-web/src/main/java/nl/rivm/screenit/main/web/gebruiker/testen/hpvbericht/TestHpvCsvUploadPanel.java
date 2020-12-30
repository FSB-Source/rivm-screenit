package nl.rivm.screenit.main.web.gebruiker.testen.hpvbericht;

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

import java.io.File;
import java.util.List;
import java.util.Map;

import nl.rivm.screenit.main.service.cervix.HpvSendingMessageService;
import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.main.web.component.validator.FileValidator;
import nl.rivm.screenit.model.enums.FileType;
import nl.rivm.screenit.util.cervix.CsvToHpvBericht.CsvToHpvBericht;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.upload.FileUpload;
import org.apache.wicket.markup.html.form.upload.FileUploadField;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.util.ListModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

import ca.uhn.hl7v2.model.Message;

public class TestHpvCsvUploadPanel extends Panel
{

	private static final long serialVersionUID = 1L;

	@SpringBean
	private HpvSendingMessageService hpvSendingMessageService;

	private IModel<List<FileUpload>> files = new ListModel<>();

	public TestHpvCsvUploadPanel(String id)
	{
		super(id);

		Form<Void> uploadForm = new ScreenitForm<>("csvForm");
		add(uploadForm);

		FileUploadField upload = new FileUploadField("fileUpload", files);
		upload.setRequired(true);
		upload.add(new FileValidator(FileType.CSV));
		uploadForm.add(upload);

		uploadForm.add(new IndicatingAjaxSubmitLink("verstuurHl7Bericht", uploadForm)
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				super.onSubmit(target);
				File file = null;
				try
				{
					List<FileUpload> modelObject = files.getObject();
					FileUpload upload = modelObject.get(0);
					file = upload.writeToTempFile();

					List<Message> messages = CsvToHpvBericht.csvToHpvBerichten(file);
					Map<String, String> verstuurd = hpvSendingMessageService.verstuurHpvBerichten(messages);
					if (verstuurd != null && verstuurd.isEmpty())
					{
						info("Berichten zijn succesvol verstuurd");
					}
					else
					{
						error("Er is probleem opgetreden met het versturen.");
					}
				}
				catch (Exception e)
				{
					error("Er is iets misgegaan met het schrijven van het bestand naar de tmp.");
				}
				finally
				{

					if (file != null && file.exists())
					{
						file.delete();
					}
				}
			}
		});
	}
}
