package nl.rivm.screenit.main.web.gebruiker.screening.mamma.palga;

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

import java.io.IOException;

import nl.rivm.screenit.main.model.mamma.MammaPalgaUploadDto;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.panels.UploadDocumentFormComponentPanel;
import nl.rivm.screenit.main.web.component.validator.FileValidator;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.enums.FileType;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.mamma.MammaPalgaService;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class MammaPalgaImportPanel extends Panel
{
	@SpringBean
	private LogService logService;

	@SpringBean
	private MammaPalgaService palgaService;

	@SpringBean(name = "testModus")
	private Boolean testModus;

	public MammaPalgaImportPanel(String id)
	{
		super(id);
		IModel<MammaPalgaUploadDto> importModel = new CompoundPropertyModel<>(new MammaPalgaUploadDto());
		importModel.getObject().setImportDocument(palgaService.getImport());
		add(new MammaPalgaImportForm("importForm", importModel));
	}

	private class MammaPalgaImportForm extends Form<MammaPalgaUploadDto>
	{
		private MammaPalgaImportForm(String id, IModel<MammaPalgaUploadDto> model)
		{
			super(id, model);
			FileValidator validator;
			if (testModus)
			{
				validator = new FileValidator(FileType.CSV, FileType.ZIP);
			}
			else
			{
				validator = new FileValidator(FileType.ZIP);
			}
			UploadDocumentFormComponentPanel uploadPanel = new UploadDocumentFormComponentPanel("importDocument", new PropertyModel<>(model, "importDocument"), validator);
			add(uploadPanel);
			add(new TextField<>("zipWachtwoord"));
			add(new IndicatingAjaxSubmitLink("submit")
			{
				@Override
				protected void onSubmit(AjaxRequestTarget target)
				{
					UploadDocument importDocument = uploadPanel.getUploadDocumentFromSelectedFile();
					if (uploadPanel.isVerwijderd())
					{
						palgaService.deleteImports();
						setResponsePage(new MammaPalgaUitwisselingPage());
					}
					else if (importDocument != null)
					{
						if (importDocument.getNaam().endsWith(".zip"))
						{
							importZip(importDocument);
						}
						else if (testModus && importDocument.getNaam().endsWith(".csv"))
						{
							importCsv(importDocument);
						}
						else
						{
							error(getString("FileTypeValidator"));
						}
					}
				}

				private void importCsv(UploadDocument importDocument)
				{
					try
					{
						palgaService.saveOrUpdateImport(importDocument);
						setResponsePage(new MammaPalgaUitwisselingPage());
						ScreenitSession.get().info(getString("palga.import.success"));
					}
					catch (IOException e)
					{
						error(e.getMessage());
					}
				}

				private void importZip(UploadDocument importDocument)
				{
					String zipWachtwoord = getModelObject().getZipWachtwoord();
					if (zipWachtwoord != null)
					{
						String message = palgaService.saveOrUpdateImportZip(importDocument, zipWachtwoord);
						if (message != null)
						{
							error(message);
						}
						else
						{
							ScreenitSession.get().info(getString("palga.import.success"));
							setResponsePage(new MammaPalgaUitwisselingPage());
						}

					}
					else
					{
						error(getString("palga.import.error.wachtwoord.null"));
					}
				}

			});
		}
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();
	}

}
