package nl.rivm.screenit.main.web.gebruiker.screening.colon.importcapverdeling;

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

import nl.rivm.screenit.main.service.ImportCapVerdelingService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.component.validator.FileValidator;
import nl.rivm.screenit.main.web.gebruiker.screening.colon.ColonScreeningBasePage;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.FileType;
import nl.rivm.screenit.model.enums.Level;
import nl.rivm.screenit.model.enums.Recht;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxButton;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.FormComponent;
import org.apache.wicket.markup.html.form.upload.FileUpload;
import org.apache.wicket.markup.html.form.upload.FileUploadField;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.util.ListModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.INZIEN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = Recht.GEBRUIKER_IMPORT_CAP_VERDELING,
	bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON })
public class ImportCapVerdelingPage extends ColonScreeningBasePage
{
	private static final long serialVersionUID = 1L;

	private static final Logger LOG = LoggerFactory.getLogger(ImportCapVerdelingPage.class);

	@SpringBean
	private ImportCapVerdelingService importCapVerdelingService;

	public ImportCapVerdelingPage()
	{
		Form<Void> form = new ScreenitForm<>("form");

		final IModel<List<FileUpload>> importCapVerdelingen = new ListModel<>();

		FormComponent<List<FileUpload>> importCapVerdeling = new FileUploadField("importCapVerdeling", importCapVerdelingen)
			.add(new FileValidator(FileType.EXCEL_NIEUW));
		form.add(importCapVerdeling);
		importCapVerdeling.setRequired(true);
		importCapVerdeling.setOutputMarkupId(true);
		importCapVerdeling.setLabel(Model.of("Bestand retourzendingen"));

		final BootstrapDialog dialog = new BootstrapDialog("dialog");
		add(dialog);
		form.add(new IndicatingAjaxButton("verwerken", form)
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				if (importCapVerdelingen.getObject().size() == 1)
				{

					FileUpload importCapVerdelingFileUpload = importCapVerdelingen.getObject().get(0);

					try
					{
						Level level = importCapVerdelingService.verwerkBestand(ScreenitSession.get().getLoggedInInstellingGebruiker(),
							importCapVerdelingFileUpload.writeToTempFile());
						switch (level)
						{
						case ERROR:
							error("Fout opgetreden tijdens importeren (zie logregel).");
							break;
						case WARNING:
							warn("Verwerking voltooid, maar met intakelocaties met minder dan 100% capaciteitsverdeling (zie logregel).");
							break;
						case INFO:
							success("Verwerking voor alle intakelocaties succesvol voltooid (zie logregel).");
							break;
						}
					}
					catch (Exception e)
					{
						LOG.error("Er is een fout opgetreden!", e);
					}

				}
			}
		});

		add(form);
	}

}
