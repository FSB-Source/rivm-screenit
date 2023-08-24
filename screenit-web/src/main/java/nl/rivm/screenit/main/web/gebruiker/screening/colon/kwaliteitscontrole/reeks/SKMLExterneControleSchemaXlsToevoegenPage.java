package nl.rivm.screenit.main.web.gebruiker.screening.colon.kwaliteitscontrole.reeks;

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

import java.io.IOException;
import java.util.List;

import nl.rivm.screenit.main.model.SKMLImportVoortgang;
import nl.rivm.screenit.main.service.SKMLExterneSchemaXlsService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.validator.AantalBestandenUploadenValidator;
import nl.rivm.screenit.main.web.component.validator.FileValidator;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerBasePage;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.colon.SKMLExternSchema;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.FileType;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.LogService;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.form.CheckBox;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.upload.FileUpload;
import org.apache.wicket.markup.html.form.upload.FileUploadField;
import org.apache.wicket.markup.html.link.Link;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.util.ListModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.util.lang.Bytes;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.AANPASSEN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = Recht.GEBRUIKER_BEHEER_SCHEMA_EXTERNE_CONTROLE,
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.COLON })
public class SKMLExterneControleSchemaXlsToevoegenPage extends KwaliteitscontroleBasePage
{

	private static final long serialVersionUID = 1L;

	private static final Logger LOG = LoggerFactory.getLogger(SKMLExterneControleSchemaXlsToevoegenPage.class);

	private IModel<List<FileUpload>> fileUploadModel = new ListModel<>();

	private IModel<Boolean> allesOverschrijven = new Model<Boolean>(Boolean.FALSE);

	@SpringBean
	private SKMLExterneSchemaXlsService xlsService;

	@SpringBean
	private LogService logService;

	public SKMLExterneControleSchemaXlsToevoegenPage()
	{
		add(new Link<Void>("terugViaTitle")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick()
			{
				setResponsePage(SKMLExterneControleSchemaPage.class);
			}
		});

		Form<Void> uploadForm = new Form<Void>("uploadForm");
		uploadForm.setMultiPart(true);
		uploadForm.add(new CheckBox("allesOverschrijven", allesOverschrijven));
		FileUploadField uploadveld = new FileUploadField("bestand", fileUploadModel);
		uploadveld.setRequired(true);
		uploadveld.add(new FileValidator(FileType.EXCEL_NIEUW));
		uploadveld.add(new AantalBestandenUploadenValidator(1));
		uploadForm.add(uploadveld);
		uploadForm.setMaxSize(Bytes.megabytes(40));
		uploadForm.add(new IndicatingAjaxSubmitLink("importeer")
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				if (fileUploadModel != null && CollectionUtils.isNotEmpty(fileUploadModel.getObject()))
				{
					FileUpload excelfile = fileUploadModel.getObject().get(0);

					if (StringUtils.equals("application/octet-stream", excelfile.getContentType()) && !StringUtils.endsWith(excelfile.getClientFileName(), ".xls")
						&& !StringUtils.endsWith(excelfile.getClientFileName(), ".xlsx"))
					{
						error("Het opgeladen bestand is niet herkend als valide excel bestand.");
					}
					else
					{
						try
						{
							LOG.info("Er wordt een SKML XLS schema geupload, alles overschrijven: " + allesOverschrijven.getObject());
							logService.logGebeurtenis(LogGebeurtenis.SKML_SCHEMA_IMPORT_GESTART, ScreenitSession.get().getLoggedInAccount(), Bevolkingsonderzoek.COLON);
							SKMLImportVoortgang voortgang = xlsService.importSchemaXls(excelfile.getInputStream(), allesOverschrijven.getObject());
							if (CollectionUtils.isEmpty(voortgang.getFoutmeldingen()))
							{
								info("SKML externe controle schema is succesvol verwerkt.");
							}
							else if (CollectionUtils.isNotEmpty(voortgang.getFoutmeldingen()) && (voortgang.getNieuw() > 0 || voortgang.getGeupdate() > 0))
							{
								info("SKML externe controle schema is verwerkt, maar er waren regels met fouten.");
							}
							else
							{
								error("SKML externe controle schema is niet succesvol verwerkt.");
							}
							String melding = voortgang.toString();
							logService.logGebeurtenis(LogGebeurtenis.SKML_SCHEMA_IMPORT_AFGEROND, ScreenitSession.get().getLoggedInAccount(), melding, Bevolkingsonderzoek.COLON);
						}
						catch (IOException e)
						{
							LOG.error(e.getMessage(), e);
						}
					}
				}
				else
				{
					error("Er was geen bestand geupload, er zijn geen wijzigingen doorgevoerd in het SKML schema.");
				}
			}
		});
		add(uploadForm);
	}

	@Override
	protected Class<? extends GebruikerBasePage> getActiveContextMenuClass()
	{
		return SKMLExterneControleSchemaPage.class;
	}
}
