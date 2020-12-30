package nl.rivm.screenit.main.web.gebruiker.screening.colon.proefbvo;

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

import nl.rivm.screenit.main.service.ProefBvoService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.main.web.component.validator.FileValidator;
import nl.rivm.screenit.model.enums.FileType;

import org.apache.commons.lang.StringUtils;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxButton;
import org.apache.wicket.markup.html.basic.MultiLineLabel;
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

public class ProefBVOHeraanmeldenPage extends ProefBVOPage
{
	private static final long serialVersionUID = 1L;

	private static final Logger LOG = LoggerFactory.getLogger(ProefBVOPage.class);

	@SpringBean
	private ProefBvoService proefBvoService;

	public ProefBVOHeraanmeldenPage()
	{
		addMassaalHeraanmelden();
	}

	private void addMassaalHeraanmelden()
	{
		Form<Void> form = new ScreenitForm<>("form");

		final IModel<List<FileUpload>> clientenBestanden = new ListModel<>();

		FormComponent<List<FileUpload>> clientenBestand = new FileUploadField("clientenBestand", clientenBestanden)
			.add(new FileValidator(FileType.CSV));
		form.add(clientenBestand);
		clientenBestand.setRequired(true);
		clientenBestand.setOutputMarkupId(true);
		clientenBestand.setLabel(Model.of("Bestand met clienten"));

		final IModel<List<FileUpload>> heraanmeldBrieven = new ListModel<>();

		FormComponent<List<FileUpload>> heraanmeldBrief = new FileUploadField("heraanmeldBrief", heraanmeldBrieven)
			.add(new FileValidator(FileType.PDF));
		form.add(heraanmeldBrief.setRequired(true).setOutputMarkupId(true));
		heraanmeldBrief.setRequired(true);
		heraanmeldBrief.setOutputMarkupId(true);
		heraanmeldBrief.setLabel(Model.of("Heraanmeldbrief"));

		final Model<String> meldingenModel = new Model<String>("");
		final MultiLineLabel meldingen = new MultiLineLabel("meldingen", meldingenModel);
		meldingen.setEscapeModelStrings(false);
		meldingen.setOutputMarkupId(true);
		form.add(meldingen);
		form.add(new IndicatingAjaxButton("submit", form)
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				if (clientenBestanden.getObject().size() == 1)
				{

					FileUpload clientenBestandFileUpload = clientenBestanden.getObject().get(0);
					if (heraanmeldBrieven.getObject().size() == 1)
					{

						FileUpload heraanmeldBriefFileUpload = heraanmeldBrieven.getObject().get(0);
						try
						{

							meldingenModel.setObject(StringUtils.join(
								proefBvoService.heraanmelden(ScreenitSession.get().getLoggedInAccount(), heraanmeldBriefFileUpload.writeToTempFile(),
									heraanmeldBriefFileUpload.getContentType(), heraanmeldBriefFileUpload.getClientFileName(), clientenBestandFileUpload.writeToTempFile()),
								"<br>"));
							target.add(meldingen);
							info("Heraanmelding voor het proefbevolkingsonderzoek is voltooid");

						}
						catch (Exception e)
						{
							LOG.error("Fout bij uploaden van een afmeldingformulier met handtekening of clientenbestand: ", e);
							error(getString("error.onbekend"));
						}

					}
					else
					{
						LOG.error("Er mag maar 1 bestand geuploaded worden als afmeldingformulier met handtekening");
						error(getString("error.onjuistaantalfiles"));
					}
				}
				else
				{
					LOG.error("Er mag maar 1 bestand geuploaded worden als clientenbestand");
					error(getString("error.onjuistaantalfiles"));
				}
			}
		});
		add(form);
	}
}
