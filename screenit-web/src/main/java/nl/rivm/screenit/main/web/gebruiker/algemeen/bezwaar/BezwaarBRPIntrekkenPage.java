package nl.rivm.screenit.main.web.gebruiker.algemeen.bezwaar;

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

import java.text.SimpleDateFormat;
import java.util.List;

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ScreenitDateTextField;
import nl.rivm.screenit.main.web.component.validator.FileValidator;
import nl.rivm.screenit.main.web.gebruiker.algemeen.AlgemeenPage;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.FileType;
import nl.rivm.screenit.model.enums.GbaStatus;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.BezwaarService;
import nl.rivm.screenit.service.ClientService;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;
import nl.topicuszorg.wicket.input.validator.BSNValidator;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormComponentUpdatingBehavior;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.markup.html.form.upload.FileUpload;
import org.apache.wicket.markup.html.form.upload.FileUploadField;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.util.ListModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.AANPASSEN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = { Recht.GEBRUIKER_BEZWAAR_BRP },
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX })
public class BezwaarBRPIntrekkenPage extends AlgemeenPage
{
	private static final long serialVersionUID = 1L;

	private static final Logger LOG = LoggerFactory.getLogger(BezwaarBRPIntrekkenPage.class);

	private SimpleDateFormat format = new SimpleDateFormat("dd-MM-yyyy");

	@SpringBean
	private BezwaarService bezwaarService;

	@SpringBean
	private ClientService clientService;

	public BezwaarBRPIntrekkenPage()
	{
		Form<GbaPersoon> form = new Form<>("form", new CompoundPropertyModel<GbaPersoon>(new GbaPersoon()));
		add(form);

		IModel<List<FileUpload>> files = new ListModel<>();

		TextField<String> bsnField = new TextField<>("bsn");
		bsnField.add(new BSNValidator(true, false));
		bsnField.setRequired(true);
		form.add(bsnField);

		form.add(new ScreenitDateTextField("geboortedatum").setRequired(true).setOutputMarkupId(true).add(new AjaxFormComponentUpdatingBehavior("change")
		{
			private static final long serialVersionUID = 1L;

			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				target.add(getComponent());
			}
		}));

		FileUploadField upload = new FileUploadField("bestand", files);
		upload.add(new FileValidator(FileType.PDF));
		upload.setRequired(true);
		form.add(upload);

		IndicatingAjaxSubmitLink submit = new IndicatingAjaxSubmitLink("submit", form)
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				GbaPersoon persoon = (GbaPersoon) form.getModelObject();

				Client client = clientService.getClientByBsn(persoon.getBsn());
				String geboortedatumClient = format.format(client.getPersoon().getGeboortedatum());
				String geboortedatumForm = format.format(persoon.getGeboortedatum());
				if (client != null && !geboortedatumClient.equals(geboortedatumForm))
				{
					error("Er is geen client gevonden met deze gegevens.");
					return;
				}
				if (GbaStatus.BEZWAAR != client.getGbaStatus())
				{
					error("Deze client heeft geen actief bezwaar op het BRP");
					return;
				}

				if (files.getObject().size() == 1)
				{

					FileUpload fileUpload = files.getObject().get(0);

					try
					{

						UploadDocument uploadDocument = new UploadDocument();
						uploadDocument.setActief(Boolean.TRUE);
						uploadDocument.setContentType(fileUpload.getContentType());
						uploadDocument.setFile(fileUpload.writeToTempFile());
						uploadDocument.setNaam(fileUpload.getClientFileName());

						bezwaarService.bezwaarBRPIntrekken(ScreenitSession.get().getLoggedInAccount(), client, uploadDocument);

						info("Bezwaar BRP ingetrokken");

					}
					catch (Exception e)
					{
						LOG.error("Fout bij uploaden van bezwaar BRP intrekken met handtekening: ", e);
						error(getString("error.onbekend"));
					}
				}
				else
				{
					LOG.error("Er mag maar 1 bestand geuploaded worden als afmeldingformulier met handtekening");
					error(getString("error.onjuistaantalfiles"));
				}
			}

		};
		form.add(submit);
	}
}
