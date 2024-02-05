package nl.rivm.screenit.main.web.gebruiker.algemeen.intervalcarcinoom;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.Arrays;
import java.util.List;
import java.util.concurrent.Executors;

import nl.rivm.screenit.main.service.impl.KoppelresultatenKankerregistratieVerwerkenThread;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.component.validator.FileValidator;
import nl.rivm.screenit.main.web.gebruiker.algemeen.AlgemeenPage;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.FileType;
import nl.rivm.screenit.model.enums.Recht;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.upload.FileUpload;
import org.apache.wicket.markup.html.form.upload.FileUploadField;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.util.ListModel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.AANPASSEN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = { Recht.GEBRUIKER_KOPPELRESULTATEN_KANKERREGISTRATIE },
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.COLON })
public class UploadKoppelresultatenKankerregistratiePage extends AlgemeenPage
{
	private static final long serialVersionUID = 1L;

	private static final Logger LOG = LoggerFactory.getLogger(UploadKoppelresultatenKankerregistratiePage.class);

	public UploadKoppelresultatenKankerregistratiePage()
	{
		Form<GbaPersoon> form = new Form<>("form");
		add(form);

		IModel<List<FileUpload>> files = new ListModel<>();

		IModel<Bevolkingsonderzoek> bvoModel = Model.of(Bevolkingsonderzoek.COLON);
		ScreenitDropdown<Bevolkingsonderzoek> bvo = ComponentHelper.addDropDownChoice(form, "bvo", true, Arrays.asList(Bevolkingsonderzoek.COLON), false);
		bvo.setModel(bvoModel);
		form.add(bvo);

		FileUploadField upload = new FileUploadField("bestand", files);
		upload.add(new FileValidator(FileType.CSV));
		upload.setRequired(true);
		form.add(upload);

		IndicatingAjaxSubmitLink submit = new IndicatingAjaxSubmitLink("submit", form)
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{

				if (files.getObject().size() == 1)
				{

					FileUpload fileUpload = files.getObject().get(0);

					try
					{
						Executors.newSingleThreadExecutor().submit(new KoppelresultatenKankerregistratieVerwerkenThread(
							fileUpload.writeToTempFile(), fileUpload.getContentType(), fileUpload.getClientFileName(), ScreenitSession.get().getLoggedInInstellingGebruiker(),
							bvoModel.getObject()));

						info(getString("upload.gestart"));

					}
					catch (Exception e)
					{
						LOG.error("Onbekende fout ", e);
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
