package nl.rivm.screenit.main.web.gebruiker.algemeen.retourzending;

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

import java.util.List;

import nl.rivm.screenit.main.service.RetourzendingService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.component.modal.IDialog;
import nl.rivm.screenit.main.web.component.validator.FileValidator;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.FileType;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.logging.RetourzendingLogEvent;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxButton;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.FormComponent;
import org.apache.wicket.markup.html.form.upload.FileUpload;
import org.apache.wicket.markup.html.form.upload.FileUploadField;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.util.ListModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.INZIEN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = Recht.GEBRUIKER_SCREENING_RETOURZENDINGEN,
	bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX })
public class RetourzendingenVerwerkenPage extends RetourzendingBasePage
{

	private static final long serialVersionUID = 1L;

	private static final Logger LOG = LoggerFactory.getLogger(RetourzendingenVerwerkenPage.class);

	@SpringBean
	private RetourzendingService retourzendingService;

	public RetourzendingenVerwerkenPage()
	{
		Form<Void> form = new Form<>("form");

		final IModel<List<FileUpload>> retourzendingBestanden = new ListModel<>();

		FormComponent<List<FileUpload>> retourzendingBestand = new FileUploadField("retourzendingBestand", retourzendingBestanden)
			.add(new FileValidator(FileType.EXCEL_NIEUW));
		form.add(retourzendingBestand);
		retourzendingBestand.setRequired(true);
		retourzendingBestand.setOutputMarkupId(true);

		final BootstrapDialog dialog = new BootstrapDialog("dialog");
		add(dialog);
		form.add(new IndicatingAjaxButton("verwerken", form)
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				if (retourzendingBestanden.getObject().size() == 1)
				{

					FileUpload retourzendingBestandFileUpload = retourzendingBestanden.getObject().get(0);

					try
					{
						RetourzendingLogEvent logEvent = retourzendingService.verwerkBestandMetRetourzendingen(ScreenitSession.get().getLoggedInInstellingGebruiker(),
							retourzendingBestandFileUpload.getContentType(), retourzendingBestandFileUpload.writeToTempFile(), retourzendingBestandFileUpload.getClientFileName());
						dialog.openWith(target, new RetourzendingenVerwerkingsVerslagPopup(IDialog.CONTENT_ID, ModelUtil.cRModel(logEvent)));
					}
					catch (Exception e)
					{
						LOG.error("Fout bij verwerken van het bestand", e);
						error("Fout bij verwerken van het bestand");
					}
				}
			}
		});

		add(form);
	}
}
