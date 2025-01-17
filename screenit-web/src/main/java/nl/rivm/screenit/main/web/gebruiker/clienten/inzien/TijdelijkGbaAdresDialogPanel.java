package nl.rivm.screenit.main.web.gebruiker.clienten.inzien;

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

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.ConfirmingIndicatingAjaxLink;
import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.TijdelijkGbaAdres;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.ClientService;

import org.apache.commons.lang3.StringUtils;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.ajax.markup.html.form.AjaxSubmitLink;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.validation.validator.PatternValidator;

public abstract class TijdelijkGbaAdresDialogPanel extends GenericPanel<Client>
{

	private static final long serialVersionUID = 1L;

	@SpringBean
	private ClientService clientService;

	public TijdelijkGbaAdresDialogPanel(String id, IModel<Client> clientModel)
	{
		super(id, clientModel);

		boolean heeftLocatieOmschrijving = StringUtils.isNotBlank(clientModel.getObject().getPersoon().getTijdelijkGbaAdres().getLocatieBeschrijving());
		ScreenitForm<TijdelijkGbaAdres> form = new ScreenitForm<>("form");
		ComponentHelper.addTextField(form, "persoon.tijdelijkGbaAdres.straat", !heeftLocatieOmschrijving, 43, false);
		ComponentHelper.addTextField(form, "persoon.tijdelijkGbaAdres.huisnummer", !heeftLocatieOmschrijving, 10, Integer.class, false);
		ComponentHelper.addTextField(form, "persoon.tijdelijkGbaAdres.huisletter", false, 1, false).add(new PatternValidator(".*[a-zA-Z].*"));
		ComponentHelper.addTextField(form, "persoon.tijdelijkGbaAdres.huisnummerToevoeging", false, 26, false);
		ComponentHelper.addTextField(form, "persoon.tijdelijkGbaAdres.huisnummerAanduiding", false, 2, false);
		ComponentHelper.newPostcodeTextField(form, "persoon.tijdelijkGbaAdres.postcode", true, false);
		ComponentHelper.addTextField(form, "persoon.tijdelijkGbaAdres.plaats", true, 200, false);
		form.add(new Label("persoon.tijdelijkGbaAdres.locatieBeschrijving").setVisible(heeftLocatieOmschrijving));
		add(form);
		BootstrapDialog dialog = new BootstrapDialog("dialog");
		add(dialog);
		add(new ConfirmingIndicatingAjaxLink<Void>("verwijderen", dialog, "label.tijdelijkGbaAdres.verwijderen")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				clientService.verwijderTijdelijkGbaAdres(TijdelijkGbaAdresDialogPanel.this.getModelObject(), ScreenitSession.get().getLoggedInInstellingGebruiker());
				close(target);
			}
		}.setVisible(ScreenitSession.get().checkPermission(Recht.GEBRUIKER_GBA_TIJDELIJK_ADRES, Actie.VERWIJDEREN)));

		add(new AjaxSubmitLink("opslaan", form)
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				clientService.saveOrUpdateTijdelijkGbaAdres(TijdelijkGbaAdresDialogPanel.this.getModelObject(), ScreenitSession.get().getLoggedInInstellingGebruiker());
				close(target);
			}
		});

		add(new AjaxLink<Void>("annuleren")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				close(target);
			}
		});
	}

	protected abstract void close(AjaxRequestTarget target);

}
