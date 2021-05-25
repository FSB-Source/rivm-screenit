package nl.rivm.screenit.main.web.client.dashboard.colon;

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

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.client.base.ClientBasePage;
import nl.rivm.screenit.main.web.client.base.ClientHoofdMenuitem;
import nl.rivm.screenit.main.web.component.SimpleStringResourceModel;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.colon.ColonClientAfspraakAfzeggenPanel;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.colon.ColonIntakeAfspraak;
import nl.rivm.screenit.model.colon.planning.AfspraakStatus;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.colon.AfspraakService;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.form.AjaxSubmitLink;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.link.Link;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.AANPASSEN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = Recht.CLIENT_GEGEVENS,
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.COLON })
public class ClientAfspraakAfzeggenPage extends ClientBasePage
{
	@SpringBean
	private AfspraakService afspraakService;

	public ClientAfspraakAfzeggenPage(IModel<ColonIntakeAfspraak> afspraakModel)
	{
		Form<ColonIntakeAfspraak> form = new Form<ColonIntakeAfspraak>("form", afspraakModel);

		form.add(new ColonClientAfspraakAfzeggenPanel("details", afspraakModel));

		form.add(new AjaxSubmitLink("submit")
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				afspraakService.annuleerAfspraak(form.getModelObject(), ScreenitSession.get().getLoggedInAccount(), AfspraakStatus.GEANNULEERD_CLIENT, false);
				ScreenitSession.get().success(getString("success.afspraak_afgezegd"));
				setResponsePage(ClientColonDashboardPage.class);
			}

		});
		form.add(new Link<Client>("annuleren")
		{
			@Override
			public void onClick()
			{
				setResponsePage(ClientColonDashboardPage.class);
			}

		});
		add(form);
	}

	@Override
	public ClientHoofdMenuitem getHoofdMenuitem()
	{
		return ClientHoofdMenuitem.MIJNBEVOLKINGSONDERZOEK_COLON;
	}

	@Override
	public IModel<String> getPageName()
	{
		return new SimpleStringResourceModel("label.afspraakafzeggen");
	}

}
