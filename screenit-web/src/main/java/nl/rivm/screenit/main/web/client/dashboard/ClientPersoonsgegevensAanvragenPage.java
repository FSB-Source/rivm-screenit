package nl.rivm.screenit.main.web.client.dashboard;

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

import nl.rivm.screenit.main.service.algemeen.OverdrachtPersoonsgegevensService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.client.base.ClientBasePage;
import nl.rivm.screenit.main.web.client.base.ClientHoofdMenuitem;
import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.main.web.component.SimpleStringResourceModel;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.form.AjaxSubmitLink;
import org.apache.wicket.markup.html.WebMarkupContainer;
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
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
public class ClientPersoonsgegevensAanvragenPage extends ClientBasePage
{
	@SpringBean
	private OverdrachtPersoonsgegevensService overdrachtPersoonsgegevensService;

	private ClientHoofdMenuitem menu;

	private final boolean isBriefOnderweg;

	ClientPersoonsgegevensAanvragenPage(IModel<Client> clientModel, ClientHoofdMenuitem menu)
	{
		this.menu = menu;
		isBriefOnderweg = overdrachtPersoonsgegevensService.heeftVerzoekZonderGegenereerdeBrief(clientModel.getObject());

		Form<Client> form = new ScreenitForm<>("form", clientModel);
		add(form);

		form.add(new Link<Void>("annuleren")
		{
			@Override
			public void onClick()
			{
				setResponsePage(menu.getTargetClass());
			}
		});

		form.add(maakBriefOnderwegContainer());
		form.add(maakOpslaanLink(clientModel));
	}

	private WebMarkupContainer maakBriefOnderwegContainer()
	{
		WebMarkupContainer container = new WebMarkupContainer("briefOnderweg");
		container.setVisible(isBriefOnderweg);
		return container;
	}

	private AjaxSubmitLink maakOpslaanLink(IModel<Client> clientModel)
	{
		AjaxSubmitLink opslaanLink = new IndicatingAjaxSubmitLink("opslaan")
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				super.onSubmit(target);
				overdrachtPersoonsgegevensService.maakOverdrachtVerzoek(clientModel.getObject());
				ScreenitSession.get().success(getString("success.persoonsgegevensAanvragen"));
				setResponsePage(menu.getTargetClass());
			}
		};
		opslaanLink.setVisible(!isBriefOnderweg);
		return opslaanLink;
	}

	@Override
	public ClientHoofdMenuitem getHoofdMenuitem()
	{
		return menu;
	}

	@Override
	public IModel<String> getPageName()
	{
		return new SimpleStringResourceModel("label.persoonsgegevensAanvragen");
	}
}
