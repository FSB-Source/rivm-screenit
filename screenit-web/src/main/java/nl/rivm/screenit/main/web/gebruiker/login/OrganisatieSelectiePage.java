package nl.rivm.screenit.main.web.gebruiker.login;

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

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.service.AuthenticatieService;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.Component;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.head.IHeaderResponse;
import org.apache.wicket.markup.head.OnDomReadyHeaderItem;
import org.apache.wicket.markup.html.WebPage;
import org.apache.wicket.markup.html.form.ChoiceRenderer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.wiquery.core.javascript.JsQuery;
import org.wicketstuff.wiquery.core.javascript.JsScopeContext;
import org.wicketstuff.wiquery.core.javascript.JsScopeEvent;
import org.wicketstuff.wiquery.core.javascript.helper.EventsHelper;

public class OrganisatieSelectiePage extends LoginBasePage
{

	private static final long serialVersionUID = 1L;

	private IModel<InstellingGebruiker> gekozenInstellingGebruiker;

	@SpringBean
	private AuthenticatieService authenticatieService;

	private IndicatingAjaxSubmitLink selecteer;

	public OrganisatieSelectiePage(Gebruiker medewerker)
	{
		Form<Instelling> selectOrganisatieForm = new Form<Instelling>("selectOrganisatieForm");

		List<InstellingGebruiker> organisatieMedewerkers = this.authenticatieService.getActieveInstellingGebruikers(medewerker);
		final ScreenitDropdown<InstellingGebruiker> organisatie = new ScreenitDropdown<InstellingGebruiker>("organisatie", new PropertyModel<>(this, "gekozenInstellingGebruiker"),
			ModelUtil.listRModel(organisatieMedewerkers), new ChoiceRenderer<>("organisatie.naam"));

		organisatie.setOpenOnEnter(false);
		organisatie.setNullValid(false);
		selectOrganisatieForm.add(organisatie);
		if (organisatieMedewerkers.size() > 0)
		{
			setGekozenInstellingGebruiker(organisatieMedewerkers.get(0));
		}

		selecteer = new IndicatingAjaxSubmitLink("selecteer")
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				InstellingGebruiker gebruiker = getGekozenInstellingGebruiker();
				Component pageForInstellingGebruiker = ScreenitSession.get().getPageForInstellingGebruiker(gebruiker);
				if (pageForInstellingGebruiker != null)
				{
					setResponsePage((WebPage) pageForInstellingGebruiker);
				}
				else
				{
					error(getString("error.nietvoldoende.rechten"));
				}
			}
		};

		selectOrganisatieForm.setDefaultButton(selecteer);
		selectOrganisatieForm.add(selecteer);

		add(selectOrganisatieForm);
	}

	public InstellingGebruiker getGekozenInstellingGebruiker()
	{
		return ModelUtil.nullSafeGet(gekozenInstellingGebruiker);
	}

	public void setGekozenInstellingGebruiker(InstellingGebruiker gekozenInstellingGebruiker)
	{
		this.gekozenInstellingGebruiker = ModelUtil.sModel(gekozenInstellingGebruiker);
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(gekozenInstellingGebruiker);
	}

	@Override
	public void renderHead(IHeaderResponse response)
	{
		super.renderHead(response);

		response.render(new OnDomReadyHeaderItem(new JsQuery().document().chain(EventsHelper.keypress(new JsScopeEvent()
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void execute(JsScopeContext scopeContext)
			{
				scopeContext.append("var code = (event.keyCode ? event.keyCode : event.which);");
				scopeContext.append("if (code == 13) {");
				scopeContext.append(new JsQuery(selecteer).$().chain("click").render());
				scopeContext.append("}");
			}
		})).render()));
	}
}
