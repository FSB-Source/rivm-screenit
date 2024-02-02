package nl.rivm.screenit.main.web.gebruiker.base;

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

import java.util.List;

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.service.AuthenticatieService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.Component;
import org.apache.wicket.ajax.AbstractDefaultAjaxBehavior;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormComponentUpdatingBehavior;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.WebPage;
import org.apache.wicket.markup.html.form.ChoiceRenderer;
import org.apache.wicket.markup.html.form.DropDownChoice;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.spring.injection.annot.SpringBean;

class OrganisatieWisselPanel extends GenericPanel<InstellingGebruiker>
{
	@SpringBean
	private AuthenticatieService authenticatieService;

	private WebMarkupContainer organisatiepanel;

	OrganisatieWisselPanel(String id)
	{
		super(id);
		InstellingGebruiker ingelogdeGebruiker = getIngelogdeGebruiker();

		setModel(ModelUtil.sModel(ingelogdeGebruiker));

		organisatiepanel = new WebMarkupContainer("organisatiepanel");
		organisatiepanel.add(createOrganisatiesDropdown());
		organisatiepanel.setOutputMarkupId(true);
		add(organisatiepanel);
	}

	private DropDownChoice<InstellingGebruiker> createOrganisatiesDropdown()
	{
		List<InstellingGebruiker> organisatieMedewerkers = authenticatieService.getActieveInstellingGebruikers(getIngelogdeGebruiker().getMedewerker());
		DropDownChoice<InstellingGebruiker> organisatie = new ScreenitDropdown<>("organisatie", getModel(),
			ModelUtil.listRModel(organisatieMedewerkers), new ChoiceRenderer<>("organisatie.naam"));

		organisatie.setNullValid(false);
		organisatie.setOutputMarkupId(true);

		AbstractDefaultAjaxBehavior wisselBehavior = createWisselBehavior();
		organisatie.add(wisselBehavior);
		AbstractDefaultAjaxBehavior cancelBehavior = createCancelBehavior();
		organisatie.add(cancelBehavior);

		organisatie.add(new AjaxFormComponentUpdatingBehavior("change")
		{
			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				target.appendJavaScript("if (confirmUnsavedChanges()) " +
					"{" + wisselBehavior.getCallbackScript() + "}" +
					"else {" + cancelBehavior.getCallbackScript() + "};");
			}
		});

		return organisatie;
	}

	private AbstractDefaultAjaxBehavior createWisselBehavior()
	{
		return new AbstractDefaultAjaxBehavior()
		{
			@Override
			protected void respond(AjaxRequestTarget target)
			{
				InstellingGebruiker gewensteInstellingGebruiker = getModelObject();
				Component pageForInstellingGebruiker = ScreenitSession.get().getPageForInstellingGebruiker(gewensteInstellingGebruiker);
				if (pageForInstellingGebruiker != null)
				{
					ScreenitSession.get().clear();
					setResponsePage((WebPage) pageForInstellingGebruiker);
				}
				else
				{
					error(getString("error.nietvoldoende.rechten"));
				}
			}
		};
	}

	private AbstractDefaultAjaxBehavior createCancelBehavior()
	{
		return new AbstractDefaultAjaxBehavior()
		{
			@Override
			protected void respond(AjaxRequestTarget target)
			{
				setModelObject(getIngelogdeGebruiker());
				target.add(organisatiepanel);
			}
		};
	}

	private InstellingGebruiker getIngelogdeGebruiker()
	{
		return ScreenitSession.get().getLoggedInInstellingGebruiker();
	}
}
