package nl.rivm.screenit.main.web.gebruiker.algemeen.documenttemplatetesten.fragments;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import nl.rivm.screenit.main.service.MedewerkerService;
import nl.rivm.screenit.main.web.gebruiker.algemeen.documenttemplatetesten.BaseDocumentTemplateTestenPage;
import nl.rivm.screenit.main.web.gebruiker.algemeen.documenttemplatetesten.DocumentTemplateTestenFieldsPanel;
import nl.rivm.screenit.main.web.gebruiker.algemeen.documenttemplatetesten.behavior.EnableBehavior;
import nl.rivm.screenit.main.web.gebruiker.algemeen.documenttemplatetesten.behavior.VisibleBehavior;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.util.NaamUtil;

import org.apache.wicket.MarkupContainer;
import org.apache.wicket.Page;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.form.ChoiceRenderer;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.model.util.ListModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class BKRadioloogFragment extends DocumentTemplateTestenFieldsPanelComponentFragment<Gebruiker>
{

	private static final String MARKUP_ID = "fragmentBKRadioloogOndertekenaar";

	@SpringBean
	private MedewerkerService medewerkerService;

	public BKRadioloogFragment(final String componentId,
		final MarkupContainer markupProvider,
		final IModel<Gebruiker> model,
		final IModel<Boolean> enabled)
	{
		super(componentId, MARKUP_ID, markupProvider, model, enabled);
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();
		add(getFormComponentTextField());
		add(getFormComponentDropDown());
	}

	private List<Gebruiker> getActieveRadiologen()
	{
		Page page = getPage();
		ScreeningOrganisatie so = null;
		if (page instanceof BaseDocumentTemplateTestenPage)
		{
			so = ((BaseDocumentTemplateTestenPage) page).getSelectedRegio();
		}
		final ScreeningOrganisatie regio = so;
		return medewerkerService.getActieveRadiologen(new InstellingGebruiker(), Collections.emptyList(), "medewerker.achternaam", true).stream()
			.filter(ig -> ig.getOrganisatie().getOrganisatieType() == OrganisatieType.BEOORDELINGSEENHEID
				&& ig.getOrganisatie().getParent() != null
				&& ig.getOrganisatie().getParent().getRegio() != null
				&& ig.getOrganisatie().getParent().getRegio().equals(regio))
			.map(instellingGebruiker -> instellingGebruiker.getMedewerker())
			.distinct()
			.collect(Collectors.toList());
	}

	private TextField<String> getFormComponentTextField()
	{
		return (TextField<String>) DocumentTemplateTestenFieldsPanel.getTextField("textField", new PropertyModel<>(getModel(), "ondertekenaar"), String.class)
			.add(new VisibleBehavior())
			.add(new EnableBehavior());
	}

	private WebMarkupContainer getFormComponentDropDown()
	{
		return (WebMarkupContainer) DocumentTemplateTestenFieldsPanel
			.getScreenitDropdown("formComponent", getModel(), new ListModel<Gebruiker>()
			{
				@Override
				public List<Gebruiker> getObject()
				{
					return getActieveRadiologen();
				}
			}, new ChoiceRenderer<Gebruiker>()
			{

				@Override
				public Object getDisplayValue(Gebruiker object)
				{
					return NaamUtil.getNaamGebruiker(object);
				}
			}, true).add(new VisibleBehavior(true)).add(new EnableBehavior(true));
	}

}
