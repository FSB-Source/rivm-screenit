package nl.rivm.screenit.main.web.gebruiker.algemeen.documenttemplatetesten.fragments;

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

import java.util.Arrays;
import java.util.List;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.main.web.gebruiker.algemeen.documenttemplatetesten.DocumentTemplateTestenFieldsPanel;
import nl.rivm.screenit.main.web.gebruiker.algemeen.documenttemplatetesten.behavior.EnableBehavior;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.apache.wicket.MarkupContainer;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.form.ChoiceRenderer;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class MammaAfspraakRedenVerzetFragment extends DocumentTemplateTestenFieldsPanelComponentFragment<String>
{

	private static final String MARKUP_ID = "fragmentMammaAfspraakRedenVerzet";

	@SpringBean
	private SimplePreferenceService preferenceService;

	public MammaAfspraakRedenVerzetFragment(final String id,
		final MarkupContainer markupProvider,
		final IModel<String> model)
	{
		this(id, markupProvider, model, null);
	}

	public MammaAfspraakRedenVerzetFragment(final String id,
		final MarkupContainer markupProvider,
		final IModel<String> model,
		final IModel<Boolean> enabled)
	{
		super(id, MARKUP_ID, markupProvider, model, enabled);
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();
		List<String> literals = Arrays.asList(preferenceService.getString(PreferenceKey.MAMMA_BULK_VERZETTEN_VERLEDEN_AFSPRAAK_TEKST.toString(), ""),
			preferenceService.getString(PreferenceKey.MAMMA_BULK_VERZETTEN_TOEKOMST_AFSPRAAK_TEKST.toString(), ""));

		add(getFormComponentDropDown(literals));
	}

	private WebMarkupContainer getFormComponentDropDown(List<String> literals)
	{
		return ((WebMarkupContainer) DocumentTemplateTestenFieldsPanel.getScreenitDropdown("formComponent", getModel(), literals, new ChoiceRenderer<>(), true)
			.add(new EnableBehavior(true)));
	}

}
