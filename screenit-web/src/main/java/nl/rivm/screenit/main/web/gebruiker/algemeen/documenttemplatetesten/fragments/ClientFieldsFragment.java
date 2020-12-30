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
import java.util.stream.Collectors;

import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.ScreenitDateTextField;
import nl.rivm.screenit.main.web.gebruiker.algemeen.documenttemplatetesten.DocumentTemplateTestWrapper;
import nl.rivm.screenit.main.web.gebruiker.algemeen.documenttemplatetesten.DocumentTemplateTestenFieldsPanel;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.util.TestBsnGenerator;
import nl.topicuszorg.patientregistratie.persoonsgegevens.model.Geslacht;

import org.apache.wicket.Component;
import org.apache.wicket.MarkupContainer;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormComponentUpdatingBehavior;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.form.AbstractSingleSelectChoice;
import org.apache.wicket.markup.html.form.ChoiceRenderer;
import org.apache.wicket.markup.html.form.EnumChoiceRenderer;
import org.apache.wicket.markup.html.form.FormComponent;
import org.apache.wicket.markup.html.panel.Fragment;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.PropertyModel;

import static nl.rivm.screenit.main.web.gebruiker.algemeen.documenttemplatetesten.DocumentTemplateTestenFieldsPanel.getTextAreaWithStringValidator;

public class ClientFieldsFragment extends Fragment
{

	private static final String MARKUP_ID = "fragmentClientFields";

	public ClientFieldsFragment(final String id,
		final MarkupContainer markupProvider,
		final IModel<DocumentTemplateTestWrapper> wrapper)
	{
		super(id, MARKUP_ID, markupProvider, new CompoundPropertyModel<>(new PropertyModel<>(wrapper, "client.persoon")));
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();
		add(getTitelDropDown(getTitels()));

		FormComponent<String> bsnField = ComponentHelper.addTextField(this, "bsn", true, 9, false);
		add(getBSNGenererenLink(bsnField));

		ComponentHelper.addTextField(this, "gbaAdres.straat", false, 43, false);
		ComponentHelper.addTextField(this, "gbaAdres.huisnummer", false, 10, Integer.class, false);
		ComponentHelper.addTextField(this, "gbaAdres.huisnummerToevoeging", false, 26, false);
		ComponentHelper.newPostcodeTextField(this, "gbaAdres.postcode", false, false);
		ComponentHelper.addTextField(this, "gbaAdres.plaats", false, 200, false);
		ComponentHelper.addTextField(this, "gbaAdres.huisletter", false, 200, false);
		ComponentHelper.addTextField(this, "gbaAdres.huisnummerAanduiding", false, 2, false);

		ComponentHelper.addTextField(this, "voornaam", true, 100, false);
		ComponentHelper.addTextField(this, "tussenvoegsel", false, 10, false);
		ComponentHelper.addTextField(this, "achternaam", true, 200, false);

		add(getLocatieBeschrijvingTextArea());

		add(getGeslachtDropDown(getGeslachten()));

		add(getGeboortedatumDateTextField());
	}

	private List<String> getTitels()
	{
		return Arrays.asList("Prinses", "Gravin", "Baron", "Ridder", "Jonkvrouw", "Graaf", "Jonkheer", "Barones");
	}

	private AbstractSingleSelectChoice<String> getTitelDropDown(final List<String> titels)
	{
		return DocumentTemplateTestenFieldsPanel.getScreenitDropdown("titel", titels, new ChoiceRenderer<>(), true);
	}

	private List<Geslacht> getGeslachten()
	{
		return Arrays.stream(Geslacht.values())
			.filter(geslacht -> !Geslacht.NIET_GESPECIFICEERD.equals(geslacht)
				&& !Geslacht.ONBEKEND.equals(geslacht))
			.collect(Collectors.toList());
	}

	private Component getGeboortedatumDateTextField()
	{
		return new ScreenitDateTextField("geboortedatum")
			.setRequired(true)
			.setOutputMarkupId(true)
			.add(getAjaxFormComponentUpdateBehavior());
	}

	private AjaxFormComponentUpdatingBehavior getAjaxFormComponentUpdateBehavior()
	{
		return new AjaxFormComponentUpdatingBehavior("change")
		{

			@Override
			protected void onUpdate(final AjaxRequestTarget target)
			{
				target.add(getComponent());
			}
		};
	}

	private AbstractSingleSelectChoice<Geslacht> getGeslachtDropDown(List<Geslacht> choices)
	{
		return DocumentTemplateTestenFieldsPanel.getScreenitDropdown("geslacht", choices, new EnumChoiceRenderer<>(this), true);
	}

	private FormComponent<String> getLocatieBeschrijvingTextArea()
	{
		return getTextAreaWithStringValidator("gbaAdres.locatieBeschrijving", 2048);
	}

	private IndicatingAjaxLink<Void> getBSNGenererenLink(FormComponent<String> bsnField)
	{
		return new IndicatingAjaxLink<Void>("bsnGenereren")
		{
			@Override
			public void onClick(final AjaxRequestTarget target)
			{
				GbaPersoon persoon = (GbaPersoon) ClientFieldsFragment.this.getDefaultModelObject();
				persoon.setBsn(TestBsnGenerator.getValideBsn());
				target.add(bsnField);
			}
		};
	}
}
