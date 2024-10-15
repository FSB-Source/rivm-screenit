package nl.rivm.screenit.main.web.gebruiker.algemeen.documenttemplatetesten.fragments;

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

import java.util.ArrayList;
import java.util.List;

import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.component.validator.EmailAddressValidator;
import nl.rivm.screenit.main.web.gebruiker.algemeen.documenttemplatetesten.DocumentTemplateTestWrapper;
import nl.rivm.screenit.main.web.gebruiker.algemeen.documenttemplatetesten.DocumentTemplateTestenFieldsPanel;
import nl.rivm.screenit.model.enums.MergeField;

import org.apache.wicket.MarkupContainer;
import org.apache.wicket.markup.html.form.FormComponent;
import org.apache.wicket.markup.html.panel.Fragment;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.PropertyModel;

public class IntakeLocatieFieldsFragment extends Fragment
{

	private static final String MARKUP_ID = "fragmentIntakeLocatieFields";

	private final IModel<DocumentTemplateTestWrapper> wrapper;

	public IntakeLocatieFieldsFragment(final String id,
		final MarkupContainer markupProvider,
		final IModel<DocumentTemplateTestWrapper> wrapper)
	{
		super(id, MARKUP_ID, markupProvider, new CompoundPropertyModel<>(new PropertyModel<>(wrapper, "intakeAfspraak.kamer.intakelocatie")));
		this.wrapper = wrapper;
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();

		ComponentHelper.addTextField(this, "naam", false, 50, false);

		ComponentHelper.addTextField(this, "adressen[0].straat", false, 43, false);
		ComponentHelper.addTextField(this, "adressen[0].huisnummer", false, 10, Integer.class, false);
		ComponentHelper.addTextField(this, "adressen[0].huisnummerToevoeging", false, 26, false);
		ComponentHelper.addTextField(this, "adressen[0].huisnummerAanduiding", false, 2, false);
		ComponentHelper.addTextField(this, "adressen[0].huisletter", false, 200, false);
		ComponentHelper.newPostcodeTextField(this, "adressen[0].postcode", false, false);
		ComponentHelper.addTextField(this, "adressen[0].plaats", false, 200, false);

		ComponentHelper.addTextField(this, "adressen[1].huisnummer", false, 10, Integer.class, false);
		ComponentHelper.newPostcodeTextField(this, "adressen[1].postcode", false, false);
		ComponentHelper.addTextField(this, "adressen[1].plaats", false, 200, false);

		ComponentHelper.addTextField(this, "email", false, 100, false).add(EmailAddressValidator.getInstance());
		ComponentHelper.addTextField(this, "website", false, 200, false);
		ComponentHelper.addTextField(this, "telefoon", false, 20, false);
		ComponentHelper.addTextField(this, "fax", false, 200, false);

		add(getLocatieBeschrijvingTextArea());
		add(getDigitaleIntakeTextArea());
		add(getDuurAfspraakInMinutenDropDown());
	}

	private FormComponent<String> getLocatieBeschrijvingTextArea()
	{
		return DocumentTemplateTestenFieldsPanel.getTextAreaWithStringValidator("locatieBeschrijving",
			2048, new CompoundPropertyModel<>(new PropertyModel<>(MergeField.IL_LOKATIE, "currentValue")));
	}

	private FormComponent<String> getDigitaleIntakeTextArea()
	{
		return DocumentTemplateTestenFieldsPanel.getTextAreaWithStringValidator("digitaleIntake",
			2048, new CompoundPropertyModel<>(new PropertyModel<>(MergeField.IL_DIGITALE_INTAKE, "currentValue")));
	}

	private ScreenitDropdown<Integer> getDuurAfspraakInMinutenDropDown()
	{
		return DocumentTemplateTestenFieldsPanel.getScreenitDropdown("duurAfspraakInMinuten",
			new CompoundPropertyModel<>(new PropertyModel<>(MergeField.IL_INTAKEDUUR, "currentValue")),
			getDurationIntervalList(5, 60, 5),
			false);
	}

	private static List<Integer> getDurationIntervalList(final int startInclusive,
		final int endInclusive,
		final int interval)
	{
		List<Integer> choices = new ArrayList<>();
		for (int i = startInclusive; i <= endInclusive; i = i + interval)
		{
			choices.add(i);
		}
		return choices;
	}

}
