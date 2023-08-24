package nl.rivm.screenit.main.web.gebruiker.screening.cervix.labformulier.werklijst;

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

import java.util.Arrays;
import java.util.Date;

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.AjaxButtonGroup;
import nl.rivm.screenit.main.web.component.BooleanChoiceRenderer;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.ScreenitDateTextField;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitListMultipleChoice;
import nl.rivm.screenit.model.cervix.CervixLabformulierenFilter;
import nl.rivm.screenit.model.cervix.enums.CervixLabformulierStatus;
import nl.topicuszorg.wicket.input.validator.BSNValidator;
import nl.topicuszorg.wicket.input.validator.DependantDateValidator;

import org.apache.wicket.Component;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormSubmitBehavior;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.form.EnumChoiceRenderer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.util.ListModel;
import org.wicketstuff.wiquery.ui.datepicker.DatePicker;

public abstract class CervixLabformulierenFilterPanel extends GenericPanel<CervixLabformulierenFilter>
{
	public CervixLabformulierenFilterPanel(String id, CervixLabformulierStatus[] mogelijkeCervixLabformulierStatussen, CervixLabformulierenFilter labformulierFilter,
		boolean labformulierStatussenVisible, boolean scanDatumRangeVisible, boolean geboortedatumVisible, boolean minimaalTweeWaardesTekstVisible)
	{
		super(id, new CompoundPropertyModel<>(labformulierFilter));

		Form<CervixLabformulierenFilter> form = new Form<>("form");
		add(form);

		WebMarkupContainer minimaalTweeWaardesContainer = new WebMarkupContainer("minimaalTweeWaardesContainer");
		minimaalTweeWaardesContainer.setVisible(minimaalTweeWaardesTekstVisible);
		form.add(minimaalTweeWaardesContainer);

		form.add(new TextField<>("monsterId"));
		form.add(new TextField<String>("bsn").add(new BSNValidator()));
		Component geboortedatumDateField = new ScreenitDateTextField("geboortedatum")
			.setRequired(false)
			.setOutputMarkupId(true)
			.setVisible(geboortedatumVisible);
		form.add(geboortedatumDateField);

		DatePicker<Date> vanaf = ComponentHelper.newDatePicker("scanDatumVanaf");
		form.add(vanaf.setVisible(scanDatumRangeVisible));

		DatePicker<Date> totEnMet = ComponentHelper.newDatePicker("scanDatumTotEnMet");
		form.add(totEnMet.setVisible(scanDatumRangeVisible));
		form.add(new DependantDateValidator(vanaf, totEnMet, DependantDateValidator.Operator.AFTER));

		form
			.add(new ScreenitListMultipleChoice<>("labformulierStatussen", Arrays.asList(mogelijkeCervixLabformulierStatussen), new EnumChoiceRenderer<>())
				.setVisible(labformulierStatussenVisible));

		form.add(new AjaxButtonGroup<Boolean>("digitaal", new ListModel<Boolean>(Arrays.asList(Boolean.TRUE, Boolean.FALSE, null)), new BooleanChoiceRenderer())
		{
			@Override
			protected void onSelectionChanged(Boolean selection, AjaxRequestTarget target, String markupId)
			{
				super.onSelectionChanged(selection, target, markupId);
				refreshFilter(target, geboortedatumVisible, geboortedatumDateField);
			}
		});

		ScreenitSession.get().setZoekObject(CervixLabformulierenFilterPanel.class, getModel());

		form.add(new AjaxFormSubmitBehavior("change")
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				refreshFilter(target, geboortedatumVisible, geboortedatumDateField);
			}
		});
	}

	private void refreshFilter(AjaxRequestTarget target, boolean geboortedatumVisible, Component geboortedatumDateField)
	{
		if (geboortedatumVisible)
		{
			target.add(geboortedatumDateField);
		}
		target.add(zoeken(CervixLabformulierenFilterPanel.this.getModelObject()));
		ScreenitSession.get().setZoekObject(CervixLabformulierenFilterPanel.class, getModel());
	}

	protected abstract WebMarkupContainer zoeken(CervixLabformulierenFilter filter);
}
