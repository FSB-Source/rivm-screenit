package nl.rivm.screenit.main.web.gebruiker.algemeen.documenttemplatetesten.fragments;

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

import java.util.Date;

import nl.topicuszorg.wicket.extensions.helpers.DatePickerHelper;

import org.apache.wicket.MarkupContainer;
import org.apache.wicket.model.IModel;
import org.wicketstuff.wiquery.ui.datepicker.DatePicker;

public class DatePickerFragment extends DocumentTemplateTestenFieldsPanelComponentFragment<Date>
{

	private static final String MARKUP_ID = "fragmentDatePicker";

	public DatePickerFragment(final String id,
		final MarkupContainer markupProvider,
		final IModel<Date> model)
	{
		this(id, markupProvider, model, null);
	}

	public DatePickerFragment(final String id,
		final MarkupContainer markupProvider,
		final IModel<Date> model,
		final IModel<Boolean> enabled)
	{
		super(id, MARKUP_ID, markupProvider, model, enabled);
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();
		add(DatePickerHelper.newDatePicker("formComponent", getModel())
			.setShowOn(DatePicker.ShowOnEnum.FOCUS));
	}

}
