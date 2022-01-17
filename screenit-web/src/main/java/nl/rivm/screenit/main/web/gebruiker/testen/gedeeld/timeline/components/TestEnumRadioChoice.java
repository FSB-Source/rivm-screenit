package nl.rivm.screenit.main.web.gebruiker.testen.gedeeld.timeline.components;

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

import java.util.List;

import org.apache.wicket.markup.html.form.IChoiceRenderer;
import org.apache.wicket.markup.html.form.RadioChoice;
import org.apache.wicket.model.IModel;

public class TestEnumRadioChoice<E extends Enum<E>> extends RadioChoice<E>
{

	private static final long serialVersionUID = 1L;

	public TestEnumRadioChoice(String id, IModel<E> model, List<E> values, IChoiceRenderer<E> renderer)
	{
		super(id, model, values, renderer);
		setMarkupId(id);
	}

	public TestEnumRadioChoice(String id, IModel<E> model, List<E> values)
	{
		super(id, model, values);
		setMarkupId(id);
	}

	public TestEnumRadioChoice(String id, List<E> values, IChoiceRenderer<E> renderer)
	{
		super(id, values);
		setMarkupId(id);
	}

}
