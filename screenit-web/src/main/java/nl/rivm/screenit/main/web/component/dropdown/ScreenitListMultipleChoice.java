
package nl.rivm.screenit.main.web.component.dropdown;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.Collection;
import java.util.List;

import org.apache.wicket.ajax.IAjaxIndicatorAware;
import org.apache.wicket.extensions.ajax.markup.html.AjaxIndicatorAppender;
import org.apache.wicket.markup.head.IHeaderResponse;
import org.apache.wicket.markup.head.OnDomReadyHeaderItem;
import org.apache.wicket.markup.html.form.IChoiceRenderer;
import org.apache.wicket.markup.html.form.ListMultipleChoice;
import org.apache.wicket.model.IModel;
import org.wicketstuff.wiquery.core.javascript.JsQuery;
import org.wicketstuff.wiquery.core.javascript.JsUtils;
import org.wicketstuff.wiquery.core.options.Options;

public class ScreenitListMultipleChoice<T> extends ListMultipleChoice<T>implements IAjaxIndicatorAware
{

	private static final long serialVersionUID = 1L;

	private final AjaxIndicatorAppender indicatorAppender = new AjaxIndicatorAppender();

	public ScreenitListMultipleChoice(String id, IModel<? extends Collection<T>> model, IModel<? extends List<? extends T>> choices, IChoiceRenderer<? super T> renderer)
	{
		super(id, model, choices, renderer);
		add(indicatorAppender);
	}

	public ScreenitListMultipleChoice(String id, IModel<? extends Collection<T>> model, IModel<? extends List<? extends T>> choices)
	{
		super(id, model, choices);
		add(indicatorAppender);
	}

	public ScreenitListMultipleChoice(String id, IModel<? extends Collection<T>> object, List<? extends T> choices, IChoiceRenderer<? super T> renderer)
	{
		super(id, object, choices, renderer);
		add(indicatorAppender);
	}

	public ScreenitListMultipleChoice(String id, IModel<? extends Collection<T>> object, List<? extends T> choices)
	{
		super(id, object, choices);
		add(indicatorAppender);
	}

	public ScreenitListMultipleChoice(String id, IModel<? extends List<? extends T>> choices, IChoiceRenderer<? super T> renderer)
	{
		super(id, choices, renderer);
		add(indicatorAppender);
	}

	public ScreenitListMultipleChoice(String id, IModel<? extends List<? extends T>> choices)
	{
		super(id, choices);
		add(indicatorAppender);
	}

	public ScreenitListMultipleChoice(String id, List<? extends T> choices, IChoiceRenderer<? super T> renderer)
	{
		super(id, choices, renderer);
		add(indicatorAppender);
	}

	public ScreenitListMultipleChoice(String id, List<? extends T> choices, int maxRows)
	{
		super(id, choices, maxRows);
		add(indicatorAppender);
	}

	public ScreenitListMultipleChoice(String id, List<? extends T> choices)
	{
		super(id, choices);
		add(indicatorAppender);
	}

	public ScreenitListMultipleChoice(String id)
	{
		super(id);
		add(indicatorAppender);
	}

	@Override
	public void renderHead(IHeaderResponse response)
	{
		super.renderHead(response);

		Options select2Options = new Options();
		select2Options.put("placeholder", JsUtils.quotes("- Maak een keuze -"));
		select2Options.put("formatNoMatches", "function () { return \"Geen resultaten gevonden.\"; }");
		select2Options.put("formatInputTooShort", "function (input, min) { return \"Vul a.u.b. meer dan \" + (min - input.length) + \" karakters in.\"; }");
		select2Options.put("formatSelectionTooBig", "function (limit) { return \"U kunt maximaal \" + limit + \" items selecteren\"; }");
		select2Options.put("formatLoadMore", "function (pageNumber) { return \"Meer resultaten laden...\"; }");
		select2Options.put("formatSearching", "function () { return \"Zoeken...\"; }");
		select2Options.put("minimumResultsForSearch", 0);
		select2Options.put("minimumInputLength", 0);
		select2Options.put("maximumSelectionSize", 0);
		select2Options.put("openOnEnter", true);

		response.render(OnDomReadyHeaderItem.forScript(new JsQuery(this).$().chain("select2", select2Options.getJavaScriptOptions()).render()));
	}

	@Override
	public String getAjaxIndicatorMarkupId()
	{
		return indicatorAppender.getMarkupId();
	}
}
