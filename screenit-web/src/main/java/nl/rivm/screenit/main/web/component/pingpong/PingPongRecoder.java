
package nl.rivm.screenit.main.web.component.pingpong;

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

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.apache.wicket.WicketRuntimeException;
import org.apache.wicket.markup.html.form.HiddenField;
import org.apache.wicket.markup.html.form.IChoiceRenderer;
import org.apache.wicket.model.Model;
import org.apache.wicket.util.string.Strings;

public class PingPongRecoder<T> extends HiddenField<Object>
{

	private static final long serialVersionUID = 1L;

	private boolean attached;

	private final PingPongInput<T> pingPongInput;

	private final List<String> selectedIds;

	public PingPongRecoder(String id, PingPongInput<T> pingPongInput)
	{
		super(id);
		this.pingPongInput = pingPongInput;
		this.selectedIds = new ArrayList<String>();
		setDefaultModel(new Model<Serializable>());
		setOutputMarkupId(true);
	}

	@Override
	protected void onBeforeRender()
	{
		super.onBeforeRender();

		if (!getForm().hasError())
		{
			initIds();
		}
		attached = true;
	}

	private void initIds()
	{

		IChoiceRenderer<T> renderer = pingPongInput.getChoiceRenderer();
		StringBuilder modelStringBuffer = new StringBuilder();
		Collection<T> modelCollection = pingPongInput.getModelCollection();
		if (modelCollection == null)
		{
			throw new WicketRuntimeException(
				"Expected getPalette().getModelCollection() to return a non-null value." + " Please make sure you have model object assigned to the palette");
		}
		Iterator<T> selection = modelCollection.iterator();

		int i = 0;
		while (selection.hasNext())
		{
			modelStringBuffer.append(renderer.getIdValue(selection.next(), i++));
			if (selection.hasNext())
			{
				modelStringBuffer.append(",");
			}
		}

		String modelString = modelStringBuffer.toString();
		setDefaultModel(new Model<String>(modelString));
		updateIds(modelString);
	}

	@Override
	protected void onValid()
	{
		super.onValid();
		if (attached)
		{
			updateIds();
		}
	}

	protected List<T> getSelectedList()
	{
		if (getSelectedIds().isEmpty())
		{
			return Collections.emptyList();
		}

		final IChoiceRenderer<T> renderer = pingPongInput.getChoiceRenderer();
		final List<T> selected = new ArrayList<T>(getSelectedIds().size());
		final Collection<? extends T> choices = pingPongInput.getChoices().getObject();
		final Map<T, String> idForChoice = new HashMap<T, String>(choices.size());

		for (final T choice : choices)
		{
			idForChoice.put(choice, renderer.getIdValue(choice, 0));
		}

		for (final String id : getSelectedIds())
		{
			for (final T choice : choices)
			{
				final String idValue = idForChoice.get(choice);
				if (id.equals(idValue)) 
				{
					selected.add(choice);
					break;
				}
			}
		}
		return selected;
	}

	public Iterator<T> getSelectedChoices()
	{
		return getSelectedList().iterator();
	}

	protected List<T> getUnselectedList()
	{
		final Collection<? extends T> choices = pingPongInput.getChoices().getObject();

		if (choices.size() - getSelectedIds().size() == 0)
		{
			return Collections.<T> emptyList();
		}

		final IChoiceRenderer<T> renderer = pingPongInput.getChoiceRenderer();
		final List<T> unselected = new ArrayList<T>(Math.max(1, choices.size() - getSelectedIds().size()));

		for (final T choice : choices)
		{
			final String choiceId = renderer.getIdValue(choice, 0);

			if (!getSelectedIds().contains(choiceId))
			{
				unselected.add(choice);
			}
		}
		return unselected;
	}

	public Iterator<T> getUnselectedChoices()
	{
		return getUnselectedList().iterator();
	}

	@Override
	protected void onInvalid()
	{
		super.onInvalid();
		if (attached)
		{
			updateIds();
		}
	}

	private void updateIds()
	{
		updateIds(getValue());
	}

	protected void updateIds(final String value)
	{
		getSelectedIds().clear();

		if (!Strings.isEmpty(value))
		{
			for (final String id : Strings.split(value, ','))
			{
				getSelectedIds().add(id);
			}
		}
	}

	protected List<String> getSelectedIds()
	{
		return selectedIds;
	}
}
