package nl.rivm.screenit.main.web.component;

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

import nl.rivm.screenit.model.INaam;
import nl.topicuszorg.hibernate.object.model.HibernateObject;

import org.apache.wicket.markup.html.form.IChoiceRenderer;
import org.apache.wicket.model.IModel;

public class NaamChoiceRenderer<T extends INaam> implements IChoiceRenderer<T>
{

	private static final long serialVersionUID = 1L;

	@Override
	public Object getDisplayValue(INaam object)
	{
		String displayValue = "";
		if (object != null)
		{
			displayValue = object.getNaam();
		}
		return displayValue;
	}

	@Override
	public String getIdValue(INaam object, int index)
	{
		String idValue = "";
		if (object instanceof Enum<?>)
		{
			Enum<?> naamEnum = (Enum<?>) object;
			idValue = naamEnum.name();
		}
		else if (object instanceof HibernateObject)
		{
			HibernateObject hibernateObject = (HibernateObject) object;
			if (hibernateObject.getId() != null)
			{
				idValue = hibernateObject.getId().toString();
			}
			else
			{
				idValue = object.getNaam();
			}
		}
		return idValue;
	}

	@Override
	public T getObject(String id, IModel<? extends List<? extends T>> choices)
	{
		INaam first = null;
		if (choices.getObject() != null && choices.getObject().size() > 0)
		{
			first = choices.getObject().get(0);
		}
		if (first != null)
		{
			if (first instanceof Enum<?>)
			{
				return choices.getObject().stream().filter(o -> ((Enum<?>) o).name().equals(id)).findFirst().orElse(null);
			}
			else if (hasHibernateObjectId(first))
			{
				return choices.getObject().stream().filter(o -> ((HibernateObject) o).getId().toString().equals(id)).findFirst().orElse(null);
			}
			else
			{
				return choices.getObject().stream().filter(o -> o.getNaam().equals(id)).findFirst().orElse(null);
			}
		}
		return null;
	}

	private boolean hasHibernateObjectId(INaam object)
	{
		return object instanceof HibernateObject && ((HibernateObject) object).getId() != null;
	}
}
