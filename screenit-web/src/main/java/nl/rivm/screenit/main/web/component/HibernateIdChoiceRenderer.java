package nl.rivm.screenit.main.web.component;

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

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import nl.topicuszorg.hibernate.object.model.HibernateObject;
import org.apache.wicket.core.util.lang.PropertyResolver;
import org.apache.wicket.markup.html.form.IChoiceRenderer;
import org.apache.wicket.model.IModel;

public class HibernateIdChoiceRenderer implements IChoiceRenderer<Long>
{

	private static final long serialVersionUID = 1L;

	private Map<Long, String> idLabelHashmap;

	public HibernateIdChoiceRenderer(List<? extends HibernateObject> objecten, String labelProperty)
	{
		idLabelHashmap = new HashMap<>();
		objecten.forEach(o -> idLabelHashmap.put((Long)o.getId(), PropertyResolver.getValue(labelProperty, o).toString()));
	}

	@Override
	public Object getDisplayValue(Long object)
	{
		return this.idLabelHashmap.get(object);
	}

	@Override
	public String getIdValue(Long object, int index)
	{
		return object.toString();
	}

	@Override
	public Long getObject(String id, IModel<? extends List<? extends Long>> choices)
	{
		if (id != null)
		{
			return choices.getObject().stream().filter(o -> o.toString().equals(id)).findFirst().orElse(null);
		}
		return null;
	}

}
