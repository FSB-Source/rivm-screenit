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

import nl.rivm.screenit.model.enums.BriefType;

import org.apache.wicket.markup.ComponentTag;
import org.apache.wicket.markup.MarkupStream;
import org.apache.wicket.markup.html.basic.EnumLabel;
import org.apache.wicket.model.IModel;

public class BriefTypeLabel extends EnumLabel<BriefType>
{

	public BriefTypeLabel(String id)
	{
		super(id);
	}

	public BriefTypeLabel(String id, IModel<BriefType> model)
	{
		super(id, model);
	}

	public BriefTypeLabel(String id, BriefType briefType)
	{
		super(id, briefType);
	}

	@Override
	public void onComponentTagBody(final MarkupStream markupStream, final ComponentTag openTag)
	{
		replaceComponentTagBody(markupStream, openTag, getStringValue());
	}

	private String getStringValue()
	{
		BriefType value = getModelObject();
		String converted = value != null ? getString(resourceKey(value)) : nullValue();
		if (value != null && !value.isActief())
		{
			converted += " " + getString("BriefType.niet.meer.in.gebruik");
		}
		return getDefaultModelObjectAsString(converted);
	}
}
