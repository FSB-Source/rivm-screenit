package nl.rivm.screenit.main.web.component;

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
import org.apache.wicket.model.IModel;

public class BooleanChoiceRenderer implements IChoiceRenderer<Boolean>
{
	@Override
	public Object getDisplayValue(Boolean object)
	{
		return Boolean.TRUE.equals(object) ? "Ja" : Boolean.FALSE.equals(object) ? "Nee" : "Alle";
	}

	@Override
	public String getIdValue(Boolean object, int index)
	{
		return null;
	}

	@Override
	public Boolean getObject(String id, IModel<? extends List<? extends Boolean>> choices)
	{
		return null;
	}
}
