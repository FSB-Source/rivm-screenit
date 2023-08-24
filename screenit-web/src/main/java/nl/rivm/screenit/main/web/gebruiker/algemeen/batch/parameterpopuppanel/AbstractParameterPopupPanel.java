package nl.rivm.screenit.main.web.gebruiker.algemeen.batch.parameterpopuppanel;

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

import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;

public abstract class AbstractParameterPopupPanel<T> extends GenericPanel<T> implements BatchParameterPopupPanel
{
	private final ObjectMapper objectMapper = new ObjectMapper();

	protected AbstractParameterPopupPanel(String id)
	{
		super(id);
	}

	protected AbstractParameterPopupPanel(String id, IModel<T> model)
	{
		super(id, model);
	}

	protected String getSerializedDefaultModelObject()
	{
		try
		{
			return objectMapper.writeValueAsString(getDefaultModelObject());
		}
		catch (JsonProcessingException e)
		{
			throw new IllegalStateException(e);
		}

	}

}
