
package nl.rivm.screenit.main.web.gebruiker.clienten.contact;

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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import nl.rivm.screenit.model.enums.ExtraOpslaanKey;
import nl.topicuszorg.hibernate.object.model.HibernateObject;

import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;

public abstract class AbstractClientContactActiePanel<T extends HibernateObject> extends GenericPanel<T>
{

	private static final long serialVersionUID = 1L;

	public AbstractClientContactActiePanel(String id, IModel<T> model)
	{
		super(id, model);
	}

	public Map<ExtraOpslaanKey, Object> getOpslaanObjecten()
	{
		return new HashMap<>();
	}

	public List<String> getOpslaanMeldingen()
	{
		return new ArrayList<>();
	}

	public void validate()
	{

	}
}
