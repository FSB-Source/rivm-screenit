
package nl.rivm.screenit.main.web.component.table;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.util.NaamUtil;
import nl.topicuszorg.hibernate.object.model.HibernateObject;

import org.apache.commons.lang3.StringUtils;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;

public class ClientColumn<T extends HibernateObject> extends PropertyColumn<T, String>
{
	private static final long serialVersionUID = 1L;

	public ClientColumn(String propertyExpression)
	{
		this(null, propertyExpression);
	}

	public ClientColumn(String sortProperty, String propertyExpression)
	{
		super(Model.of("Cli\u00EBnt"), sortProperty, propertyExpression);
	}

	@Override
	public String getCssClass()
	{
		String cssClass = super.getCssClass();
		if (StringUtils.isNotBlank(cssClass))
		{
			cssClass += " span4";
		}
		else
		{
			cssClass = "span4";
		}
		return cssClass;
	}

	@SuppressWarnings({ "rawtypes", "unchecked" })
	@Override
	public IModel<Object> getDataModel(IModel<T> rowModel)
	{
		Client client = (Client) super.getDataModel(rowModel).getObject();
		return new Model(NaamUtil.titelVoorlettersTussenvoegselEnAanspreekAchternaam(client));
	}
}
