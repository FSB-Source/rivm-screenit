
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

import nl.topicuszorg.util.postcode.PostcodeFormatter;

import org.apache.wicket.markup.ComponentTag;
import org.apache.wicket.markup.MarkupStream;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.model.IModel;

public class PostcodeLabel extends Label
{

	private static final long serialVersionUID = 1L;

	private final boolean metSpatie;

	public PostcodeLabel(String id, boolean metSpatie)
	{
		this(id, metSpatie, null);
	}

	public PostcodeLabel(final String id, boolean metSpatie, IModel<?> model)
	{
		super(id, model);
		this.metSpatie = metSpatie;
	}

	@Override
	public void onComponentTagBody(final MarkupStream markupStream, final ComponentTag openTag)
	{
		String postcode = getDefaultModelObjectAsString();
		if (postcode != null)
		{
			postcode = PostcodeFormatter.formatPostcode(postcode, metSpatie);
		}
		replaceComponentTagBody(markupStream, openTag, postcode);
	}

}
