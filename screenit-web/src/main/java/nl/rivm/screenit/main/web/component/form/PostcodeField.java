
package nl.rivm.screenit.main.web.component.form;

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

import nl.rivm.screenit.main.web.component.validator.PostcodeOfGetallenValidator;
import nl.topicuszorg.wicket.input.converters.PostcodeConverter;

import org.apache.wicket.Component;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.model.IModel;
import org.apache.wicket.util.convert.IConverter;
import org.apache.wicket.validation.validator.StringValidator;

public class PostcodeField extends TextField<String>
{

	private static final long serialVersionUID = 1L;

	private boolean alleenCijfersToegestaan = false;

	public PostcodeField(String id, Class<String> type)
	{
		super(id, type);
	}

	public PostcodeField(String id, IModel<String> model, Class<String> type)
	{
		super(id, model, type);
	}

	public PostcodeField(String id, IModel<String> model)
	{
		super(id, model);
	}

	public PostcodeField(String id)
	{
		super(id);
	}

	public Component setAlleenCijfersToegestaan(boolean alleenCijfersToegestaan)
	{
		this.alleenCijfersToegestaan = alleenCijfersToegestaan;
		return this;
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();
		add(new PostcodeOfGetallenValidator(alleenCijfersToegestaan));
		add(StringValidator.maximumLength(7));
		setType(String.class);
	}

	@Override
	public <C> IConverter<C> getConverter(Class<C> type)
	{
		return (IConverter<C>) new PostcodeConverter();
	}

}
