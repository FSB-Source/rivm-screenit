
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

import java.util.Iterator;

import nl.topicuszorg.formulieren2.wicketrenderer.containers.SamengesteldeVraagBlokPanel;
import nl.topicuszorg.formulieren2.wicketrenderer.vraag.EnkelvoudigCheckBoxKeuzeVraagPanel;

import org.apache.wicket.Component;
import org.apache.wicket.MarkupContainer;
import org.apache.wicket.markup.html.form.FormComponent;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.validation.IValidationError;
import org.apache.wicket.validation.ValidationError;

public class TextFieldNaastNullFlavourField<T> extends TextField<T>
{

	private static final long serialVersionUID = 1L;

	public TextFieldNaastNullFlavourField(String id)
	{
		this(id, null);
	}

	public TextFieldNaastNullFlavourField(String id, Class<T> type)
	{
		super(id, type);
	}

	@Override
	public void error(IValidationError error)
	{
		boolean errorDoorZetten = true;
		if (error instanceof ValidationError)
		{
			ValidationError valError = (ValidationError) error;
			SamengesteldeVraagBlokPanel samengesteldePanel = findParent(SamengesteldeVraagBlokPanel.class);
			if (samengesteldePanel != null)
			{
				Iterator<Component> iterator = ((MarkupContainer) samengesteldePanel.get("elementen")).iterator();
				while (iterator.hasNext())
				{
					Component newNext = iterator.next().get("element");
					if (newNext instanceof EnkelvoudigCheckBoxKeuzeVraagPanel)
					{
						Component checkBoxValueComp = newNext.get("value");
						if (checkBoxValueComp instanceof FormComponent)
						{
							String value = ((FormComponent) checkBoxValueComp).getValue();
							if ("on".equals(value) && valError.getKeys().contains("Required"))
							{
								errorDoorZetten = false;
							}
						}
					}
				}
			}
		}
		if (errorDoorZetten)
		{
			super.error(error);
		}
	}
}
