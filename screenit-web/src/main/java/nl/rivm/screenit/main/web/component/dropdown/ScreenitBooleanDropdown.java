package nl.rivm.screenit.main.web.component.dropdown;

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

import java.util.Arrays;

import org.apache.commons.lang.BooleanUtils;
import org.apache.wicket.markup.html.form.ChoiceRenderer;

public class ScreenitBooleanDropdown extends ScreenitDropdown<Boolean>
{
	public ScreenitBooleanDropdown(String id, boolean required, boolean inzien)
	{
		this(id, "Ja", "Nee", required, inzien);
	}

	public ScreenitBooleanDropdown(String id, final String trueText, final String falseText, boolean required, boolean inzien)
	{
		super(id, Arrays.asList(Boolean.TRUE, Boolean.FALSE));
		if (inzien)
		{
			setEnabled(false);
		}
		setRequired(required);
		setChoiceRenderer(new ChoiceRenderer<Boolean>()
		{
			@Override
			public String getIdValue(Boolean object, int index)
			{
				if (BooleanUtils.isTrue(object))
				{
					return trueText;
				}
				else
				{
					return falseText;
				}
			}

			@Override
			public Object getDisplayValue(Boolean object)
			{
				if (BooleanUtils.isTrue(object))
				{
					return trueText;
				}
				else
				{
					return falseText;
				}
			}
		});
	}
}
