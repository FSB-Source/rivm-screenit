package nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.verslag.laesies;

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

import java.util.Arrays;
import java.util.List;

import nl.rivm.screenit.main.web.component.NaamChoiceRenderer;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.dto.LaesieDto;
import nl.rivm.screenit.model.INaam;

import org.apache.wicket.markup.html.form.RadioChoice;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;

public abstract class LaesiePanel extends GenericPanel<LaesieDto>
{
	protected List<LaesieDto> alleLaesies;

	LaesiePanel(String id, IModel<LaesieDto> model, List<LaesieDto> alleLaesies)
	{
		super(id, model);
		this.alleLaesies = alleLaesies;
	}

	<E extends Enum<E> & INaam> RadioChoice<E> createSpecificatieRadioChoice(String id, Class<E> enumType)
	{
		RadioChoice<E> choice = new RadioChoice<>(id,
			Arrays.asList(enumType.getEnumConstants()),
			new NaamChoiceRenderer<>());
		choice.setRequired(true);
		choice.setPrefix("<label class=\"radio\">");
		choice.setSuffix("<span class=\"checkmark\"></span></label>");
		choice.setOutputMarkupId(true);
		return choice;
	}
}
