package nl.rivm.screenit.main.web.gebruiker.testen.colon.timeline.popups;

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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import nl.rivm.screenit.main.service.colon.ColonTestTimelineService;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.gebruiker.testen.gedeeld.timeline.popups.AbstractTestBasePopupPanel;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.colon.enums.ColonConclusieType;

import org.apache.wicket.markup.html.form.EnumChoiceRenderer;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class TestIntakeAfspraakConclusiePopup extends AbstractTestBasePopupPanel
{
	private static final long serialVersionUID = 1L;

	@SpringBean
	private ColonTestTimelineService colonTestTimelineService;

	private IModel<ColonConclusieType> typeModel = Model.of(ColonConclusieType.COLOSCOPIE);

	public TestIntakeAfspraakConclusiePopup(String id, IModel<List<Client>> clientModel)
	{
		super(id, clientModel);

		List<ColonConclusieType> choices = new ArrayList<>(Arrays.asList(ColonConclusieType.values()));
		ScreenitDropdown<ColonConclusieType> conclusieDropDown = new ScreenitDropdown<>("conclusieDropDown", typeModel,
			choices, new EnumChoiceRenderer<>(this));
		add(conclusieDropDown);
	}

	@Override
	protected void opslaan()
	{
		for (Client client : getModelObject())
		{
			colonTestTimelineService.maakIntakeAfspraakConclusieVoorClient(client, typeModel.getObject());
		}
	}

}
