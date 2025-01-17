package nl.rivm.screenit.main.web.gebruiker.testen.colon.timeline.popups;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.util.List;

import nl.rivm.screenit.main.service.colon.ColonTestTimelineService;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.gebruiker.testen.gedeeld.timeline.popups.AbstractTestBasePopupPanel;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.colon.ColonIntakelocatie;
import nl.rivm.screenit.service.InstellingService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.markup.html.form.ChoiceRenderer;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class TestIntakeAfspraakPopup extends AbstractTestBasePopupPanel
{
	@SpringBean
	private ColonTestTimelineService colonTestTimelineService;

	@SpringBean
	private InstellingService instellingService;

	private final IModel<ColonIntakelocatie> intakeLocatieModel;

	public TestIntakeAfspraakPopup(String id, IModel<List<Client>> clientModel)
	{
		super(id, clientModel);

		List<ColonIntakelocatie> actieveIntakeLocatiesMetActieveKamer = getAlleIntakesLocatiesMetTenminste1ActieveKamer();
		intakeLocatieModel = ModelUtil.sModel(actieveIntakeLocatiesMetActieveKamer.get(0));

		ScreenitDropdown<ColonIntakelocatie> intakeLocatieDropDown = new ScreenitDropdown<ColonIntakelocatie>("intakeOrganisatie", intakeLocatieModel,
			ModelUtil.listRModel(actieveIntakeLocatiesMetActieveKamer), new ChoiceRenderer<ColonIntakelocatie>("naam"));
		add(intakeLocatieDropDown);

	}

	@Override
	protected void opslaan()
	{
		ColonIntakelocatie locatie = intakeLocatieModel.getObject();
		for (Client client : getModelObject())
		{
			colonTestTimelineService.maaktIntakeAfspraakVoorClient(client, locatie);
		}
	}

	private List<ColonIntakelocatie> getAlleIntakesLocatiesMetTenminste1ActieveKamer()
	{
		List<ColonIntakelocatie> actieveIntakeLocatiesMetActieveKamer = new ArrayList<ColonIntakelocatie>();
		List<ColonIntakelocatie> actieveIntakeLocaties = instellingService.getActieveIntakelocaties();
		for (var intakelocatie : actieveIntakeLocaties)
		{
			for (var kamer : intakelocatie.getKamers())
			{
				if (kamer.getActief())
				{
					actieveIntakeLocatiesMetActieveKamer.add(intakelocatie);
					break;
				}
			}
		}
		return actieveIntakeLocatiesMetActieveKamer;
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(intakeLocatieModel);
	}
}
