package nl.rivm.screenit.main.web.gebruiker.testen.colon.timeline.popups;

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

import java.util.List;

import nl.rivm.screenit.main.service.colon.ColonTestTimelineService;
import nl.rivm.screenit.main.util.EnversSwitch;
import nl.rivm.screenit.main.web.gebruiker.testen.gedeeld.timeline.popups.TestAbstractVerzetDatumPopup;
import nl.rivm.screenit.model.Client;

import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class TestColonVerzetDatumPopup extends TestAbstractVerzetDatumPopup
{

	@SpringBean
	private ColonTestTimelineService colonTestTimelineService;

	public TestColonVerzetDatumPopup(String id, IModel<List<Client>> model)
	{
		super(id, model);
	}

	@Override
	protected void opslaan()
	{
		EnversSwitch.off();
		for (Client client : getModelObject())
		{
			colonTestTimelineService.verzetDossierAchteruitInTijd(client, getAantalDagen());
		}
		EnversSwitch.on();
	}

}
