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

import java.util.List;

import nl.rivm.screenit.main.service.colon.ColonTestTimelineService;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.gebruiker.testen.gedeeld.timeline.popups.AbstractTestBasePopupPanel;
import nl.rivm.screenit.model.Client;

import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class TestEindeScreeningRondePopUp extends AbstractTestBasePopupPanel
{

	private static final long serialVersionUID = 1L;

	@SpringBean
	private ColonTestTimelineService colonTestTimelineService;

	IModel<Boolean> eindeScreeningRonde = new Model(Boolean.TRUE);

	public TestEindeScreeningRondePopUp(String id, IModel<List<Client>> clientModel)
	{
		super(id, clientModel);
		add(ComponentHelper.newCheckBox("eindeScreeningRonde", eindeScreeningRonde));
	}

	@Override
	protected void opslaan()
	{
		Boolean eindeScreeningRondeBoolean = eindeScreeningRonde.getObject();
		if (Boolean.TRUE.equals(eindeScreeningRondeBoolean))
		{
			for (Client client : getModelObject())
			{
				colonTestTimelineService.naarEindeVanRonde(client);
			}
		}
	}

}
