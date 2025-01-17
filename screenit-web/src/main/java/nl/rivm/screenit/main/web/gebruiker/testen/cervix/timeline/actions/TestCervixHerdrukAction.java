package nl.rivm.screenit.main.web.gebruiker.testen.cervix.timeline.actions;

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

import nl.rivm.screenit.main.service.cervix.CervixTestTimelineService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.gebruiker.testen.gedeeld.timeline.TestCervixVervolgKeuzeAction;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.service.cervix.CervixBaseTestTimelineService;

import java.util.List;

public class TestCervixHerdrukAction extends TestCervixVervolgKeuzeAction
{

	public TestCervixHerdrukAction(List<Client> clienten, CervixTestTimelineService cervixTestTimelineService, CervixBaseTestTimelineService cervixBaseTestTimelineService)
	{
		super(clienten, cervixTestTimelineService, cervixBaseTestTimelineService);
	}

	@Override
	public void execute()
	{
		for (Client client : clienten)
		{
			cervixTestTimelineService.herdruk(client.getCervixDossier().getLaatsteScreeningRonde(), ScreenitSession.get().getLoggedInAccount());
		}
	}
}
