package nl.rivm.screenit.main.web.gebruiker.testen.gedeeld.timeline;

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

import nl.rivm.screenit.main.service.mamma.MammaTestTimelineService;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.service.mamma.MammaBaseTestTimelineService;

import org.apache.wicket.model.IModel;

public abstract class TestMammaVervolgKeuzeAction extends TestVervolgKeuzeAction
{

	protected final IModel<Boolean> verstuurHl7Berichten;

	protected MammaTestTimelineService mammaTestTimelineService;

	protected MammaBaseTestTimelineService baseTestTimelineService;

	public TestMammaVervolgKeuzeAction(List<Client> clienten, MammaTestTimelineService mammaTestTimelineService, MammaBaseTestTimelineService baseTestTimelineService,
		IModel<Boolean> verstuurHl7Berichten)
	{
		super(clienten);
		this.mammaTestTimelineService = mammaTestTimelineService;
		this.baseTestTimelineService = baseTestTimelineService;
		this.verstuurHl7Berichten = verstuurHl7Berichten;
	}
}
