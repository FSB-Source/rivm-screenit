package nl.rivm.screenit.main.web.gebruiker.testen.mamma.timeline.popups;

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

import java.util.Arrays;
import java.util.List;

import nl.rivm.screenit.main.model.mamma.MammaFollowUpConclusieChoice;
import nl.rivm.screenit.main.service.mamma.MammaFollowUpService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.model.Client;

import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class TestMammaFollowUpVastleggenPopup extends TestMammaAbstractPopupPanel
{
	@SpringBean
	private MammaFollowUpService followUpService;

	private final IModel<MammaFollowUpConclusieChoice> conclusieEnumModel = new Model<>();

	public TestMammaFollowUpVastleggenPopup(String id, IModel<List<Client>> clientModel)
	{
		super(id, clientModel);
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();

		var followUpConclusieKeuzes = Arrays.asList(MammaFollowUpConclusieChoice.values());

		ComponentHelper.addDropDownChoice(this, "followUpConclusies", true, followUpConclusieKeuzes, false)
			.setModel(conclusieEnumModel);
	}

	@Override
	protected void opslaan()
	{
		for (var client : getModelObject())
		{
			var screeningRonde = client.getMammaDossier().getLaatsteScreeningRonde();
			var conclusieStatus = followUpService.bepaalFollowUpConclusie(screeningRonde, conclusieEnumModel.getObject());
			followUpService.saveFollowUpConclusieStatus(screeningRonde, conclusieStatus, ScreenitSession.get().getLoggedInAccount());
		}
	}
}
