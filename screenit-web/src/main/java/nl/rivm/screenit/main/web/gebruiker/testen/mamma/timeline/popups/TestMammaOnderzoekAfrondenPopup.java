package nl.rivm.screenit.main.web.gebruiker.testen.mamma.timeline.popups;

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
import java.util.List;

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.gebruiker.testen.mamma.timeline.MammaTestTimelinePage;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.mamma.enums.OnderbrokenOnderzoekOption;
import nl.rivm.screenit.model.mamma.enums.OnvolledigOnderzoekOption;
import nl.rivm.screenit.util.mamma.MammaScreeningRondeUtil;

import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;

public class TestMammaOnderzoekAfrondenPopup extends TestMammaAbstractPopupPanel
{
	private IModel<OnvolledigOnderzoekOption> onvolledigModel = Model.of();

	private IModel<OnderbrokenOnderzoekOption> onderbrokenModel = Model.of();

	public TestMammaOnderzoekAfrondenPopup(String id, IModel<List<Client>> clientModel)
	{
		super(id, clientModel);

		ComponentHelper.addDropDownChoice(this, "onvolledig", false, Arrays.asList(OnvolledigOnderzoekOption.values()), false).setModel(onvolledigModel);
		ComponentHelper.addDropDownChoice(this, "onderbroken", false, Arrays.asList(OnderbrokenOnderzoekOption.values()), false).setModel(onderbrokenModel);

	}

	@Override
	protected void opslaan()
	{
		if (onvolledigModel.getObject() != null && onderbrokenModel.getObject() != null)
		{
			error(getString("onvolledig.onderbroken.niet.tegelijk"));
			return;
		}
		for (Client client : getModelObject())
		{
			InstellingGebruiker instellingGebruiker = ScreenitSession.get().getLoggedInInstellingGebruiker();
			boolean versturenHl7Berichten = ((MammaTestTimelinePage) getPage()).getVerstuurHl7Berichten().getObject();
			testTimelineService.rondOnderzoekAf(MammaScreeningRondeUtil.getLaatsteAfspraak(client.getMammaDossier().getLaatsteScreeningRonde()), instellingGebruiker,
				versturenHl7Berichten, onvolledigModel.getObject(), onderbrokenModel.getObject());
		}

	}

	@Override
	protected void detachModel()
	{
		super.detachModel();
	}
}
