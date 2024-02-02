package nl.rivm.screenit.main.web.gebruiker.testen.mamma.timeline.popups;

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

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.enums.MammaOnderzoekType;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaLezing;
import nl.rivm.screenit.model.mamma.enums.MammaLezingType;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.mamma.MammaBaseBeoordelingService;
import nl.rivm.screenit.util.mamma.MammaScreeningRondeUtil;

import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class TestMammaZettenNaarArbitragePopup extends TestMammaAbstractPopupPanel
{
	@SpringBean
	private MammaBaseBeoordelingService baseBeoordelingService;

	@SpringBean
	private ICurrentDateSupplier currentDateSupplier;

	public TestMammaZettenNaarArbitragePopup(String id, IModel<List<Client>> clientModel)
	{
		super(id, clientModel);
	}

	@Override
	protected void opslaan()
	{
		for (Client client : getModelObject())
		{
			MammaBeoordeling beoordeling = MammaScreeningRondeUtil.getLaatsteBeoordeling(client.getMammaDossier().getLaatsteScreeningRonde());
			MammaLezing discrepantieLezing = new MammaLezing();
			discrepantieLezing.setBeoordeling(beoordeling);
			discrepantieLezing.setBiradsOpmerking("Automatisch doorgezet vanuit test-timeline.");
			discrepantieLezing.setLezingType(MammaLezingType.DISCREPANTIE_LEZING);
			if (MammaOnderzoekType.TOMOSYNTHESE == beoordeling.getOnderzoek().getOnderzoekType())
			{
				discrepantieLezing.setTomosyntheseRelevantVoorBeoordeling(false);
			}
			baseBeoordelingService.discrepantieAfrondenEnNaarArbitrageZetten(beoordeling, discrepantieLezing);
		}
	}
}
