package nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.discrepantie_arbitrage;

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

import java.util.ArrayList;
import java.util.List;

import nl.rivm.screenit.main.service.mamma.MammaImsService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.AbstractMammaBeoordelenPage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.AbstractMammaRondePanel;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.werklijst.AbstractMammaBeWerklijstPage;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.enums.MammaBeLezerSoort;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class MammaDiscrepantiePage extends AbstractMammaBeoordelenPage
{

	@SpringBean
	private MammaImsService imsService;

	private MammaDiscrepantieArbitrageRondePanel huidigeRondePanel;

	public MammaDiscrepantiePage(Long initieleBeoordelingId, List<Long> beoordelingenIds, Class<? extends AbstractMammaBeWerklijstPage> werklijstPageClass)
	{
		super(initieleBeoordelingId, beoordelingenIds, werklijstPageClass);
	}

	@Override
	protected void maakRondesContainer(IModel<MammaBeoordeling> beoordelingModel)
	{
		List<AbstractMammaRondePanel> rondePanels = new ArrayList<>();
		huidigeRondePanel = new MammaDiscrepantieArbitrageRondePanel("rondeItem", beoordelingModel, getLezerSoort());
		rondePanels.add(huidigeRondePanel);

		getHistorischeRondePanels(beoordelingModel, rondePanels);
		fillRondesInContainer(rondePanels);
	}

	@Override
	protected MammaBeLezerSoort getLezerSoort()
	{
		return MammaBeLezerSoort.DISCREPANTIE_LEZER;
	}

	@Override
	protected void handleImsError(AjaxRequestTarget target, String errorMessage, Long onderzoekId)
	{
		error(imsService.handleError(errorMessage, ScreenitSession.get().getLoggedInInstellingGebruiker(), (b) -> getString((String) b), onderzoekId));
		huidigeRondePanel.blokeerOpslaan(target);
	}

}
