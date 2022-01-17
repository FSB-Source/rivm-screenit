package nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.discrepantie_arbitrage;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.MammaOnderzoekPanel;
import nl.rivm.screenit.util.EnumStringUtil;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.AbstractMammaBeoordelenPage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.AbstractMammaRondePanel;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.MammaMBBBeoordelingPanel;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.MammaVisueleInspectiePanel;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.dto.LaesieDto;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.dto.LaesieDtoMapper;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaLezing;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;
import nl.rivm.screenit.model.mamma.enums.MammaBeLezerSoort;
import nl.rivm.screenit.service.mamma.MammaBaseBeoordelingService;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class MammaDiscrepantieArbitrageRondePanel extends AbstractMammaRondePanel
{

	@SpringBean
	private MammaBaseBeoordelingService baseBeoordelingService;

	private MammaBeLezerSoort lezerSoort;

	private DiscrepantieArbitrageLezingenContainer biradsPanel;

	MammaDiscrepantieArbitrageRondePanel(String id, IModel<MammaBeoordeling> model, MammaBeLezerSoort lezerSoort)
	{
		super(id, model);
		this.lezerSoort = lezerSoort;
	}

	@Override
	protected void renderPanelComponents()
	{
		createDiscrepantiePanel(panelContainer);
		createVisueleInspectiePanel(panelContainer);
		createOnderzoekPanel(panelContainer);
		createMBBerPanel(panelContainer);
	}

	private void createDiscrepantiePanel(WebMarkupContainer panelContainer)
	{
		biradsPanel = new DiscrepantieArbitrageLezingenContainer("lezingenContainer", getModel(), this);
		biradsPanel.setOutputMarkupId(true);
		panelContainer.add(biradsPanel);
	}

	private void createMBBerPanel(WebMarkupContainer panelContainer)
	{
		IModel<MammaOnderzoek> onderzoekModel = new CompoundPropertyModel<>(new PropertyModel<>(getModel(), "onderzoek"));
		MammaMBBBeoordelingPanel mammaMBBBeoordelingPanel = new MammaMBBBeoordelingPanel("mbberBevindingenPanel", onderzoekModel, true);

		panelContainer.add(mammaMBBBeoordelingPanel);
	}

	private void createVisueleInspectiePanel(WebMarkupContainer panelContainer)
	{
		IModel<MammaOnderzoek> onderzoekModel = new CompoundPropertyModel<>(new PropertyModel<>(getModel(), "onderzoek"));
		MammaVisueleInspectiePanel visueleInspectiePanel = new MammaVisueleInspectiePanel("visueleInspectiePanel", onderzoekModel);
		panelContainer.add(visueleInspectiePanel);
	}

	private void createOnderzoekPanel(WebMarkupContainer panelContainer)
	{
		panelContainer.add(new MammaOnderzoekPanel("onderzoekPanel", new PropertyModel<>(getModel(), "onderzoek")));
	}

	void lezingOpslaan(MammaLezing lezing, AjaxRequestTarget target, List<LaesieDto> laesieDtos)
	{
		koppelNieuweLaesiesAanLezing(lezing, laesieDtos);
		baseBeoordelingService.slaLezingOpEnVerwerkStatus(getModelObject(), lezing, ScreenitSession.get().getLoggedInInstellingGebruiker(),
			b -> getString(EnumStringUtil.getPropertyString(((MammaBeoordeling) b).getOpschortReden())));
		((AbstractMammaBeoordelenPage) getPage()).volgendeVerslag(target);
	}

	private void koppelNieuweLaesiesAanLezing(MammaLezing lezing, List<LaesieDto> laesieDtos)
	{
		LaesieDtoMapper mapper = new LaesieDtoMapper();
		mapper.koppelNieuweLaesiesAanLezing(mapper.laesieDtosToMammaLaesies(laesieDtos), lezing);
	}

	void beoordelingNaarArbitrageEnOpslaan(MammaLezing discrepantieLezing, AjaxRequestTarget target)
	{
		baseBeoordelingService.discrepantieAfrondenEnNaarArbitrageZetten(getModelObject(), discrepantieLezing);
		((AbstractMammaBeoordelenPage) getPage()).volgendeVerslag(target);
	}

	MammaBeLezerSoort getLezerSoort()
	{
		return lezerSoort;
	}

	public void blokkeerOpslaan(AjaxRequestTarget target)
	{
		biradsPanel.blokkeerLezing(target);
	}

}
