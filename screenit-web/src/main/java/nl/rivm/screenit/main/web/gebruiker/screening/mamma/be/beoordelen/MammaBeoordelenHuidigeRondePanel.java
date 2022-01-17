package nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.beoordelen;

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

import nl.rivm.screenit.main.service.mamma.MammaBeoordelingService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.AbstractMammaBeoordelenPage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.AbstractMammaRondePanel;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.MammaMBBBeoordelingPanel;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.MammaOnderzoekPanel;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.MammaVisueleInspectiePanel;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.dto.LaesieDto;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.dto.LaesieDtoMapper;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.MammaAnnotatieAfbeelding;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaLezing;
import nl.rivm.screenit.model.mamma.enums.MammaAmputatie;
import nl.rivm.screenit.model.mamma.enums.MammaBIRADSWaarde;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus;
import nl.rivm.screenit.service.mamma.MammaBaseBeoordelingService;
import nl.rivm.screenit.util.EnumStringUtil;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class MammaBeoordelenHuidigeRondePanel extends AbstractMammaRondePanel
{
	@SpringBean
	private MammaBaseBeoordelingService baseBeoordelingService;

	@SpringBean
	private MammaBeoordelingService beoordelingService;

	private MammaMBBBeoordelingPanel mammaMBBBeoordelingPanel;

	private boolean opslaanPopupGezien = false;

	private MammaBeoordelenLezingPanel mammaBeoordelenLezingPanel;

	private MammaVisueleInspectiePanel visueleInspectiePanel;

	public MammaBeoordelenHuidigeRondePanel(String id, IModel<MammaBeoordeling> beoordelingModel, Integer jaarLaatsteVerwijzing)
	{
		super(id, beoordelingModel, jaarLaatsteVerwijzing);
	}

	@Override
	protected void renderPanelComponents()
	{
		createMammaBeoordelenPanel(panelContainer);

		createVisueleInspectiePanel(panelContainer);
		createOnderzoekPanel(panelContainer);
		createMBBerPanel(panelContainer);
	}

	private void createOnderzoekPanel(WebMarkupContainer panelContainer)
	{
		panelContainer.add(new MammaOnderzoekPanel("onderzoekPanel", new PropertyModel<>(getModel(), "onderzoek")));
	}

	private void createMammaBeoordelenPanel(WebMarkupContainer panelContainer)
	{
		MammaLezing lezing = beoordelingService.getOrCreate1eOf2eLezing(getModelObject(),
			ScreenitSession.get().getLoggedInInstellingGebruiker(),
			isOnervarenRadioloog());
		IModel<MammaLezing> lezingModel = createModel(getModelObject(), lezing, ScreenitSession.get().getLoggedInInstellingGebruiker());
		mammaBeoordelenLezingPanel = new MammaBeoordelenLezingPanel(this, "mammaBeoordelenPanel", lezingModel);
		panelContainer.add(mammaBeoordelenLezingPanel);
	}

	public void lezingOpslaan(IModel<MammaLezing> lezingModel, AjaxRequestTarget target, List<LaesieDto> laesieDtos)
	{
		if (lezingModel.getObject().getNevenbevindingen().isEmpty())
		{
			lezingModel.getObject().setNevenbevindingOpmerking(null);
		}
		koppelNieuweLaesiesAanLezing(lezingModel, laesieDtos);
		baseBeoordelingService.slaLezingOpEnVerwerkStatus(getModelObject(), lezingModel.getObject(), ScreenitSession.get().getLoggedInInstellingGebruiker(),
			(b) -> getString(EnumStringUtil.getPropertyString(((MammaBeoordeling) b).getOpschortReden())));
		((AbstractMammaBeoordelenPage) getPage()).volgendeVerslag(target);
	}

	private void koppelNieuweLaesiesAanLezing(IModel<MammaLezing> lezingModel, List<LaesieDto> laesieDtos)
	{
		LaesieDtoMapper mapper = new LaesieDtoMapper();
		mapper.koppelNieuweLaesiesAanLezing(mapper.laesieDtosToMammaLaesies(laesieDtos), lezingModel.getObject());
	}

	public boolean isOpslaanPopupGezien()
	{
		return opslaanPopupGezien;
	}

	public void setOpslaanPopupGezien(boolean opslaanPopupGezien)
	{
		this.opslaanPopupGezien = opslaanPopupGezien;
	}

	private boolean toonMBBSignaleren()
	{
		MammaBeoordelingStatus beoordelingStatus = getModelObject().getStatus();
		return opslaanPopupGezien || (MammaBeoordelingStatus.EERSTE_LEZING_OPGESLAGEN.equals(beoordelingStatus)
			|| MammaBeoordelingStatus.TWEEDE_LEZING_OPGESLAGEN.equals(beoordelingStatus))
			|| MammaBeoordelingStatus.DISCREPANTIE.equals(beoordelingStatus)
			|| MammaBeoordelingStatus.ARBITRAGE.equals(beoordelingStatus);
	}

	private IModel<MammaLezing> createModel(MammaBeoordeling beoordeling, MammaLezing lezing, InstellingGebruiker beoordelaar)
	{

		IModel<MammaLezing> model = ModelUtil.cModel(lezing);
		if ((MammaBeoordelingStatus.EERSTE_LEZING.equals(beoordeling.getStatus()) ||
			MammaBeoordelingStatus.EERSTE_LEZING_OPGESLAGEN.equals(beoordeling.getStatus())) &&
			beoordeling.getEersteLezing() != null)
		{

		}
		else if ((MammaBeoordelingStatus.TWEEDE_LEZING.equals(beoordeling.getStatus())
			|| MammaBeoordelingStatus.TWEEDE_LEZING_OPGESLAGEN.equals(beoordeling.getStatus())) &&
			beoordeling.getTweedeLezing() != null)
		{

		}
		else
		{
			MammaLezing lezingProxy = model.getObject();

			MammaAmputatie amputatie = getModelObject().getOnderzoek().getAmputatie();
			lezingProxy.setBiradsRechts(amputatie != null && MammaAmputatie.RECHTERBORST.equals(amputatie) ? MammaBIRADSWaarde.GEEN : MammaBIRADSWaarde.EEN);
			lezingProxy.setBiradsLinks(amputatie != null && MammaAmputatie.LINKERBORST.equals(amputatie) ? MammaBIRADSWaarde.GEEN : MammaBIRADSWaarde.EEN);
			lezingProxy.setBeoordelaar(beoordelaar);
			lezingProxy.setOnervarenRadioloog(lezing.isOnervarenRadioloog());
		}
		return model;
	}

	void reloadMbberComponent(AjaxRequestTarget target)
	{
		mammaMBBBeoordelingPanel.reloadContent(target, toonMBBSignaleren());
	}

	void reloadVisueleInspectiePanel(AjaxRequestTarget target)
	{
		visueleInspectiePanel.setPanelSize(target, getVisueleInspectiePanelSize());
	}

	private boolean isOnervarenRadioloog()
	{
		return !ScreenitSession.get().checkPermission(Recht.GEBRUIKER_SCREENING_MAMMA_ARBITRAGE_WERKLIJST, Actie.TOEVOEGEN);
	}

	private void createMBBerPanel(WebMarkupContainer panelContainer)
	{
		mammaMBBBeoordelingPanel = new MammaMBBBeoordelingPanel("mbberBevindingenPanel", new CompoundPropertyModel<>(new PropertyModel<>(getModel(), "onderzoek")),
			toonMBBSignaleren());
		panelContainer.add(mammaMBBBeoordelingPanel);
	}

	private void createVisueleInspectiePanel(WebMarkupContainer panelContainer)
	{
		visueleInspectiePanel = new MammaVisueleInspectiePanel("visueleInspectiePanel", ModelUtil.cRModel(getModelObject().getOnderzoek()), getVisueleInspectiePanelSize());
		panelContainer.add(visueleInspectiePanel);
	}

	public void blokeerOpslaan(AjaxRequestTarget target)
	{
		mammaBeoordelenLezingPanel.blokeerOpslaan(target);
	}

	private int getVisueleInspectiePanelSize()
	{
		MammaAnnotatieAfbeelding visueleInspectieAfbeelding = getModelObject().getOnderzoek().getMammografie().getVisueleInspectieAfbeelding();
		return toonMBBSignaleren() || visueleInspectieAfbeelding == null || visueleInspectieAfbeelding.getIconen().isEmpty() ? 4 : 6;
	}
}
