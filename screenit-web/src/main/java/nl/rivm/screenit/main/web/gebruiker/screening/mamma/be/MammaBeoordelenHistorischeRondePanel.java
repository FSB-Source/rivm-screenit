package nl.rivm.screenit.main.web.gebruiker.screening.mamma.be;

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

import nl.rivm.screenit.main.service.mamma.MammaFollowUpService;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaFollowUpRadiologieVerslag;
import nl.rivm.screenit.model.mamma.MammaFollowUpVerslag;
import nl.rivm.screenit.model.mamma.MammaLezing;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus;
import nl.rivm.screenit.service.mamma.MammaBaseBeoordelingService;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.panel.EmptyPanel;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class MammaBeoordelenHistorischeRondePanel extends AbstractMammaRondePanel
{
	private static final String LEZINGEN_PANEL_MARKUP_ID = "lezingenPanel";

	private static final String VISUELE_INSPECTIE_PANEL_MARKUP_ID = "visueleInspectiePanel";

	private static final String ONDERZOEK_PANEL_MARKUP_ID = "onderzoekPanel";

	private static final String MBBER_BEVINDINGEN_PANEL_MARKUP_ID = "mbberBevindingenPanel";

	private static final String NEVENBEVINDINGEN_MARKUP_ID = "nevenbevindingen";

	private static final String REDENEN_FOTOBESPREKING_RADIOLOOG_MARKUP_ID = "redenenFotobesprekingRadioloog";

	private static final String REDENEN_FOTOBESPREKING_MBBER_MARKUP_ID = "redenenFotobesprekingMbber";

	private static final String FOLLOW_UP_PATHOLOGIE_MARKUP_ID = "followUpPathologie";

	private static final String FOLLOW_UP_RADIOLOGIE_MARKUP_ID = "followUpRadiologie";

	private static final String FOLLOW_UP_CONCLUSIE_MARKUP_ID = "followUpConclusie";

	@SpringBean
	private MammaBaseBeoordelingService beoordelingService;

	@SpringBean
	private MammaFollowUpService followUpService;

	public MammaBeoordelenHistorischeRondePanel(String id, IModel<MammaBeoordeling> beoordelingModel)
	{
		super(id, beoordelingModel);
		super.setIngeklapt(true);
		setUseLazyLoading(true);
	}

	@Override
	protected void lazyLoadContent(AjaxRequestTarget target)
	{
		createPanelComponents();
		target.add(panelContainer);
	}

	private void createPanelComponents()
	{

		createVisueleInspectiePanel(panelContainer);
		createOnderzoekPanel(panelContainer);
		createMBBerPanel(panelContainer);

		createLezingenPanel(panelContainer);
		panelContainer.addOrReplace(new MammaBeNevenbevindingenHistoriesPanel(NEVENBEVINDINGEN_MARKUP_ID, getModel()));

		createRedenenFotobesprekingPanel(panelContainer);

		createFollowUpPathologiePanel(panelContainer);
		createFollowUpRadiologiePanel(panelContainer);
		createFollowUpConclusiePanel(panelContainer);

	}

	@Override
	protected void renderPanelComponents()
	{
		if (isUseLazyLoading())
		{
			Arrays
				.asList(VISUELE_INSPECTIE_PANEL_MARKUP_ID, ONDERZOEK_PANEL_MARKUP_ID, MBBER_BEVINDINGEN_PANEL_MARKUP_ID, NEVENBEVINDINGEN_MARKUP_ID, LEZINGEN_PANEL_MARKUP_ID,
					REDENEN_FOTOBESPREKING_RADIOLOOG_MARKUP_ID, REDENEN_FOTOBESPREKING_MBBER_MARKUP_ID, FOLLOW_UP_PATHOLOGIE_MARKUP_ID, FOLLOW_UP_RADIOLOGIE_MARKUP_ID,
					FOLLOW_UP_CONCLUSIE_MARKUP_ID)
				.forEach(markupId -> panelContainer.add(new EmptyPanel(markupId).setOutputMarkupId(true)));
		}
		else
		{
			createPanelComponents();
		}
	}

	private void createVisueleInspectiePanel(WebMarkupContainer panelContainer)
	{
		MammaVisueleInspectiePanel visueleInspectiePanel = new MammaVisueleInspectiePanel(VISUELE_INSPECTIE_PANEL_MARKUP_ID, new PropertyModel<>(getModel(), "onderzoek"));
		panelContainer.addOrReplace(visueleInspectiePanel);
	}

	private void createOnderzoekPanel(WebMarkupContainer panelContainer)
	{
		panelContainer.addOrReplace(new MammaOnderzoekPanel(ONDERZOEK_PANEL_MARKUP_ID, new PropertyModel<>(getModel(), "onderzoek")));
	}

	private void createMBBerPanel(WebMarkupContainer panelContainer)
	{
		IModel<MammaOnderzoek> onderzoekModel = new CompoundPropertyModel<>(new PropertyModel<>(getModel(), "onderzoek"));
		MammaMBBBeoordelingPanel mbberBevindingenPanel = new MammaMBBBeoordelingPanel(MBBER_BEVINDINGEN_PANEL_MARKUP_ID, onderzoekModel, true);
		panelContainer.addOrReplace(mbberBevindingenPanel);
	}

	private void createLezingenPanel(WebMarkupContainer panelContainer)
	{
		MammaHistorischeLezingenPanel result = new MammaHistorischeLezingenPanel(LEZINGEN_PANEL_MARKUP_ID, getModel());
		panelContainer.addOrReplace(result);
	}

	private void createRedenenFotobesprekingPanel(WebMarkupContainer panelContainer)
	{
		MammaLezing eersteLezing = getModelObject().getEersteLezing();
		MammaLezing tweedeLezing = getModelObject().getTweedeLezing();

		String redenenFotobesprekingRadioloogTekst = beoordelingService.getMammaLezingEnumsTekst(MammaLezing::getRedenenFotobesprekingRadioloog, eersteLezing, tweedeLezing);
		String redenenFotobesprekingMbberTekst = beoordelingService.getMammaLezingEnumsTekst(MammaLezing::getRedenenFotobesprekingMbber, eersteLezing, tweedeLezing);

		panelContainer.addOrReplace(
			new MammaBeRedenenFotobesprekingHistoriesPanel(REDENEN_FOTOBESPREKING_RADIOLOOG_MARKUP_ID, getModel(), "Redenen fotobespreking radioloog",
				redenenFotobesprekingRadioloogTekst));
		panelContainer.addOrReplace(
			new MammaBeRedenenFotobesprekingHistoriesPanel(REDENEN_FOTOBESPREKING_MBBER_MARKUP_ID, getModel(), "Redenen fotobespreking MBB'er", redenenFotobesprekingMbberTekst));
	}

	private void createFollowUpConclusiePanel(WebMarkupContainer panelContainer)
	{
		if (getModelObject().getStatus().equals(MammaBeoordelingStatus.UITSLAG_ONGUNSTIG)
			|| getModelObject().getOnderzoek().getAfspraak().getUitnodiging().getScreeningRonde().getFollowUpConclusieStatus() != null)
		{
			panelContainer.addOrReplace(new MammaBeFollowUpConclusiePanel(FOLLOW_UP_CONCLUSIE_MARKUP_ID, getModel()));
		}
		else
		{
			panelContainer.addOrReplace(new EmptyPanel(FOLLOW_UP_CONCLUSIE_MARKUP_ID));
		}
	}

	private void createFollowUpPathologiePanel(WebMarkupContainer panelContainer)
	{
		List<MammaFollowUpVerslag> followUpVerslagen = followUpService
			.getAfgerondeFollowUpPathologieVerslagen(getModelObject().getOnderzoek().getAfspraak().getUitnodiging().getScreeningRonde());

		if (!followUpVerslagen.isEmpty())
		{
			panelContainer.addOrReplace(new MammaBeFollowUpPathologiePanel(FOLLOW_UP_PATHOLOGIE_MARKUP_ID, getModel(), followUpVerslagen));
		}
		else
		{
			panelContainer.addOrReplace(new EmptyPanel(FOLLOW_UP_PATHOLOGIE_MARKUP_ID));
		}
	}

	private void createFollowUpRadiologiePanel(WebMarkupContainer panelContainer)
	{
		List<MammaFollowUpRadiologieVerslag> followUpVerslagen = followUpService
			.getIngevoerdeFollowUpRadiologieVerslagen(getModelObject().getOnderzoek().getAfspraak().getUitnodiging().getScreeningRonde());

		if (!followUpVerslagen.isEmpty())
		{
			panelContainer.addOrReplace(new MammaBeFollowUpRadiologiePanel(FOLLOW_UP_RADIOLOGIE_MARKUP_ID, getModel(), followUpVerslagen));
		}
		else
		{
			panelContainer.addOrReplace(new EmptyPanel(FOLLOW_UP_RADIOLOGIE_MARKUP_ID));
		}
	}
}
