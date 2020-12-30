package nl.rivm.screenit.main.web.gebruiker.screening.mamma.kwaliteitscontrole.panels;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.util.stream.Collectors;

import nl.rivm.screenit.main.service.mamma.MammaFollowUpService;
import nl.rivm.screenit.main.service.mamma.MammaImsService;
import nl.rivm.screenit.main.service.mamma.MammaKwaliteitscontroleService;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.AbstractMammaRondePanel;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.MammaBeFollowUpConclusiePanel;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.MammaBeFollowUpPathologiePanel;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.MammaBeFollowUpRadiologiePanel;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.MammaBeNevenbevindingenHistoriesPanel;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.MammaHistorischeLezingenPanel;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.MammaMBBBeoordelingPanel;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.MammaOnderzoekPanel;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.MammaVisueleInspectiePanel;
import nl.rivm.screenit.model.mamma.MammaAnnotatieAfbeelding;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaFollowUpRadiologieVerslag;
import nl.rivm.screenit.model.mamma.MammaFollowUpVerslag;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus;
import nl.rivm.screenit.service.LogService;

import org.apache.wicket.Component;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.panel.EmptyPanel;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public abstract class MammaKwaliteitscontroleHuidigeRondePanel extends AbstractMammaRondePanel
{
	private static final String FOLLOW_UP_PATHOLOGIE_MARKUP_ID = "followUpPathologie";

	private static final String FOLLOW_UP_RADIOLOGIE_MARKUP_ID = "followUpRadiologie";

	private static final String FOLLOW_UP_CONCLUSIE_MARKUP_ID = "followUpConclusie";

	private List<Component> buttons = new ArrayList<>();

	@SpringBean
	MammaFollowUpService followUpService;

	@SpringBean
	private MammaImsService imsService;

	@SpringBean
	private LogService logService;

	@SpringBean
	private MammaKwaliteitscontroleService kwaliteitscontroleService;

	public MammaKwaliteitscontroleHuidigeRondePanel(String id, IModel<MammaBeoordeling> beoordelingModel)
	{
		super(id, beoordelingModel);
	}

	@Override
	protected void renderPanelComponents()
	{

		createVisueleInspectiePanel(panelContainer);
		createOnderzoekPanel(panelContainer);
		createMBBerPanel(panelContainer);

		createLezingenPanel(panelContainer);
		panelContainer.add(new MammaBeNevenbevindingenHistoriesPanel("nevenbevindingen", getModel()));
		createFollowUpPathologiePanel(panelContainer);
		createFollowUpRadiologiePanel(panelContainer);
		createFollowUpConclusiePanel(panelContainer);
		createButtons(panelContainer, buttons);
	}

	protected abstract void createButtons(WebMarkupContainer panelContainer, List<Component> buttons);

	private void createVisueleInspectiePanel(WebMarkupContainer panelContainer)
	{
		MammaVisueleInspectiePanel visueleInspectiePanel = new MammaVisueleInspectiePanel("visueleInspectiePanel", new PropertyModel<>(getModel(), "onderzoek"),
			getVisueleInspectiePanelSize());
		panelContainer.add(visueleInspectiePanel);
	}

	private void createOnderzoekPanel(WebMarkupContainer panelContainer)
	{
		panelContainer.add(new MammaOnderzoekPanel("onderzoekPanel", new PropertyModel<>(getModel(), "onderzoek")));
	}

	private void createMBBerPanel(WebMarkupContainer panelContainer)
	{
		IModel<MammaOnderzoek> onderzoekModel = new CompoundPropertyModel<>(new PropertyModel<>(getModel(), "onderzoek"));
		MammaMBBBeoordelingPanel mbberBevindingenPanel = new MammaMBBBeoordelingPanel("mbberBevindingenPanel", onderzoekModel, true);
		panelContainer.add(mbberBevindingenPanel);
	}

	private void createLezingenPanel(WebMarkupContainer panelContainer)
	{
		MammaHistorischeLezingenPanel result = new MammaHistorischeLezingenPanel("lezingenPanel", getModel());
		panelContainer.add(result);
	}

	public void blokeerButtons(AjaxRequestTarget target)
	{
		List<Component> visibleButtons = buttons.stream().filter(b -> b.isVisible()).collect(Collectors.toList());
		visibleButtons.forEach(b -> b.setEnabled(false));
		target.add(visibleButtons.toArray(new Component[] {}));
	}

	private int getVisueleInspectiePanelSize()
	{
		MammaAnnotatieAfbeelding visueleInspectieAfbeelding = getModelObject().getOnderzoek().getMammografie().getVisueleInspectieAfbeelding();
		return visueleInspectieAfbeelding == null || visueleInspectieAfbeelding.getIconen().isEmpty() ? 4 : 8;
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
