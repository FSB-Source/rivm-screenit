package nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.verslag;

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

import java.util.List;

import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.AbstractMammaRondePanel;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.dto.LaesieDto;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.dto.LaesieDtoMapper;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaLezing;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;

public class MammaVerslagRondePanel extends AbstractMammaRondePanel
{

	private MammaVerslagVerfijnenPanel verslagVerfijnenPanel;

	private boolean verslagLezingGeblokeerd = false;

	MammaVerslagRondePanel(String id, IModel<MammaBeoordeling> model)
	{
		super(id, model, Model.of("Verslaglezing"));
	}

	@Override
	protected void renderPanelComponents()
	{
		WebMarkupContainer verslagPanel = bepaalVerslagPanel();
		panelContainer.add(verslagPanel);
	}

	private Panel bepaalVerslagPanel()
	{
		Panel verslagPanel;
		MammaBeoordeling beoordeling = getModelObject();
		if (MammaBeoordelingStatus.VERSLAG_MAKEN.equals(beoordeling.getStatus()) && beoordeling.getVerslagLezing() == null)
		{
			verslagPanel = new MammaVerslagKiesUitgangssituatiePanel("verslagPanel", getModel());
		}
		else if (beoordeling.getVerslagLezing() != null)
		{
			beoordeling.getVerslagLezing().setBeoordeling(beoordeling);
			verslagPanel = new MammaVerslagVerfijnenPanel(this, "verslagPanel", new CompoundPropertyModel<>(new PropertyModel<>(getModel(), "verslagLezing")),
				getModelObject().getOnderzoek().getAmputatie());
			verslagVerfijnenPanel = (MammaVerslagVerfijnenPanel) verslagPanel;
		}
		else
		{
			throw new IllegalStateException(beoordeling.getStatus() + " is geen geldige status");
		}
		verslagPanel.setOutputMarkupId(true);
		return verslagPanel;
	}

	void koppelNieuweLaesiesAanLezing(IModel<MammaLezing> lezingModel, List<LaesieDto> laesieDtos)
	{
		LaesieDtoMapper mapper = new LaesieDtoMapper();
		mapper.koppelNieuweLaesiesAanLezing(mapper.laesieDtosToMammaLaesies(laesieDtos), lezingModel.getObject());
	}

	public void replaceRonde(AjaxRequestTarget target, WebMarkupContainer verslagVerfijnenPanel)
	{
		panelContainer.replace(verslagVerfijnenPanel);
		target.add(verslagVerfijnenPanel);
	}

	public void blokeerOpslaan()
	{
		verslagLezingGeblokeerd = true;
		if (verslagVerfijnenPanel != null)
		{
			verslagVerfijnenPanel.blokeerOpslaan();
		}
	}

	public boolean isVerslagLezingGeblokeerd()
	{
		return verslagLezingGeblokeerd;
	}

	public void setVerslagLezingGeblokeerd(boolean verslagLezingGeblokeerd)
	{
		this.verslagLezingGeblokeerd = verslagLezingGeblokeerd;
	}
}
