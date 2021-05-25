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

import nl.rivm.screenit.main.service.mamma.MammaBeoordelingService;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.MammaReadOnlyLezingPanel;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaLezing;
import nl.rivm.screenit.model.mamma.enums.MammaLezingType;

import org.apache.wicket.ajax.AjaxEventBehavior;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class MammaVerslagKiesUitgangssituatiePanel extends GenericPanel<MammaBeoordeling>
{

	@SpringBean
	private MammaBeoordelingService beoordelingService;

	public MammaVerslagKiesUitgangssituatiePanel(String id, IModel<MammaBeoordeling> model)
	{
		super(id, model);
		MammaBeoordeling beoordeling = model.getObject();
		MammaLezing[] lezingen = beoordelingService.getLezingenVoorVerslag(beoordeling);

		add(maakLezingPanel(model, "eersteVerwijsLezing", lezingen[0]));
		add(maakLezingPanel(model, "tweedeVerwijsLezing", lezingen[1]));
	}

	private MammaReadOnlyLezingPanel maakLezingPanel(IModel<MammaBeoordeling> model, String id, MammaLezing lezing)
	{
		final MammaReadOnlyLezingPanel result = new MammaReadOnlyLezingPanel(id, model.getObject(), lezing, false, false);
		result.add(createClickEvent(result));
		return result;
	}

	private AjaxEventBehavior createClickEvent(MammaReadOnlyLezingPanel result)
	{
		return new AjaxEventBehavior("click")
		{
			@Override
			protected void onEvent(AjaxRequestTarget target)
			{
				MammaVerslagRondePanel verslagRondePanel = result.findMammaVerslagPanel();
				MammaVerslagVerfijnenPanel verslagVerfijnenPanel = new MammaVerslagVerfijnenPanel(verslagRondePanel, "verslagPanel", result.maakVerslagLezing(),
					getModelObject().getOnderzoek().getAmputatie());
				verslagVerfijnenPanel.setOutputMarkupId(true);
				verslagRondePanel.replaceRonde(target, verslagVerfijnenPanel);
			}
		};
	}
}
