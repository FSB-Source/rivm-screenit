package nl.rivm.screenit.main.web.gebruiker.screening.mamma.ce.popups;

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

import nl.rivm.screenit.main.web.gebruiker.screening.mamma.ce.panels.CeRadioloogZoekPanel;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.service.mamma.MammaBaseBeoordelingService;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public abstract class MammaCeKoppelRadioloogAanBeoordelingPopup extends GenericPanel<MammaBeoordeling>
{

	@SpringBean
	private MammaBaseBeoordelingService beoordelingService;

	public MammaCeKoppelRadioloogAanBeoordelingPopup(String id, IModel<MammaBeoordeling> model)
	{
		super(id, model);
		CeRadioloogZoekPanel radioloogZoekPanel = new CeRadioloogZoekPanel("zoekPanel", getModel())
		{
			@Override
			public void callback(AjaxRequestTarget target, IModel<InstellingGebruiker> radioloog)
			{
				MammaBeoordeling beoordeling = MammaCeKoppelRadioloogAanBeoordelingPopup.this.getModelObject();
				beoordelingService.wijsBeoordelingAanRadioloogToe(beoordeling, radioloog.getObject());
				info(getString("info.radioloogGekoppeld"));
				close(target);
			}
		};
		add(radioloogZoekPanel);
	}

	public abstract void close(AjaxRequestTarget target);
}
