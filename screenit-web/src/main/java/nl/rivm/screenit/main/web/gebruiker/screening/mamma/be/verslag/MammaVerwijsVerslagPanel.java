package nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.verslag;

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

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.AbstractMammaBeoordelenPage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.panel.MammaBeoordelingPdfTonenPanel;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.mamma.MammaLezing;
import nl.rivm.screenit.model.mamma.enums.MammaAmputatie;
import nl.rivm.screenit.service.mamma.MammaBaseBeoordelingService;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxButton;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

class MammaVerwijsVerslagPanel extends GenericPanel<MammaLezing>
{
	@SpringBean
	private MammaBaseBeoordelingService baseBeoordelingService;

	MammaVerwijsVerslagPanel(String id, MammaVerslagRondePanel verslagPanel, IModel<MammaLezing> verslagLezingModel, MammaAmputatie amputatie)
	{
		super(id, verslagLezingModel);

		Form form = new Form("verwijsverslagForm");
		form.add(new MammaBeoordelingPdfTonenPanel("verslagPdf", verslagPanel.getModel()));
		form.add(new IndicatingAjaxButton("terugNaarVerslagVerfijnen")
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				MammaVerslagVerfijnenPanel verslagVerfijnenPanel = new MammaVerslagVerfijnenPanel(verslagPanel, "verslagPanel", verslagLezingModel, amputatie);
				verslagVerfijnenPanel.setOutputMarkupId(true);
				verslagPanel.replaceRonde(target, verslagVerfijnenPanel);
			}
		});

		form.add(new IndicatingAjaxButton("verstuurVerslag")
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				InstellingGebruiker beoordelaar = ScreenitSession.get().getLoggedInInstellingGebruiker();
				verslagPanel.getModelObject().getVerslagLezing().setBeoordelaar(beoordelaar);
				baseBeoordelingService.setStatusNaarVerslagGereed(verslagPanel.getModelObject());
				((AbstractMammaBeoordelenPage) getPage()).volgendeVerslag(target);
			}
		});
		add(form);
	}
}
