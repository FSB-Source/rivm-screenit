
package nl.rivm.screenit.main.web.client.dashboard.colon;

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

import nl.rivm.screenit.main.model.BaseHuisartsModel;
import nl.rivm.screenit.main.model.EnovationHuisartsModel;
import nl.rivm.screenit.service.RondeNummerService;
import nl.rivm.screenit.service.colon.ColonHuisartsService;
import nl.rivm.screenit.main.web.client.base.ClientBasePage;
import nl.rivm.screenit.main.web.client.base.ClientHoofdMenuitem;
import nl.rivm.screenit.main.web.client.dashboard.huisartswizard.ClientHuisartsBevestigenWizardPanel;
import nl.rivm.screenit.main.web.client.dashboard.huisartswizard.ClientHuisartsZoekenWizardPanel;
import nl.rivm.screenit.main.web.component.SimpleStringResourceModel;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.EnovationHuisarts;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.panel.EmptyPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.AANPASSEN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = Recht.CLIENT_GEGEVENS,
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.COLON })
public class ClientColonWijzigenHuisartsPage extends ClientBasePage
{

	private static final long serialVersionUID = 1L;

	@SpringBean
	private RondeNummerService rondeNummerService;

	@SpringBean
	private ColonHuisartsService huisartsService;

	private IModel<EnovationHuisarts> zoekModel;

	public ClientColonWijzigenHuisartsPage()
	{
		WebMarkupContainer huisartsContainer = new WebMarkupContainer("panel");
		huisartsContainer.setOutputMarkupId(true);
		ColonScreeningRonde ronde = getLaatsteRonde();
		ColonScreeningRonde vorigeRonde = rondeNummerService.getVorigeRonde(ronde);

		zoekModel = ModelUtil.cRModel(new EnovationHuisarts());

		if (vorigeRonde != null && vorigeRonde.getColonHuisarts() != null && ronde.getColonHuisarts() == null)
		{
			toonVraagOmBevestiging(huisartsContainer, vorigeRonde.getColonHuisarts());
		}
		else
		{
			toonZoeken(huisartsContainer, ronde, false);
		}

		add(huisartsContainer);
	}

	private ColonScreeningRonde getLaatsteRonde()
	{
		Client client = getIngelogdeClient();
		ColonScreeningRonde ronde = client.getColonDossier().getLaatsteScreeningRonde();
		return ronde;
	}

	private void toonZoeken(WebMarkupContainer huisartsContainer, ColonScreeningRonde laatsteRonde, boolean toonDirectZoekresultaten)
	{

		huisartsContainer.addOrReplace(new ClientHuisartsZoekenWizardPanel("wizard", zoekModel, toonDirectZoekresultaten)
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onHuisartsGekozen(AjaxRequestTarget target, EnovationHuisarts ha)
			{
				toonVraagOmBevestiging(huisartsContainer, ha);
				target.add(huisartsContainer);
			}

			@Override
			protected void onVerwijderHuidigeHuisarts(AjaxRequestTarget target)
			{
				changeHuisarts(target, null);
			}

			@Override
			protected void onStop(AjaxRequestTarget target)
			{
				ClientColonWijzigenHuisartsPage.this.onStop(target);
			}

			@Override
			protected WebMarkupContainer getHuisartsInfoComponent(String id)
			{
				BaseHuisartsModel<?> huisartsModel = getHuisartsModel();
				if (huisartsModel != null)
				{
					return super.getHuisartsInfoComponent(id);
				}
				EmptyPanel geenHuisarts = new EmptyPanel(id);
				geenHuisarts.setVisible(false);
				return geenHuisarts;
			}

			@Override
			protected BaseHuisartsModel<?> getHuisartsModel()
			{
				if (getLaatsteRonde() != null && getLaatsteRonde().getColonHuisarts() != null)
				{
					return new EnovationHuisartsModel(getLaatsteRonde().getColonHuisarts());
				}
				return null;
			}
		});
	}

	private void toonVraagOmBevestiging(WebMarkupContainer huisartsContainer, EnovationHuisarts huisarts)
	{
		huisartsContainer.addOrReplace(new ClientHuisartsBevestigenWizardPanel("wizard", ModelUtil.sModel(huisarts))
		{

			@Override
			protected void onBevestigd(AjaxRequestTarget target, EnovationHuisarts huisarts)
			{
				changeHuisarts(target, huisarts);
			}

			@Override
			protected void onStop(AjaxRequestTarget target)
			{
				ClientColonWijzigenHuisartsPage.this.onStop(target);
			}

			@Override
			protected void onZoekAndereHuisarts(AjaxRequestTarget target)
			{
				toonZoeken(huisartsContainer, getLaatsteRonde(), true);
				target.add(huisartsContainer);
			}
		});

	}

	private void changeHuisarts(AjaxRequestTarget target, EnovationHuisarts huisarts)
	{
		ColonScreeningRonde ronde = getLaatsteRonde();
		ronde.setColonHuisarts(huisarts);
		huisartsService.koppelHuisarts(huisarts, null, ronde, getIngelogdeClient());
		onStop(target);
	}

	protected void onStop(AjaxRequestTarget target)
	{
		setResponsePage(ClientColonDashboardPage.class);
	}

	@Override
	protected void detachModel()
	{
		super.detachModel();
		ModelUtil.nullSafeDetach(zoekModel);
	}

	@Override
	public ClientHoofdMenuitem getHoofdMenuitem()
	{
		return ClientHoofdMenuitem.MIJNBEVOLKINGSONDERZOEK_COLON;
	}

	@Override
	public IModel<String> getPageName()
	{
		return new SimpleStringResourceModel("label.huisartswijzigen");
	}

}
