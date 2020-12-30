package nl.rivm.screenit.main.web.client.dashboard.mamma;

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

import nl.rivm.screenit.main.model.BaseHuisartsModel;
import nl.rivm.screenit.main.model.EnovationHuisartsModel;
import nl.rivm.screenit.main.service.RondeNummerService;
import nl.rivm.screenit.main.service.mamma.MammaHuisartsService;
import nl.rivm.screenit.main.web.client.base.ClientBasePage;
import nl.rivm.screenit.main.web.client.base.ClientHoofdMenuitem;
import nl.rivm.screenit.main.web.client.dashboard.huisartswizard.ClientHuisartsBevestigenWizardPanel;
import nl.rivm.screenit.main.web.client.dashboard.huisartswizard.ClientHuisartsZoekenWizardPanel;
import nl.rivm.screenit.main.web.component.SimpleStringResourceModel;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.EnovationHuisarts;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.enums.MammaGeenHuisartsOption;
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
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.MAMMA })
public class ClientMammaWijzigenHuisartsPage extends ClientBasePage
{

	private static final long serialVersionUID = 1L;

	@SpringBean
	private RondeNummerService rondeNummerService;

	@SpringBean
	private MammaHuisartsService huisartsService;

	private IModel<EnovationHuisarts> zoekModel;

	public ClientMammaWijzigenHuisartsPage()
	{
		WebMarkupContainer huisartsContainer = new WebMarkupContainer("panel");
		huisartsContainer.setOutputMarkupId(true);
		MammaScreeningRonde ronde = getLaatsteRonde();
		MammaScreeningRonde vorigeRonde = rondeNummerService.getVorigeRonde(ronde);

		zoekModel = ModelUtil.cRModel(new EnovationHuisarts());

		if (vorigeRonde != null &&
			(vorigeRonde.getHuisarts() != null && !vorigeRonde.getHuisarts().isVerwijderd() || vorigeRonde.getGeenHuisartsOptie() != null)
			&& ronde.getHuisarts() == null && ronde.getGeenHuisartsOptie() == null)
		{
			toonVraagOmBevestiging(huisartsContainer, vorigeRonde.getHuisarts(), vorigeRonde.getGeenHuisartsOptie());
		}
		else
		{
			toonZoeken(huisartsContainer, ronde, false);
		}

		add(huisartsContainer);
	}

	private MammaScreeningRonde getLaatsteRonde()
	{
		Client client = getIngelogdeClient();
		MammaScreeningRonde ronde = client.getMammaDossier().getLaatsteScreeningRonde();
		return ronde;
	}

	private void toonZoeken(WebMarkupContainer huisartsContainer, MammaScreeningRonde laatsteRonde, boolean toonDirectZoekresultaten)
	{

		huisartsContainer.addOrReplace(new ClientHuisartsZoekenWizardPanel("wizard", zoekModel, toonDirectZoekresultaten)
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onHuisartsGekozen(AjaxRequestTarget target, EnovationHuisarts ha)
			{
				toonVraagOmBevestiging(huisartsContainer, ha, null);
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
				ClientMammaWijzigenHuisartsPage.this.onStop(target);
			}

			@Override
			protected BaseHuisartsModel<?> getHuisartsModel()
			{
				if (getLaatsteRonde() != null && getLaatsteRonde().getHuisarts() != null)
				{
					return new EnovationHuisartsModel(getLaatsteRonde().getHuisarts());
				}
				return null;
			}

			@Override
			protected WebMarkupContainer getHuisartsInfoComponent(String id)
			{
				MammaScreeningRonde ronde = getLaatsteRonde();
				if (ronde != null)
				{
					if (ronde.getHuisarts() != null)
					{
						return super.getHuisartsInfoComponent(id);
					}
					else if (ronde.getGeenHuisartsOptie() != null)
					{
						return new GeenHuisartsOptiePanel(id, ronde.getGeenHuisartsOptie());
					}
				}
				EmptyPanel geenHuisarts = new EmptyPanel(id);
				geenHuisarts.setVisible(false);
				return geenHuisarts;
			}

			@Override
			protected boolean magHuisartsVerwijderen()
			{
				return huisartsService.magHuisartsVerwijderen(getLaatsteRonde());
			}
		});
	}

	private void toonVraagOmBevestiging(WebMarkupContainer huisartsContainer, EnovationHuisarts huisarts, MammaGeenHuisartsOption geenHuisartsOption)
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
				ClientMammaWijzigenHuisartsPage.this.onStop(target);
			}

			@Override
			protected void onZoekAndereHuisarts(AjaxRequestTarget target)
			{
				toonZoeken(huisartsContainer, getLaatsteRonde(), true);
				target.add(huisartsContainer);
			}

			@Override
			protected WebMarkupContainer getHuisartsInfoComponent(String id)
			{
				if (getModelObject() != null)
				{
					return super.getHuisartsInfoComponent(id);
				}
				else if (geenHuisartsOption != null)
				{
					return new GeenHuisartsOptiePanel(id, geenHuisartsOption);
				}
				EmptyPanel geenHuisarts = new EmptyPanel(id);
				geenHuisarts.setVisible(false);
				return geenHuisarts;
			}
		});

	}

	private void changeHuisarts(AjaxRequestTarget target, EnovationHuisarts huisarts)
	{
		MammaScreeningRonde ronde = getLaatsteRonde();
		ronde.setHuisarts(huisarts);
		ronde.setGeenHuisartsOptie(null);
		huisartsService.koppelHuisarts(huisarts, ronde, getIngelogdeClient());
		onStop(target);
	}

	protected void onStop(AjaxRequestTarget target)
	{
		setResponsePage(ClientMammaDashboardPage.class);
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
		return ClientHoofdMenuitem.MIJNBEVOLKINGSONDERZOEK_MAMMA;
	}

	@Override
	public IModel<String> getPageName()
	{
		return new SimpleStringResourceModel("label.huisartswijzigen");
	}

}
