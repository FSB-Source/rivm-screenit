package nl.rivm.screenit.main.web.client.dashboard.cervix;

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

import java.util.ArrayList;
import java.util.List;

import nl.rivm.screenit.service.ClientContactService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.gebruiker.clienten.ClientTooltipPanel;
import nl.rivm.screenit.main.web.gebruiker.clienten.inzien.ClientInzienPage;
import nl.rivm.screenit.model.AanvraagBriefStatus;
import nl.rivm.screenit.model.AfmeldingType;
import nl.rivm.screenit.model.BezwaarMoment;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientContactActieType;
import nl.rivm.screenit.model.ClientTooltipType;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.RegioBvoContactGegevens;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.service.ClientDoelgroepService;
import nl.rivm.screenit.service.GebruikersService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.Application;
import org.apache.wicket.RuntimeConfigurationType;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.link.Link;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class CervixOverigeDashboardActiesPanel extends GenericPanel<Client>
{
	private static final long serialVersionUID = 1L;

	@SpringBean
	private ClientContactService clientContactService;

	@SpringBean
	private SimplePreferenceService preferenceService;

	@SpringBean
	private ClientDoelgroepService doelgroepService;

	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private GebruikersService gebruikerService;

	public CervixOverigeDashboardActiesPanel(String id, IModel<Client> model)
	{
		super(id, model);
		final Client client = getModelObject();
		final List<ClientContactActieType> availableActieTypes = clientContactService.getAvailableActies(client, true);

		add(new ClientTooltipPanel("bezwaarTooltip", ClientTooltipType.BEZWAAR_MAKEN, true));
		Link<Void> bezwaarLink = new Link<Void>("bezwaar")
		{
			@Override
			public void onClick()
			{
				List<Object> extraPanelParams = new ArrayList<>();
				extraPanelParams.add(Bevolkingsonderzoek.CERVIX);
				setResponsePage(
					new CervixOverigeDashboardActiePage(CervixOverigeDashboardActiesPanel.this.getModel(), ClientCervixBezwaarPanel.class, ClientContactActieType.BEZWAAR,
						extraPanelParams));
			}
		};
		boolean magBezwaarMaken = doelgroepService.behoortTotDoelgroep(client, Bevolkingsonderzoek.CERVIX);

		if (client.getBezwaarMomenten().size() > 0)
		{
			BezwaarMoment laatsteBezwaar = client.getBezwaarMomenten().get(0);
			magBezwaarMaken &= !AanvraagBriefStatus.BRIEF.equals(laatsteBezwaar.getStatus());
		}
		bezwaarLink.setVisible(magBezwaarMaken);
		add(bezwaarLink);

		add(new ClientTooltipPanel("afmeldenTooltip", ClientTooltipType.AFMELDEN, true));

		add(new Link<Void>("afmelden")
		{

			@Override
			public void onClick()
			{
				List<Object> extraPanelParams = new ArrayList<>();
				extraPanelParams.add(AanvraagBriefStatus.BRIEF);
				extraPanelParams.add(ClientContactActieType.CERVIX_AFMELDEN);
				setResponsePage(
					new CervixOverigeDashboardActiePage(getNieuwClientCCModel(), ClientCervixAfmeldenPanel.class, ClientContactActieType.CERVIX_AFMELDEN,
						extraPanelParams));
			}

		}.setVisible(

			availableActieTypes.contains(ClientContactActieType.CERVIX_AFMELDEN) && client.getCervixDossier() != null && client.getCervixDossier().getLaatsteAfmelding() != null
				|| availableActieTypes.contains(ClientContactActieType.CERVIX_AFMELDEN) && client.getCervixDossier() == null
				|| availableActieTypes.contains(ClientContactActieType.CERVIX_AFMELDEN) && client.getCervixDossier().getLaatsteAfmelding() == null));

		add(new ClientTooltipPanel("heraanmeldenTooltip", ClientTooltipType.HERAANMELDEN, true));
		add(new Link<Void>("heraanmelden")
		{

			@Override
			public void onClick()
			{
				setResponsePage(
					new CervixOverigeDashboardActiePage(getNieuwClientCCModel(), ClientCervixHeraanmeldenPanel.class,
						ClientContactActieType.CERVIX_HERAANMELDEN));
			}

		}.setVisible(
			availableActieTypes.contains(ClientContactActieType.CERVIX_HERAANMELDEN) && client.getCervixDossier() != null && client.getCervixDossier().getLaatsteAfmelding() != null
				|| availableActieTypes.contains(ClientContactActieType.CERVIX_HERAANMELDEN) && client.getCervixDossier() != null
					&& client.getCervixDossier().getLaatsteScreeningRonde() != null && client.getCervixDossier().getLaatsteScreeningRonde().getLaatsteAfmelding() != null
					&& AfmeldingType.EENMALIG.equals(client.getCervixDossier().getLaatsteScreeningRonde().getLaatsteAfmelding().getType())));

		add(new ClientTooltipPanel("zasTooltip", ClientTooltipType.ZAS_AANVRAGEN, true));
		add(new Link<Void>("zas")
		{

			@Override
			public void onClick()
			{
				setResponsePage(new CervixOverigeDashboardActiePage(getNieuwClientCCModel(), ClientZasAanvragenPanel.class,
					ClientContactActieType.CERVIX_ZAS_AANVRAGEN));
			}

		}.setVisible(availableActieTypes.contains(ClientContactActieType.CERVIX_ZAS_AANVRAGEN)));

		add(new ClientTooltipPanel("herdrukTooltip", ClientTooltipType.HERDRUK_AANVRAGEN, true));
		add(new Link<Void>("herdruk")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick()
			{
				setResponsePage(new CervixOverigeDashboardActiePage(getNieuwClientCCModel(), ClientHerdrukAanvragenPanel.class,
					ClientContactActieType.CERVIX_HERDRUK));
			}

		}.setVisible(availableActieTypes.contains(ClientContactActieType.CERVIX_HERDRUK)));

		add(new ClientTooltipPanel("uitstelTooltip", ClientTooltipType.UITSTEL_AANVRAGEN, true));
		add(new Link<Void>("uitstel")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick()
			{
				setResponsePage(new CervixOverigeDashboardActiePage(getNieuwClientCCModel(), ClientUitstelAanvragenPanel.class,
					ClientContactActieType.CERVIX_UITSTEL));
			}

		}.setVisible(availableActieTypes.contains(ClientContactActieType.CERVIX_UITSTEL)));

		ScreeningOrganisatie screeningOrganisatie = getModelObject().getPersoon().getGbaAdres().getGbaGemeente().getScreeningOrganisatie();
		if (screeningOrganisatie != null)
		{
			RegioBvoContactGegevens contactGegevens = screeningOrganisatie.getRegioBvoContactGegevensBmhk();
			if (contactGegevens != null)
			{
				add(new Label("soTel", contactGegevens.getTelefoon()));
				add(new Label("clientPortaalVrijeTekst", contactGegevens.getClientPortaalVrijeTekst()));
			}
			else
			{
				add(new Label("soTel", ""));
				add(new Label("clientPortaalVrijeTekst", ""));
			}
		}
		else
		{
			add(new Label("soTel", ""));
			add(new Label("clientPortaalVrijeTekst", ""));
		}

		add(new AjaxLink<Void>("directNaarClientDossier")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				Gebruiker beherder = gebruikerService.getGebruikerByGebruikersnaam("beheer");
				if (beherder != null && beherder.getOrganisatieMedewerkers().size() > 0)
				{
					ScreenitSession.get().login(
						beherder.getOrganisatieMedewerkers().stream().filter(om -> om.getOrganisatie().getOrganisatieType() == OrganisatieType.RIVM).findFirst().orElse(null));
					setResponsePage(new ClientInzienPage(CervixOverigeDashboardActiesPanel.this.getModel()));
				}
			}
		}.setVisible(RuntimeConfigurationType.DEVELOPMENT.equals(Application.get().getConfigurationType())));
	}

	private IModel<Client> getNieuwClientCCModel()
	{
		return ModelUtil.ccModel(getModelObject());
	}
}
