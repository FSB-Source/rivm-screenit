package nl.rivm.screenit.main.web.client.dashboard.mamma;

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
import nl.rivm.screenit.service.ClientDoelgroepService;
import nl.rivm.screenit.service.RondeNummerService;
import nl.rivm.screenit.util.EnumStringUtil;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.SimpleStringResourceModel;
import nl.rivm.screenit.main.web.gebruiker.clienten.ClientTooltipPanel;
import nl.rivm.screenit.main.web.gebruiker.clienten.inzien.ClientInzienPage;
import nl.rivm.screenit.model.AanvraagBriefStatus;
import nl.rivm.screenit.model.AfmeldingType;
import nl.rivm.screenit.model.BezwaarMoment;
import nl.rivm.screenit.model.CentraleEenheid;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientContactActieType;
import nl.rivm.screenit.model.ClientTooltipType;
import nl.rivm.screenit.model.EnovationHuisarts;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.enums.MammaDoelgroep;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.GebruikersService;
import nl.rivm.screenit.util.NaamUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.Application;
import org.apache.wicket.RuntimeConfigurationType;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.basic.MultiLineLabel;
import org.apache.wicket.markup.html.link.Link;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class ClientMammaOverigeDashboardActiesPanel extends GenericPanel<Client>
{

	@SpringBean
	private ClientContactService clientContactService;

	@SpringBean
	private SimplePreferenceService preferenceService;

	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private ClientService clientService;

	@SpringBean
	private ClientDoelgroepService doelgroepService;

	@SpringBean
	private GebruikersService gebruikerService;

	@SpringBean
	private RondeNummerService rondeNummerService;

	public ClientMammaOverigeDashboardActiesPanel(String id, IModel<Client> model)
	{
		super(id, model);
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();
		final Client client = getModelObject();
		final List<ClientContactActieType> availableActieTypes = clientContactService.getAvailableActies(client, true);

		maakBezwaarActie(client);

		maakAfmeldenActie(client, availableActieTypes);

		maakHeraanmeldenActie(client, availableActieTypes);

		maakAfspraakMakenActie(client, availableActieTypes);

		maakFooter(client);

		maakOntwikkelaarButton();

		maakHuisartsBlock();
	}

	private void maakOntwikkelaarButton()
	{
		add(new AjaxLink<Void>("directNaarClientDossier")
		{

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				Gebruiker beherder = gebruikerService.getGebruikerByGebruikersnaam("beheer");
				if (beherder != null && beherder.getOrganisatieMedewerkers().size() > 0)
				{
					ScreenitSession.get().login(
						beherder.getOrganisatieMedewerkers().stream().filter(om -> om.getOrganisatie().getOrganisatieType() == OrganisatieType.RIVM).findFirst().orElse(null));
					setResponsePage(new ClientInzienPage(ClientMammaOverigeDashboardActiesPanel.this.getModel()));
				}
			}
		}.setVisible(RuntimeConfigurationType.DEVELOPMENT.equals(Application.get().getConfigurationType())));
	}

	private void maakFooter(final Client client)
	{
		CentraleEenheid centraleEenheid = clientService.bepaalCe(client);
		String telefoonNummer = null;
		String clientPortaalVrijeTekst = null;
		if (centraleEenheid != null)
		{
			clientPortaalVrijeTekst = centraleEenheid.getClientPortaalVrijeTekst();
			MammaDoelgroep doelgroep = client.getMammaDossier().getDoelgroep();
			if (doelgroep == MammaDoelgroep.MINDER_VALIDE)
			{
				telefoonNummer = centraleEenheid.getTelefoon4();
			}
			else
			{
				telefoonNummer = centraleEenheid.getTelefoon();
			}
		}
		add(new Label("soTel", telefoonNummer));
		add(new Label("clientPortaalVrijeTekst", clientPortaalVrijeTekst));
	}

	private void maakHeraanmeldenActie(final Client client, final List<ClientContactActieType> availableActieTypes)
	{

		add(new ClientTooltipPanel("heraanmeldenTooltip", ClientTooltipType.HERAANMELDEN, true));
		add(new Link<Void>("heraanmelden")
		{

			@Override
			public void onClick()
			{
				setResponsePage(
					new ClientMammaOverigeDashboardActiePage(getNieuwClientCCModel(), ClientMammaHeraanmeldenPanel.class,
						ClientContactActieType.MAMMA_HERAANMELDEN));
			}

		}.setVisible(
			availableActieTypes.contains(ClientContactActieType.MAMMA_HERAANMELDEN) && client.getMammaDossier() != null && client.getMammaDossier().getLaatsteAfmelding() != null
				|| availableActieTypes.contains(ClientContactActieType.MAMMA_HERAANMELDEN) && client.getMammaDossier() != null
					&& client.getMammaDossier().getLaatsteScreeningRonde() != null && client.getMammaDossier().getLaatsteScreeningRonde().getLaatsteAfmelding() != null
					&& AfmeldingType.EENMALIG.equals(client.getMammaDossier().getLaatsteScreeningRonde().getLaatsteAfmelding().getType())));
	}

	private void maakAfmeldenActie(final Client client, final List<ClientContactActieType> availableActieTypes)
	{

		add(new ClientTooltipPanel("afmeldenTooltip", ClientTooltipType.AFMELDEN, true));

		add(new Link<Void>("afmelden")
		{

			@Override
			public void onClick()
			{
				List<Object> extraPanelParams = new ArrayList<>();
				extraPanelParams.add(AanvraagBriefStatus.BRIEF);
				extraPanelParams.add(ClientContactActieType.MAMMA_AFMELDEN);
				setResponsePage(
					new ClientMammaOverigeDashboardActiePage(getNieuwClientCCModel(), ClientMammaAfmeldenPanel.class,
						ClientContactActieType.MAMMA_AFMELDEN, extraPanelParams));
			}

		}.setVisible(

			availableActieTypes.contains(ClientContactActieType.MAMMA_AFMELDEN) && client.getMammaDossier() != null && client.getMammaDossier().getLaatsteAfmelding() != null
				|| availableActieTypes.contains(ClientContactActieType.MAMMA_AFMELDEN) && client.getMammaDossier() == null
				|| availableActieTypes.contains(ClientContactActieType.MAMMA_AFMELDEN) && client.getMammaDossier().getLaatsteAfmelding() == null));
	}

	private void maakBezwaarActie(final Client client)
	{

		add(new ClientTooltipPanel("bezwaarTooltip", ClientTooltipType.BEZWAAR_MAKEN, true));
		Link<Void> bezwaarLink = new Link<Void>("bezwaar")
		{

			@Override
			public void onClick()
			{
				List<Object> extraPanelParams = new ArrayList<>();
				extraPanelParams.add(Bevolkingsonderzoek.MAMMA);
				setResponsePage(
					new ClientMammaOverigeDashboardActiePage(getNieuwClientCCModel(), ClientMammaBezwaarPanel.class, ClientContactActieType.BEZWAAR,
						extraPanelParams));
			}
		};
		boolean magBezwaarMaken = doelgroepService.behoortTotDoelgroep(client, Bevolkingsonderzoek.MAMMA);

		if (client.getBezwaarMomenten().size() > 0)
		{
			BezwaarMoment laatsteBezwaar = client.getBezwaarMomenten().get(0);
			magBezwaarMaken &= !AanvraagBriefStatus.BRIEF.equals(laatsteBezwaar.getStatus());
		}
		bezwaarLink.setVisible(magBezwaarMaken);
		add(bezwaarLink);
	}

	private void maakAfspraakMakenActie(Client client, List<ClientContactActieType> availableActieTypes)
	{
		add(new ClientTooltipPanel("afspraakAanmakenTooltip", ClientTooltipType.MAMMA_AFSPRAAK_MAKEN, true));
		MammaDossier dossier = client.getMammaDossier();
		add(new Link<Void>("afspraakAanmaken")
		{
			@Override
			public void onClick()
			{
				setResponsePage(new ClientMammaAfspraakAanmakenPage(getNieuwClientCCModel()));
			}

		}.setVisible(availableActieTypes.contains(ClientContactActieType.MAMMA_AFSPRAAK_MAKEN)
			&& dossier.getDoelgroep() != MammaDoelgroep.MINDER_VALIDE
			&& dossier.getTehuis() == null));

	}

	private void maakHuisartsBlock()
	{
		WebMarkupContainer huisarts = new WebMarkupContainer("huisarts");
		MammaScreeningRonde laatsteScreeningRonde = getLaatsteScreeningRonde();
		add(huisarts.setVisible(laatsteScreeningRonde != null));

		boolean heeftHuisarts = getHuisarts() != null || laatsteScreeningRonde != null && laatsteScreeningRonde.getGeenHuisartsOptie() != null;

		String aanvullendeTekstHuisartsProperty = "label.aanvullendehuisartstekst.geenhuisarts";
		MammaScreeningRonde vorigeRonde = null;
		if (laatsteScreeningRonde != null)
		{
			vorigeRonde = rondeNummerService.getVorigeRonde(laatsteScreeningRonde);
		}
		if (vorigeRonde != null && vorigeRonde.getHuisarts() != null)
		{
			aanvullendeTekstHuisartsProperty = "label.aanvullendehuisartstekst.uitvorigeronde";
		}
		MultiLineLabel aanvullendeTekstHuisarts = new MultiLineLabel("aanvullendeTekstHuisarts", new SimpleStringResourceModel(aanvullendeTekstHuisartsProperty));
		huisarts.add(aanvullendeTekstHuisarts);

		String buttonLabel = "label.button.";
		Label huisartsNaam = new Label("huisarts.naam", getHuisartsNaam());
		huisarts.add(huisartsNaam);

		Label huisartspraktijk = new Label("huisarts.praktijknaam", new PropertyModel<String>(laatsteScreeningRonde, "huisarts.praktijknaam").getObject());
		huisarts.add(huisartspraktijk);

		if (heeftHuisarts)
		{
			buttonLabel += "huisarts_wijzigen";
			aanvullendeTekstHuisarts.setVisible(false);
		}
		else
		{
			if (vorigeRonde != null && vorigeRonde.getHuisarts() != null)
			{
				buttonLabel += "huisarts_bevestigen";
			}
			else
			{
				buttonLabel += "huisarts_opgeven";
			}
		}

		if (!heeftHuisarts)
		{
			huisartsNaam.setVisible(false);
			huisartspraktijk.setVisible(false);
		}

		Link<Void> huisartsLink = new Link<Void>("huisarts")
		{

			@Override
			public void onClick()
			{
				setResponsePage(ClientMammaWijzigenHuisartsPage.class);
			}

		};
		huisarts.add(huisartsLink);
		huisartsLink.add(new Label("button.label", getString(buttonLabel)));

	}

	private MammaScreeningRonde getLaatsteScreeningRonde()
	{
		return getModelObject().getMammaDossier().getLaatsteScreeningRonde();
	}

	private EnovationHuisarts getHuisarts()
	{
		MammaScreeningRonde ronde = getLaatsteScreeningRonde();
		if (ronde != null)
		{
			return ronde.getHuisarts();
		}
		return null;
	}

	private String getHuisartsNaam()
	{
		EnovationHuisarts huisarts = getHuisarts();

		if (huisarts != null)
		{
			return NaamUtil.getNaamHuisarts(huisarts);
		}
		if (getLaatsteScreeningRonde() != null && getLaatsteScreeningRonde().getGeenHuisartsOptie() != null)
		{
			return getString(EnumStringUtil.getPropertyString(getLaatsteScreeningRonde().getGeenHuisartsOptie()));
		}
		return "";
	}

	private IModel<Client> getNieuwClientCCModel()
	{
		return ModelUtil.ccModel(getModelObject());
	}
}
