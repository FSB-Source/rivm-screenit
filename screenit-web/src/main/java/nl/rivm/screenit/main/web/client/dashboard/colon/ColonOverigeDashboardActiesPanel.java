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

import java.util.ArrayList;
import java.util.List;

import nl.rivm.screenit.service.ClientContactService;
import nl.rivm.screenit.service.RondeNummerService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.client.base.ClientFoutPage;
import nl.rivm.screenit.main.web.component.SimpleStringResourceModel;
import nl.rivm.screenit.main.web.gebruiker.clienten.ClientTooltipPanel;
import nl.rivm.screenit.main.web.gebruiker.clienten.inzien.ClientInzienPage;
import nl.rivm.screenit.model.AanvraagBriefStatus;
import nl.rivm.screenit.model.AfmeldingType;
import nl.rivm.screenit.model.BezwaarMoment;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientContactActieType;
import nl.rivm.screenit.model.ClientTooltipType;
import nl.rivm.screenit.model.EnovationHuisarts;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.OnbekendeHuisarts;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.RegioBvoContactGegevens;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BezwaarType;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.GebruikersService;
import nl.rivm.screenit.util.BezwaarUtil;
import nl.rivm.screenit.util.NaamUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;
import nl.topicuszorg.wicket.hibernate.SimpleHibernateModel;
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

public class ColonOverigeDashboardActiesPanel extends GenericPanel<Client>
{
	private static final long serialVersionUID = 1L;

	@SpringBean
	private ClientContactService clientContactService;

	@SpringBean
	private ClientService clientService;

	@SpringBean
	private SimplePreferenceService preferenceService;

	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private RondeNummerService rondeNummerService;

	@SpringBean
	private GebruikersService gebruikerService;

	private IModel<ColonScreeningRonde> colonScreeningRonde = new SimpleHibernateModel<>();

	private boolean heeftOngunstigeUitslag;

	private boolean heeftBezwaarHuisartsAangetekend;

	public ColonOverigeDashboardActiesPanel(String id, IModel<Client> model)
	{
		super(id, model);
		final Client client = getModelObject();
		ColonScreeningRonde laatsteColonScreeningRonde = getLaatsteColonScreeningRonde();
		this.colonScreeningRonde.setObject(laatsteColonScreeningRonde);
		heeftOngunstigeUitslag = laatsteColonScreeningRonde != null
			&& laatsteColonScreeningRonde.getBrieven().stream().anyMatch(brief -> BriefType.COLON_UITNODIGING_INTAKE.equals(brief.getBriefType()));
		heeftBezwaarHuisartsAangetekend = BezwaarUtil.isBezwaarActiefVoor(client, BezwaarType.GEEN_UITWISSELING_MET_DE_HUISARTS, Bevolkingsonderzoek.COLON);
		final List<ClientContactActieType> availableActieTypes = clientContactService.getAvailableActies(model.getObject(), true);
		add(new ClientTooltipPanel("bezwaarTooltip", ClientTooltipType.BEZWAAR_MAKEN, true));
		Link<Void> bezwaarLink = new Link<Void>("bezwaar")
		{
			private static final long serialVersionUID = 1L;

			@Override
			public void onClick()
			{
				List<Object> extraPanelParams = new ArrayList<>();
				extraPanelParams.add(Bevolkingsonderzoek.COLON);
				setResponsePage(new ColonOverigeDashboardActiePage(getNieuwClientCCModel(), ClientColonBezwaarPanel.class, ClientContactActieType.BEZWAAR,
					extraPanelParams));
			}
		};
		boolean behoortTotDoelgroep = clientService.behoortTotDoelgroep(client, Bevolkingsonderzoek.COLON);
		boolean magBezwaarMaken = behoortTotDoelgroep;

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
				if (clientService.behoortTotDoelgroep(ColonOverigeDashboardActiesPanel.this.getModelObject(), Bevolkingsonderzoek.COLON))
				{
					List<Object> extraPanelParams = new ArrayList<>();
					extraPanelParams.add(AanvraagBriefStatus.BRIEF);
					setResponsePage(
						new ColonOverigeDashboardActiePage(getNieuwClientCCModel(), ClientColonAfmeldenPanel.class, ClientContactActieType.COLON_AFMELDEN,
							extraPanelParams));
				}
				else
				{
					setResponsePage(new ClientFoutPage(getString("label.niet.afmelden"), ClientColonDashboardPage.class));
				}
			}

		}.setVisible(
			availableActieTypes.contains(ClientContactActieType.COLON_AFMELDEN) && 
				(client.getColonDossier().getLaatsteAfmelding() != null || 
					client.getColonDossier().getLaatsteAfmelding() == null)));
		add(new ClientTooltipPanel("heraanmeldenTooltip", ClientTooltipType.HERAANMELDEN, true));
		add(new Link<Void>("heraanmelden")
		{
			@Override
			public void onClick()
			{
				setResponsePage(
					new ColonOverigeDashboardActiePage(getNieuwClientCCModel(), ClientColonHeraanmeldenPanel.class,
						ClientContactActieType.COLON_HERAANMELDEN));
			}

		}.setVisible(
			availableActieTypes.contains(ClientContactActieType.COLON_HERAANMELDEN) && client.getColonDossier().getLaatsteAfmelding() != null
				|| availableActieTypes.contains(ClientContactActieType.COLON_HERAANMELDEN)
					&& client.getColonDossier().getLaatsteScreeningRonde() != null && client.getColonDossier().getLaatsteScreeningRonde().getLaatsteAfmelding() != null
					&& AfmeldingType.EENMALIG.equals(client.getColonDossier().getLaatsteScreeningRonde().getLaatsteAfmelding().getType())));

		add(new ClientTooltipPanel("aanvragenNieuwIfobtTooltip", ClientTooltipType.NIEUWE_IFOBT_AANVRAGEN, true));
		add(new Link<Void>("aanvragenNieuwIfobt")
		{
			@Override
			public void onClick()
			{
				setResponsePage(new ColonOverigeDashboardActiePage(getNieuwClientCCModel(), ClientNieuweIfobtAanvragenPanel.class,
					ClientContactActieType.COLON_AANVRAGEN_NIEUWE_IFOBT));
			}

		}.setVisible(availableActieTypes.contains(ClientContactActieType.COLON_AANVRAGEN_NIEUWE_IFOBT)));

		ScreeningOrganisatie screeningOrganisatie = getModelObject().getPersoon().getGbaAdres().getGbaGemeente().getScreeningOrganisatie();
		if (screeningOrganisatie != null)
		{
			RegioBvoContactGegevens contactGegevens = screeningOrganisatie.getRegioBvoContactGegevensDk();
			if (contactGegevens != null)
			{
				add(new Label("soTel", contactGegevens.getTelefoon()));
				add(new Label("clientPortaalVrijeTekst", contactGegevens.getClientPortaalVrijeTekst()));
			}
			else
			{
				add(new Label("soTel", screeningOrganisatie.getTelefoon()));
				add(new Label("clientPortaalVrijeTekst", screeningOrganisatie.getClientPortaalVrijeTekst()));
			}
		}
		else
		{
			add(new Label("soTel", ""));
			add(new Label("clientPortaalVrijeTekst", ""));
		}

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
					setResponsePage(new ClientInzienPage(ColonOverigeDashboardActiesPanel.this.getModel()));
				}
			}
		}.setVisible(RuntimeConfigurationType.DEVELOPMENT.equals(Application.get().getConfigurationType())));

		huisartsBlock(behoortTotDoelgroep);
	}

	private void huisartsBlock(boolean behoortTotDoelgroep)
	{
		WebMarkupContainer huisarts = new WebMarkupContainer("huisarts");
		ColonScreeningRonde laatsteColonScreeningRonde = getLaatsteColonScreeningRonde();
		add(huisarts.setVisible(behoortTotDoelgroep && laatsteColonScreeningRonde != null));

		boolean heeftHuisarts = laatsteColonScreeningRonde != null && (laatsteColonScreeningRonde.getColonHuisarts() != null
			|| laatsteColonScreeningRonde.getOnbekendeHuisarts() != null);

		String aanvullendeTekstHuisartsProperty = "label.aanvullendehuisartstekst.geenhuisarts";
		ColonScreeningRonde vorigeRonde = null;
		if (laatsteColonScreeningRonde != null)
		{
			vorigeRonde = rondeNummerService.getVorigeRonde(laatsteColonScreeningRonde);
		}
		if (vorigeRonde != null && vorigeRonde.getColonHuisarts() != null)
		{
			aanvullendeTekstHuisartsProperty = "label.aanvullendehuisartstekst.uitvorigeronde";
		}
		if (heeftOngunstigeUitslag && !heeftHuisarts)
		{
			aanvullendeTekstHuisartsProperty = "label.aanvullendehuisartstekst.ongunstiggeenhuisarts";
		}
		if (heeftOngunstigeUitslag && heeftHuisarts)
		{
			aanvullendeTekstHuisartsProperty = "label.aanvullendehuisartstekst.ongunstigheefthuisarts";
		}
		if (heeftBezwaarHuisartsAangetekend)
		{
			aanvullendeTekstHuisartsProperty = "label.aanvullendehuisartstekst.bezwaarhuisarts";
		}
		MultiLineLabel aanvullendeTekstHuisarts = new MultiLineLabel("aanvullendeTekstHuisarts", new SimpleStringResourceModel(aanvullendeTekstHuisartsProperty));
		huisarts.add(aanvullendeTekstHuisarts);

		String buttonLabel = "label.button.";
		Label huisartsNaam = new Label("colonHuisarts.naam", getHuisartsNaam());
		huisarts.add(huisartsNaam);

		String praktijknaamProperty = "onbekendeHuisarts.praktijkNaam";
		if (getColonHuisarts() != null)
		{
			praktijknaamProperty = "colonHuisarts.praktijknaam";
		}
		Label huisartspraktijk = new Label("colonHuisarts.praktijknaam", new PropertyModel<String>(colonScreeningRonde, praktijknaamProperty));
		huisarts.add(huisartspraktijk);

		if (heeftHuisarts && !heeftOngunstigeUitslag && !heeftBezwaarHuisartsAangetekend)
		{
			buttonLabel += "huisarts_wijzigen";
			aanvullendeTekstHuisarts.setVisible(false);
		}
		else
		{
			if (vorigeRonde != null && vorigeRonde.getColonHuisarts() != null)
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

		Link<Void> huisartsLink = new Link<Void>("colonHuisarts")
		{

			@Override
			public void onClick()
			{
				setResponsePage(ClientColonWijzigenHuisartsPage.class);
			}

		};
		huisartsLink.setVisible(!heeftOngunstigeUitslag && !heeftBezwaarHuisartsAangetekend);
		huisarts.add(huisartsLink);
		huisartsLink.add(new Label("button.label", getString(buttonLabel)));

	}

	private ColonScreeningRonde getLaatsteColonScreeningRonde()
	{
		return getModelObject().getColonDossier().getLaatsteScreeningRonde();
	}

	private EnovationHuisarts getColonHuisarts()
	{
		ColonScreeningRonde ronde = getLaatsteColonScreeningRonde();
		if (ronde != null)
		{
			return ronde.getColonHuisarts();
		}
		return null;
	}

	private OnbekendeHuisarts getOnbekendeHuisarts()
	{
		ColonScreeningRonde ronde = getLaatsteColonScreeningRonde();
		if (ronde != null)
		{
			return ronde.getOnbekendeHuisarts();
		}
		return null;
	}

	private String getHuisartsNaam()
	{
		EnovationHuisarts huisarts = getColonHuisarts();
		OnbekendeHuisarts onbekendehuisarts = getOnbekendeHuisarts();

		if (huisarts != null)
		{
			return NaamUtil.getNaamHuisarts(huisarts);
		}
		else if (onbekendehuisarts != null)
		{
			return NaamUtil.getNaamOnbekendeHuisarts(onbekendehuisarts);
		}
		return "";
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(colonScreeningRonde);
	}

	private IModel<Client> getNieuwClientCCModel()
	{
		return ModelUtil.ccModel(getModelObject());
	}
}
