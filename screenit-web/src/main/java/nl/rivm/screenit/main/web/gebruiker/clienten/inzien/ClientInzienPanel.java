package nl.rivm.screenit.main.web.gebruiker.clienten.inzien;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.util.Comparator;
import java.util.Date;
import java.util.List;

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.PostcodeLabel;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.component.modal.IDialog;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.ClientContactPage;
import nl.rivm.screenit.model.BagAdres;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Gemeente;
import nl.rivm.screenit.model.TijdelijkGbaAdres;
import nl.rivm.screenit.model.cervix.CervixAfmelding;
import nl.rivm.screenit.model.cervix.CervixBrief;
import nl.rivm.screenit.model.cervix.CervixDossier;
import nl.rivm.screenit.model.colon.ColonAfmelding;
import nl.rivm.screenit.model.colon.ColonBrief;
import nl.rivm.screenit.model.colon.ColonDossier;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.GbaStatus;
import nl.rivm.screenit.model.enums.GbaVraagType;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.gba.GbaVraag;
import nl.rivm.screenit.model.mamma.MammaAfmelding;
import nl.rivm.screenit.model.mamma.MammaBrief;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.util.AdresUtil;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.EnumLabel;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.basic.MultiLineLabel;
import org.apache.wicket.markup.html.panel.EmptyPanel;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.hibernate.envers.AuditReaderFactory;
import org.hibernate.envers.query.AuditEntity;
import org.wicketstuff.datetime.PatternDateConverter;
import org.wicketstuff.datetime.markup.html.basic.DateLabel;

public class ClientInzienPanel extends GenericPanel<Client>
{

	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private ClientService clientService;

	private BootstrapDialog dialog;

	public ClientInzienPanel(String id, IModel<Client> model)
	{
		super(id, new CompoundPropertyModel<>(model));
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();
		dialog = new BootstrapDialog("dialog");
		dialog.setOutputMarkupPlaceholderTag(true);
		add(dialog);
		var client = getModelObject();
		addBezwaarPanel();

		addAanvraagOverdrachtGegevensPanel();

		var algemeneBrievenPanel = new ClientInzienAlgemeneBrievenPanel("algemeneBrievenPanel", getModel(), dialog);
		algemeneBrievenPanel.setEnabled(clientService.isClientActief(client));
		add(algemeneBrievenPanel);

		IndicatingAjaxLink<Void> contactAanmaken = new IndicatingAjaxLink<>("contactAanmaken")
		{

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				setResponsePage(new ClientContactPage(ClientInzienPanel.this.getModel()));
			}

		};
		contactAanmaken
			.setVisible(ScreenitSession.get().checkPermission(Recht.GEBRUIKER_CLIENT_CONTACT, null, client) && !clientService.isClientOverleden(client)
				&& clientService.isClientActief(client));
		add(contactAanmaken);

		var mutatie = client.getLaatsteGbaMutatie();
		var laatsteGbaMutatie = DateLabel.forDatePattern("laatsteGbaMutatie", mutatie != null ? Model.of(mutatie.getMutatieDatum()) : null, "dd-MM-yyyy HH:mm:ss");
		add(laatsteGbaMutatie);

		laatsteGbaMutatie.setVisible(ScreenitSession.get().checkPermission(Recht.GEBRUIKER_GBA_AANVRAGEN, null));

		add(new ClientPaspoortHorizontaal("paspoort", getModel()));
		add(new DateLabel("laatstAangevraagd", (IModel<Date>) () ->
		{
			List<GbaVraag> gbaVragen = new ArrayList<>(getModelObject().getGbaVragen());
			var momentLaatsteVerwijderIndicatie = gbaVragen.stream().filter(v -> v.getVraagType() == GbaVraagType.VERWIJDER_INDICATIE).map(GbaVraag::getDatum)
				.max(Comparator.naturalOrder())
				.orElse(null);
			return DateUtil.toUtilDate(momentLaatsteVerwijderIndicatie);
		}, new PatternDateConverter("dd-MM-yyyy HH:mm:ss", true))
		{

			@Override
			protected void onConfigure()
			{
				super.onConfigure();
				setVisible(CollectionUtils.isNotEmpty(ClientInzienPanel.this.getModelObject().getGbaVragen()));
			}

		});

		var contactGegevens = new ClientContactGegevensPanel("contactGegevens", getModel());
		add(contactGegevens);

		IModel<String> laatseBekendeRegioBijRni = laatseBekendeRegioBijRni(client.getPersoon().getGbaAdres());
		add(new Label("persoon.gbaAdres.gbaGemeente.screeningOrganisatie.naam", laatseBekendeRegioBijRni));

		var tijdelijkAdres = client.getPersoon().getTijdelijkAdres();
		add(new Label("persoon.tijdelijkAdres.plaats"));
		add(new Label("persoon.tijdelijkAdres.huisletter"));
		add(new PostcodeLabel("persoon.tijdelijkAdres.postcode", true));
		add(new Label("persoon.tijdelijkAdres.adres"));
		add(DateLabel.forDatePattern("persoon.tijdelijkAdres.startDatum", "dd-MM-yyyy"));
		add(DateLabel.forDatePattern("persoon.tijdelijkAdres.eindDatum", "dd-MM-yyyy").setVisible(tijdelijkAdres != null && tijdelijkAdres.getStartDatum() != null));

		tijdelijkeGbaAdres();

		add(new MultiLineLabel("adres", new IModel<String>()
		{

			private static final long serialVersionUID = 1L;

			@Override
			public String getObject()
			{
				return AdresUtil.getVolledigeGbaAdresString(getModelObject().getPersoon());
			}

		})
		{

			@Override
			protected void onConfigure()
			{
				super.onConfigure();
				setVisible(ClientInzienPanel.this.getModelObject().getPersoon().getDatumVertrokkenUitNederland() == null);
			}
		});

		add(new EnumLabel<GbaStatus>("gbaStatus")
		{

			@Override
			protected void onConfigure()
			{
				super.onConfigure();
				setVisible(ScreenitSession.get().checkPermission(Recht.GEBRUIKER_GBA_AANVRAGEN, null, ClientInzienPanel.this.getModelObject()));
			}

		});

		add(new DateLabel("persoon.datumVertrokkenUitNederland", new PatternDateConverter("dd-MM-yyyy", true))
		{

			@Override
			protected void onConfigure()
			{
				super.onConfigure();
				setVisible(ClientInzienPanel.this.getModelObject().getPersoon().getDatumVertrokkenUitNederland() != null);
			}
		});
		add(new DateLabel("persoon.datumVestigingNederland", new PatternDateConverter("dd-MM-yyyy", true))
		{

			@Override
			protected void onConfigure()
			{
				super.onConfigure();
				setVisible(ClientInzienPanel.this.getModelObject().getPersoon().getDatumVestigingNederland() != null);
			}
		});

		addDossierPanels();
	}

	private void addBezwaarPanel()
	{
		var bezwaarRecht = ScreenitSession.get().checkPermission(Recht.GEBRUIKER_CLIENT_BEZWAAR, Actie.INZIEN);
		if (bezwaarRecht)
		{
			var clientInzienBezwaarPanel = new ClientInzienBezwaarPanel("clientInzienBezwaarPanel", getModel(), dialog);
			clientInzienBezwaarPanel.setEnabled(clientService.isClientActief(getModelObject()));
			add(clientInzienBezwaarPanel);
		}
		else
		{
			add(new EmptyPanel("clientInzienBezwaarPanel"));
		}
	}

	private void addDossierPanels()
	{
		var client = getModelObject();
		var colonDossier = new ClientInzienDossierPanel<ColonDossier, ColonAfmelding, ColonBrief>("colonDossier", ModelUtil.csModel(client.getColonDossier()), getModel(),
			Bevolkingsonderzoek.COLON, dialog);
		colonDossier.setEnabled(clientService.isClientActief(client));
		add(colonDossier);

		if (client.getCervixDossier() != null)
		{
			var cervixDossier = new ClientInzienDossierPanel<CervixDossier, CervixAfmelding, CervixBrief>("cervixDossier", ModelUtil.csModel(client.getCervixDossier()),
				getModel(), Bevolkingsonderzoek.CERVIX, dialog);
			cervixDossier.setEnabled(clientService.isClientActief(client));
			add(cervixDossier);
		}
		else
		{
			add(new EmptyPanel("cervixDossier"));
		}

		if (client.getMammaDossier() != null)
		{
			var mammaDossier = new ClientInzienDossierPanel<MammaDossier, MammaAfmelding, MammaBrief>("mammaDossier", ModelUtil.csModel(client.getMammaDossier()),
				getModel(), Bevolkingsonderzoek.MAMMA, dialog);
			mammaDossier.setEnabled(clientService.isClientActief(client));
			add(mammaDossier);
		}
		else
		{
			add(new EmptyPanel("mammaDossier"));
		}
	}

	private void addAanvraagOverdrachtGegevensPanel()
	{
		if (ScreenitSession.get().checkPermission(Recht.GEBRUIKER_AANVRAAG_OVERDRACHT_PERSOONSGEGEVENS, Actie.INZIEN))
		{
			var clientInzienAanvraagOverdrachtGegevensPanel = new ClientInzienOverdrachtPersoonsgegevensPanel("clientInzienAanvraagOverdrachtGegevensPanel", getModel(), dialog);
			clientInzienAanvraagOverdrachtGegevensPanel.setEnabled(clientService.isClientActief(getModelObject()));
			add(clientInzienAanvraagOverdrachtGegevensPanel);

		}
		else
		{
			add(new EmptyPanel("clientInzienAanvraagOverdrachtGegevensPanel"));
		}
	}

	private IModel<String> laatseBekendeRegioBijRni(BagAdres gbaAdres)
	{
		IModel<String> laatseBekendeRegioBijRni = null;
		if (gbaAdres.getGbaGemeente() == null || gbaAdres.getGbaGemeente().getCode().equals(Gemeente.RNI_CODE) || gbaAdres.getGbaGemeente().getScreeningOrganisatie() == null)
		{
			var reader = AuditReaderFactory.get(hibernateService.getHibernateSession());
			var query = reader.createQuery().forRevisionsOfEntity(BagAdres.class, false, true);
			query.add(AuditEntity.id().eq(gbaAdres.getId()));

			query.addOrder(AuditEntity.revisionNumber().desc());
			var resultList = query.getResultList();
			for (Object auditRow : resultList)
			{
				var auditGemeente = ((BagAdres) ((Object[]) auditRow)[0]).getGbaGemeente();
				if (auditGemeente != null)
				{
					var oldGemeente = hibernateService.load(Gemeente.class, auditGemeente.getId());
					if (oldGemeente != null && !oldGemeente.getCode().equals(Gemeente.RNI_CODE) && oldGemeente.getScreeningOrganisatie() != null)
					{
						laatseBekendeRegioBijRni = Model.of(oldGemeente.getScreeningOrganisatie().getNaam() + " (laatst bekende)");
						break;
					}
				}
			}
			if (laatseBekendeRegioBijRni == null)
			{
				laatseBekendeRegioBijRni = Model.of("Onbekend");
			}
		}

		return laatseBekendeRegioBijRni;
	}

	private void tijdelijkeGbaAdres()
	{
		final var tijdelijkGbaAdres = new WebMarkupContainer("tijdelijkGbaAdres");
		tijdelijkGbaAdres.setOutputMarkupId(true);

		var tijdelijkGbaAdresWijzigen = new AjaxLink<>("tijdelijkGbaAdresWijzigen")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				var client = ClientInzienPanel.this.getModelObject();
				if (client.getPersoon().getTijdelijkGbaAdres() == null)
				{
					var tijdelijkGbaAdres = new TijdelijkGbaAdres();
					var gbaAdres = client.getPersoon().getGbaAdres();
					tijdelijkGbaAdres.setHuisletter(gbaAdres.getHuisletter());
					tijdelijkGbaAdres.setHuisnummer(gbaAdres.getHuisnummer());
					tijdelijkGbaAdres.setStraat(gbaAdres.getStraat());
					tijdelijkGbaAdres.setPlaats(gbaAdres.getPlaats());
					tijdelijkGbaAdres.setHuisnummerAanduiding(gbaAdres.getHuisnummerAanduiding());
					tijdelijkGbaAdres.setHuisnummerToevoeging(gbaAdres.getHuisnummerToevoeging());
					tijdelijkGbaAdres.setPostcode(gbaAdres.getPostcode());
					tijdelijkGbaAdres.setLocatieBeschrijving(gbaAdres.getLocatieBeschrijving());
					client.getPersoon().setTijdelijkGbaAdres(tijdelijkGbaAdres);
				}
				dialog.openWith(target, new TijdelijkGbaAdresDialogPanel(IDialog.CONTENT_ID, ModelUtil.ccModel(client))
				{

					@Override
					protected void close(AjaxRequestTarget target)
					{
						target.add(tijdelijkGbaAdres);
						dialog.close(target);
					}
				});
			}

		};

		tijdelijkGbaAdresWijzigen.add(new Label("label", new IModel<String>()
		{
			@Override
			public String getObject()
			{
				var client = ClientInzienPanel.this.getModelObject();
				if (client.getPersoon().getTijdelijkGbaAdres() == null)
				{
					return getString("label.aanmaken");
				}
				else
				{
					return getString("label.wijzigen");
				}
			}
		}));
		var client = getModelObject();
		tijdelijkGbaAdresWijzigen.setVisible(
			ScreenitSession.get().checkPermission(Recht.GEBRUIKER_GBA_TIJDELIJK_ADRES, Actie.AANPASSEN) && clientService.isClientActief(client));
		tijdelijkGbaAdres.add(tijdelijkGbaAdresWijzigen);
		tijdelijkGbaAdres.add(new Label("persoon.tijdelijkGbaAdres.plaats"));
		tijdelijkGbaAdres.add(new PostcodeLabel("persoon.tijdelijkGbaAdres.postcode", true));
		tijdelijkGbaAdres.add(new Label("persoon.tijdelijkGbaAdres.adres", new IModel<String>()
		{
			@Override
			public String getObject()
			{
				return AdresUtil.getAdres(ClientInzienPanel.this.getModelObject().getPersoon().getTijdelijkGbaAdres());
			}

		}));

		var gbaAdres = client.getPersoon().getGbaAdres();
		tijdelijkGbaAdres
			.setVisible((AdresUtil.isOnvolledigAdres(gbaAdres) || AdresUtil.isVolledigAdresVoorInpakcentrum(client)) && !StringUtils.equals(gbaAdres.getStraat(), ".")
				&& ScreenitSession.get().checkPermission(Recht.GEBRUIKER_GBA_TIJDELIJK_ADRES, Actie.INZIEN));
		add(tijdelijkGbaAdres);
	}

}
