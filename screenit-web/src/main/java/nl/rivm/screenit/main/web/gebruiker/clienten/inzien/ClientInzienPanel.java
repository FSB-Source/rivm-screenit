package nl.rivm.screenit.main.web.gebruiker.clienten.inzien;

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
import nl.rivm.screenit.model.TijdelijkAdres;
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
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.gba.GbaMutatie;
import nl.rivm.screenit.model.gba.GbaVraag;
import nl.rivm.screenit.model.mamma.MammaAfmelding;
import nl.rivm.screenit.model.mamma.MammaBrief;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.util.AdresUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.patientregistratie.persoonsgegevens.model.Geslacht;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import nl.topicuszorg.wicket.input.validator.TelefoonnummerValidator;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.EnumLabel;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.basic.MultiLineLabel;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.markup.html.panel.EmptyPanel;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.hibernate.envers.AuditReader;
import org.hibernate.envers.AuditReaderFactory;
import org.hibernate.envers.query.AuditEntity;
import org.hibernate.envers.query.AuditQuery;
import org.wicketstuff.datetime.PatternDateConverter;
import org.wicketstuff.datetime.markup.html.basic.DateLabel;

public class ClientInzienPanel extends GenericPanel<Client>
{
	private static final long serialVersionUID = 1L;

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
		boolean bezwaarRecht = ScreenitSession.get().checkPermission(Recht.GEBRUIKER_CLIENT_BEZWAAR, Actie.INZIEN);
		if (bezwaarRecht)
		{
			add(new ClientInzienBezwaarPanel("clientInzienBezwaarPanel", getModel(), dialog));
		}
		else
		{
			add(new EmptyPanel("clientInzienBezwaarPanel"));
		}

		addAanvraagOverdrachtGegevensPanel();

		IndicatingAjaxLink<Void> contactAanmaken = new IndicatingAjaxLink<>("contactAanmaken")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				setResponsePage(new ClientContactPage(ClientInzienPanel.this.getModel()));
			}

		};
		contactAanmaken
			.setVisible(ScreenitSession.get().checkPermission(Recht.GEBRUIKER_CLIENT_CONTACT, null, getModelObject()) && !clientService.isClientOverleden(getModelObject()));
		add(contactAanmaken);

		GbaMutatie mutatie = getModelObject().getLaatsteGbaMutatie();
		DateLabel laatsteGbaMutatie = DateLabel.forDatePattern("laatsteGbaMutatie", mutatie != null ? Model.of(mutatie.getMutatieDatum()) : null, "dd-MM-yyyy HH:mm:ss");
		add(laatsteGbaMutatie);

		laatsteGbaMutatie.setVisible(ScreenitSession.get().checkPermission(Recht.GEBRUIKER_GBA_AANVRAGEN, null));

		add(new ClientPaspoortHorizontaal("paspoort", getModel()));
		add(new DateLabel("laatstAangevraagd", new IModel<>()
		{
			@Override
			public Date getObject()
			{
				List<GbaVraag> gbaVragen = new ArrayList<>(getModelObject().getGbaVragen());

				return gbaVragen.stream().map(GbaVraag::getDatum).min(Comparator.naturalOrder()).orElse(null);
			}
		}, new PatternDateConverter("dd-MM-yyyy HH:mm:ss", true))
		{

			@Override
			protected void onConfigure()
			{
				super.onConfigure();
				setVisible(CollectionUtils.isNotEmpty(ClientInzienPanel.this.getModelObject().getGbaVragen()));
			}

		});

		Form<Client> telefoonNummerForm = new Form<>("telefoonform", getModel());
		WebMarkupContainer telefoonnummers = new WebMarkupContainer("telefoonnummers");
		final boolean magTelNummerRegistreren = ScreenitSession.get().checkPermission(Recht.GEBRUIKER_CLIENT_TELEFOONNUMMER_REGISTREREN, Actie.AANPASSEN, getModelObject());

		TextField<String> telefoonnummer1 = new TextField<>("persoon.telefoonnummer1");
		telefoonnummer1.setVisible(magTelNummerRegistreren || StringUtils.isNotBlank(getModelObject().getPersoon().getTelefoonnummer1()));
		telefoonnummer1.setEnabled(magTelNummerRegistreren);
		telefoonnummer1.add(TelefoonnummerValidator.alle());
		telefoonnummers.add(telefoonnummer1);

		TextField<String> telefoonnummer2 = new TextField<>("persoon.telefoonnummer2");
		telefoonnummer2.setVisible(magTelNummerRegistreren || StringUtils.isNotBlank(getModelObject().getPersoon().getTelefoonnummer2()));
		telefoonnummer2.setEnabled(magTelNummerRegistreren);
		telefoonnummer2.add(TelefoonnummerValidator.alle());
		telefoonnummers.add(telefoonnummer2);
		telefoonnummers.add(new IndicatingAjaxSubmitLink("opslaantelefoon")
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				Client client = (Client) getForm().getModelObject();
				hibernateService.saveOrUpdate(client);
				info("Telefoonnummer succesvol opgeslagen");
			}
		}.setVisible(magTelNummerRegistreren));
		telefoonNummerForm.add(telefoonnummers);
		add(telefoonNummerForm);

		IModel<String> laatseBekendeRegioBijRni = laatseBekendeRegioBijRni(getModelObject().getPersoon().getGbaAdres());
		add(new Label("persoon.gbaAdres.gbaGemeente.screeningOrganisatie.naam", laatseBekendeRegioBijRni));

		TijdelijkAdres tijdelijkAdres = getModelObject().getPersoon().getTijdelijkAdres();
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
			private static final long serialVersionUID = 1L;

			@Override
			protected void onConfigure()
			{
				super.onConfigure();
				setVisible(ClientInzienPanel.this.getModelObject().getPersoon().getDatumVertrokkenUitNederland() == null);
			}
		});

		add(new EnumLabel<GbaStatus>("gbaStatus")
		{
			private static final long serialVersionUID = 1L;

			@Override
			protected void onConfigure()
			{
				super.onConfigure();
				setVisible(ScreenitSession.get().checkPermission(Recht.GEBRUIKER_GBA_AANVRAGEN, null, ClientInzienPanel.this.getModelObject()));
			}

		});

		add(new DateLabel("persoon.datumVertrokkenUitNederland", new PatternDateConverter("dd-MM-yyyy", true))
		{
			private static final long serialVersionUID = 1L;

			@Override
			protected void onConfigure()
			{
				super.onConfigure();
				setVisible(ClientInzienPanel.this.getModelObject().getPersoon().getDatumVertrokkenUitNederland() != null);
			}
		});
		add(new DateLabel("persoon.datumVestigingNederland", new PatternDateConverter("dd-MM-yyyy", true))
		{
			private static final long serialVersionUID = 1L;

			@Override
			protected void onConfigure()
			{
				super.onConfigure();
				setVisible(ClientInzienPanel.this.getModelObject().getPersoon().getDatumVestigingNederland() != null);
			}
		});

		add(new ClientInzienDossierPanel<ColonDossier, ColonAfmelding, ColonBrief>("colonDossier", ModelUtil.csModel(getModelObject().getColonDossier()), getModel(),
			Bevolkingsonderzoek.COLON, dialog));
		if (getModelObject().getPersoon().getGeslacht().equals(Geslacht.VROUW))
		{
			add(new ClientInzienDossierPanel<CervixDossier, CervixAfmelding, CervixBrief>("cervixDossier", ModelUtil.csModel(getModelObject().getCervixDossier()), getModel(),
				Bevolkingsonderzoek.CERVIX, dialog));
			add(new ClientInzienDossierPanel<MammaDossier, MammaAfmelding, MammaBrief>("mammaDossier", ModelUtil.csModel(getModelObject().getMammaDossier()), getModel(),
				Bevolkingsonderzoek.MAMMA, dialog));
		}
		else
		{
			add(new EmptyPanel("cervixDossier"));
			add(new EmptyPanel("mammaDossier"));
		}
	}

	private void addAanvraagOverdrachtGegevensPanel()
	{
		if (ScreenitSession.get().checkPermission(Recht.GEBRUIKER_AANVRAAG_OVERDRACHT_PERSOONSGEGEVENS, Actie.INZIEN))
		{
			add(new ClientInzienOverdrachtPersoonsgegevensPanel("clientInzienAanvraagOverdrachtGegevensPanel", getModel(), dialog));
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
			AuditReader reader = AuditReaderFactory.get(hibernateService.getHibernateSession());
			AuditQuery query = reader.createQuery().forRevisionsOfEntity(BagAdres.class, false, true);
			query.add(AuditEntity.id().eq(gbaAdres.getId()));

			query.addOrder(AuditEntity.revisionNumber().desc());
			List resultList = query.getResultList();
			for (Object auditRow : resultList)
			{
				Gemeente auditGemeente = ((BagAdres) ((Object[]) auditRow)[0]).getGbaGemeente();
				if (auditGemeente != null)
				{
					Gemeente oldGemeente = hibernateService.load(Gemeente.class, auditGemeente.getId());
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
		final WebMarkupContainer tijdelijkGbaAdres = new WebMarkupContainer("tijdelijkGbaAdres");
		tijdelijkGbaAdres.setOutputMarkupId(true);

		AjaxLink<Void> tijdelijkGbaAdresWijzigen = new AjaxLink<>("tijdelijkGbaAdresWijzigen")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				Client client = ClientInzienPanel.this.getModelObject();
				if (client.getPersoon().getTijdelijkGbaAdres() == null)
				{
					TijdelijkGbaAdres tijdelijkGbaAdres = new TijdelijkGbaAdres();
					BagAdres gbaAdres = client.getPersoon().getGbaAdres();
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

					private static final long serialVersionUID = 1L;

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

			private static final long serialVersionUID = 1L;

			@Override
			public String getObject()
			{
				Client client = ClientInzienPanel.this.getModelObject();
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
		tijdelijkGbaAdresWijzigen.setVisible(ScreenitSession.get().checkPermission(Recht.GEBRUIKER_GBA_TIJDELIJK_ADRES, Actie.AANPASSEN));
		tijdelijkGbaAdres.add(tijdelijkGbaAdresWijzigen);
		tijdelijkGbaAdres.add(new Label("persoon.tijdelijkGbaAdres.plaats"));
		tijdelijkGbaAdres.add(new PostcodeLabel("persoon.tijdelijkGbaAdres.postcode", true));
		tijdelijkGbaAdres.add(new Label("persoon.tijdelijkGbaAdres.adres", new IModel<String>()
		{

			private static final long serialVersionUID = 1L;

			@Override
			public String getObject()
			{
				return AdresUtil.getAdres(ClientInzienPanel.this.getModelObject().getPersoon().getTijdelijkGbaAdres());
			}

		}));

		BagAdres gbaAdres = getModelObject().getPersoon().getGbaAdres();
		tijdelijkGbaAdres
			.setVisible((AdresUtil.isOnvolledigAdres(gbaAdres) || AdresUtil.isVolledigAdresVoorInpakcentrum(getModelObject())) && !StringUtils.equals(gbaAdres.getStraat(), ".")
				&& ScreenitSession.get().checkPermission(Recht.GEBRUIKER_GBA_TIJDELIJK_ADRES, Actie.INZIEN));
		add(tijdelijkGbaAdres);
	}

}
