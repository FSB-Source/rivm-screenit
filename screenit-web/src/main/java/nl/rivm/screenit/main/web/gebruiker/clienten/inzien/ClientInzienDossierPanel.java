package nl.rivm.screenit.main.web.gebruiker.clienten.inzien;

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

import java.text.SimpleDateFormat;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.Collections;
import java.util.List;

import nl.rivm.screenit.main.comparator.DossierGebeurtenisComparator;
import nl.rivm.screenit.main.model.AfmeldenDossierGebeurtenis;
import nl.rivm.screenit.main.model.DossierGebeurtenis;
import nl.rivm.screenit.main.model.DossierGebeurtenisType;
import nl.rivm.screenit.main.model.OpenUitnodigingDossierGebeurtenis;
import nl.rivm.screenit.main.service.DossierService;
import nl.rivm.screenit.main.util.BriefOmschrijvingUtil;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.component.modal.IDialog;
import nl.rivm.screenit.main.web.gebruiker.clienten.inzien.popup.afmelding.AfmeldformulierInzienPopupPanel;
import nl.rivm.screenit.main.web.gebruiker.clienten.inzien.popup.afmelding.UploadAfmeldformulierPopupPanel;
import nl.rivm.screenit.main.web.gebruiker.clienten.inzien.popup.heraanmelding.HeraanmeldingInzienPopupPanel;
import nl.rivm.screenit.model.AanvraagBriefStatus;
import nl.rivm.screenit.model.Afmelding;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientBrief;
import nl.rivm.screenit.model.Dossier;
import nl.rivm.screenit.model.DossierStatus;
import nl.rivm.screenit.model.colon.ColonAfmelding;
import nl.rivm.screenit.model.colon.ColonDossier;
import nl.rivm.screenit.model.colon.OpenUitnodiging;
import nl.rivm.screenit.model.colon.enums.ColonAfmeldingReden;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.GbaStatus;
import nl.rivm.screenit.model.enums.OpenUitnodigingUitslag;
import nl.rivm.screenit.model.project.ProjectClient;
import nl.rivm.screenit.service.ClientDoelgroepService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.colon.ColonDossierBaseService;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.EnumStringUtil;
import nl.rivm.screenit.util.ProjectUtil;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import nl.topicuszorg.wicket.model.DetachableListModel;

import org.apache.wicket.ajax.AjaxEventBehavior;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.behavior.AttributeAppender;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.EnumLabel;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.markup.html.panel.EmptyPanel;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.markup.repeater.RepeatingView;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.datetime.markup.html.basic.DateLabel;

public class ClientInzienDossierPanel<D extends Dossier<?, ?>, A extends Afmelding<?, ?, ?>, B extends ClientBrief<?, ?, ?>> extends GenericPanel<D>
{

	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private DossierService dossierService;

	@SpringBean
	private ColonDossierBaseService dossierBaseService;

	@SpringBean
	private ICurrentDateSupplier currentDateSupplier;

	@SpringBean
	private ClientDoelgroepService doelgroepService;

	private Bevolkingsonderzoek bevolkingsonderzoek;

	private BootstrapDialog dialog;

	private WebMarkupContainer inactiefContainer;

	private WebMarkupContainer actiefContainer;

	private WebMarkupContainer dossierGebeurtenissenContainer;

	private IModel<Client> clientModel;

	public ClientInzienDossierPanel(String id, IModel<D> dossierModel, IModel<Client> clientModel, Bevolkingsonderzoek bevolkingsonderzoek, BootstrapDialog dialog)
	{
		super(id, dossierModel);
		this.clientModel = clientModel;
		this.bevolkingsonderzoek = bevolkingsonderzoek;
		this.dialog = dialog;
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();
		add(new EnumLabel<>("bevolkingsonderzoek", bevolkingsonderzoek));

		actiefContainer = new WebMarkupContainer("isActief")
		{

			@Override
			protected void onConfigure()
			{
				super.onConfigure();
				setVisible(showActief());
			}

		};
		actiefContainer.setOutputMarkupPlaceholderTag(true);
		add(actiefContainer);

		inactiefContainer = new WebMarkupContainer("isInactief")
		{

			@Override
			protected void onConfigure()
			{
				super.onConfigure();
				setVisible(!showActief());
			}

		};
		inactiefContainer.setOutputMarkupPlaceholderTag(true);

		add(inactiefContainer);

		WebMarkupContainer inactiefDatums = new WebMarkupContainer("inactiefDatums");
		D dossier = getModelObject();
		inactiefContainer.add(maakInactiefDatumLabel(dossier));
		actiefContainer.add(maakActiefDatumLabel(dossier));
		add(getProjectBadge(bevolkingsonderzoek));

		dossierGebeurtenissenContainer = new WebMarkupContainer("dossierGebeurtenissenContainer");
		dossierGebeurtenissenContainer.setOutputMarkupId(true);
		add(addOrReplaceDossierGebeurtenissen(inactiefDatums));
	}

	private Label maakInactiefDatumLabel(D dossier)
	{
		String label = "";
		if (bevolkingsonderzoek == Bevolkingsonderzoek.COLON)
		{
			label = getVolgendeUitnodigingElement(dossier);
		}
		else
		{
			if (dossier != null && dossier.getInactiefVanaf() != null)
			{
				label += " vanaf ";
				label += DateUtil.formatShortDate(dossier.getInactiefVanaf());
			}
			if (dossier != null && dossier.getInactiefTotMet() != null)
			{
				label += " tot/met ";
				label += DateUtil.formatShortDate(dossier.getInactiefTotMet());
			}
		}
		return new Label("inactiefLabelTekst", label);
	}

	private Label maakActiefDatumLabel(D dossier)
	{
		String label = "";
		if (bevolkingsonderzoek == Bevolkingsonderzoek.COLON)
		{
			label = getVolgendeUitnodigingElement(dossier);
		}
		return new Label("actiefLabelTekst", label);
	}

	private boolean showActief()
	{
		D dossier = getModelObject();
		return dossier != null && (dossier.getLaatsteAfmelding() != null || dossier.getLaatsteScreeningRonde() != null) && DossierStatus.ACTIEF.equals(dossier.getStatus())
			&& dossier.getClient().getPersoon().getOverlijdensdatum() == null && dossier.getClient().getPersoon().getDatumVertrokkenUitNederland() == null
			&& GbaStatus.INDICATIE_AANWEZIG.equals(dossier.getClient().getGbaStatus());
	}

	private WebMarkupContainer getProjectBadge(Bevolkingsonderzoek onderzoek)
	{
		Client client = clientModel.getObject();
		List<ProjectClient> projectClienten = ProjectUtil.getProjectClientenForBVO(client, onderzoek, currentDateSupplier.getDate());
		RepeatingView projectBadges = new RepeatingView("projectBadges");
		for (ProjectClient projectClient : projectClienten)
		{
			Boolean isActief = ProjectUtil.isClientActiefInProject(projectClient, currentDateSupplier.getDate());
			String clientProjectLabel = ProjectUtil.getClientActiefInProjectString(projectClient, currentDateSupplier.getDate());
			Label label = new Label(projectBadges.newChildId(), Model.of(clientProjectLabel));
			if (isActief)
			{
				label.add(new AttributeAppender("class", Model.of("status-actief"), " "));
				label.add(new AttributeAppender("class", Model.of("badge-actief"), " "));
				label.add(new AttributeAppender("class", "badge", " "));
			}
			else
			{
				label.add(new AttributeAppender("class", Model.of("status-inactief"), " "));
				label.add(new AttributeAppender("class", Model.of("badge-inactief"), " "));
				label.add(new AttributeAppender("class", "badge", " "));
			}
			projectBadges.add(label);
		}
		if (projectClienten.isEmpty())
		{
			projectBadges.setVisible(false);
			projectBadges.add(new EmptyPanel("isActiefInProjectLabel"));
		}
		return projectBadges;
	}

	private String getVolgendeUitnodigingElement(D dossier)
	{
		ColonDossier colonDossier = (ColonDossier) dossier;
		if (doelgroepService.behoortTotDoelgroep(dossier.getClient(), bevolkingsonderzoek) && colonDossier.getVolgendeUitnodiging() != null)
		{
			LocalDate indicatieveUitnodigingdDatum = dossierBaseService.getDatumVolgendeUitnodiging(colonDossier);
			String datumString = indicatieveUitnodigingdDatum == null ? "Nooit meer uitnodigen" : indicatieveUitnodigingdDatum.format(DateTimeFormatter.ofPattern("dd-MM-yyyy"));
			return "Indicatie volgende uitnodigingsdatum: " + datumString + " ("
				+ getString(EnumStringUtil.getPropertyString(colonDossier.getVolgendeUitnodiging().getInterval().getType())) + ")";
		}
		else
		{
			return "";
		}
	}

	private WebMarkupContainer addOrReplaceDossierGebeurtenissen(WebMarkupContainer inactiefDatums)
	{
		List<DossierGebeurtenis> dossierGebeurtenissen = null;
		switch (bevolkingsonderzoek)
		{
		case COLON:
			dossierGebeurtenissen = dossierService.getColonDossierGebeurtenissen(clientModel.getObject());
			break;
		case CERVIX:
			dossierGebeurtenissen = dossierService.getCervixDossierGebeurtenissen(clientModel.getObject());
			break;
		case MAMMA:
			dossierGebeurtenissen = dossierService.getMammaDossierGebeurtenissen(clientModel.getObject());
			break;
		default:
			break;
		}

		for (DossierGebeurtenis dossierGebeurtenis : dossierGebeurtenissen)
		{
			if (DossierGebeurtenisType.AFMELDING.equals(dossierGebeurtenis.getDossierGebeurtenisType()))
			{
				if (inactiefDatums != null && ((AfmeldenDossierGebeurtenis<A>) dossierGebeurtenis).getAfmelding().getHandtekeningDocumentAfmelding() != null)
				{
					inactiefDatums.setVisible(false);
					break;
				}
			}
		}
		Collections.sort(dossierGebeurtenissen, new DossierGebeurtenisComparator());
		dossierGebeurtenissenContainer.addOrReplace(getListView(dossierGebeurtenissen));
		return dossierGebeurtenissenContainer;
	}

	private ListView<? extends DossierGebeurtenis> getListView(List<DossierGebeurtenis> dossierGebeurtenissen2)
	{
		ListView<DossierGebeurtenis> dossierGebeurtenissen = new ListView<DossierGebeurtenis>("dossierGebeurtenissen", new DetachableListModel(dossierGebeurtenissen2))
		{

			@Override
			protected void populateItem(ListItem<DossierGebeurtenis> item)
			{
				DossierGebeurtenis dossierGebeurtenis = item.getModelObject();

				WebMarkupContainer dossierGebeurtenisContainer = new WebMarkupContainer("dossierGebeurtenisContainer");

				String omschrijving = null;
				boolean clickable = false;
				switch (dossierGebeurtenis.getDossierGebeurtenisType())
				{
				case AFMELDING:
					clickable = true;
					omschrijving = afmeldingOmschrijving(((AfmeldenDossierGebeurtenis<A>) dossierGebeurtenis).getAfmelding());
					break;
				case HERAANMELDING:
					clickable = true;
					omschrijving = heraanmeldingOmschrijving(((AfmeldenDossierGebeurtenis<A>) dossierGebeurtenis).getHeraanmelding());
					break;
				case OPEN_UITNODIGING:
					omschrijving = openUitnodigingOmschrijving(((OpenUitnodigingDossierGebeurtenis) dossierGebeurtenis).getOpenUitnodiging());
					break;
				default:
					break;
				}

				dossierGebeurtenisContainer.add(new Label("omschrijving", omschrijving));

				dossierGebeurtenisContainer.add(DateLabel.forDatePattern("tijd", Model.of(dossierGebeurtenis.getTijd()), "dd-MM-yyyy HH:mm:ss"));
				dossierGebeurtenisContainer.add(new EnumLabel<>("bron", Model.of(dossierGebeurtenis.getBron())));

				if (clickable)
				{
					dossierGebeurtenisContainer.add(new AttributeAppender("class", new Model<>("badge-clickable"), " "));
					dossierGebeurtenisContainer.add(getGebeurtenisClickBehavior(dossierGebeurtenis));
				}
				else
				{
					dossierGebeurtenisContainer.add(new AttributeAppender("class", new Model<>("badge-not-clickable"), " "));
				}

				item.add(dossierGebeurtenisContainer);
			}

		};
		dossierGebeurtenissen.setOutputMarkupPlaceholderTag(true);
		return dossierGebeurtenissen;
	}

	private AjaxEventBehavior getGebeurtenisClickBehavior(DossierGebeurtenis dossierGebeurtenis)
	{
		return new AjaxEventBehavior("click")
		{

			@Override
			protected void onEvent(AjaxRequestTarget target)
			{

				AfmeldenDossierGebeurtenis<A> afmeldenDossierGebeurtenis = (AfmeldenDossierGebeurtenis<A>) dossierGebeurtenis;
				if (DossierGebeurtenisType.AFMELDING.equals(dossierGebeurtenis.getDossierGebeurtenisType()))
				{
					A afmelding = afmeldenDossierGebeurtenis.getAfmelding();
					if (AanvraagBriefStatus.VERWERKT.equals(afmelding.getAfmeldingStatus()))
					{
						dialog.openWith(target, new AfmeldformulierInzienPopupPanel<>(IDialog.CONTENT_ID, afmeldenDossierGebeurtenis.getAfmeldingModel())
						{

							@Override
							protected void close(AjaxRequestTarget target)
							{
								dialog.close(target);
							}
						});
					}
					else
					{
						dialog.openWith(target, new UploadAfmeldformulierPopupPanel<>(IDialog.CONTENT_ID, afmeldenDossierGebeurtenis.getAfmeldingModel())
						{

							@Override
							public void close(AjaxRequestTarget target)
							{
								inactiefContainer.setVisible(true);
								actiefContainer.setVisible(false);
								target.add(inactiefContainer);
								target.add(actiefContainer);
								target.add(addOrReplaceDossierGebeurtenissen(null));
								dialog.close(target);
							}
						});
					}
				}
				else if (DossierGebeurtenisType.HERAANMELDING.equals(dossierGebeurtenis.getDossierGebeurtenisType()))
				{
					dialog.openWith(target, new HeraanmeldingInzienPopupPanel<A, B>(IDialog.CONTENT_ID, afmeldenDossierGebeurtenis.getHeraanmeldingModel())
					{

						@Override
						protected void close(AjaxRequestTarget target)
						{
							dialog.close(target);
						}

					});

				}
			}

		};
	}

	private String openUitnodigingOmschrijving(OpenUitnodiging openUitnodiging)
	{
		StringBuilder omschrijving = new StringBuilder();
		omschrijving.append(getString("openuitnodiging.tekst"));

		OpenUitnodigingUitslag reactie = openUitnodiging.getUitslag();
		if (OpenUitnodigingUitslag.CLIENT_ONDER_CONTROLE.equals(reactie) || OpenUitnodigingUitslag.CLIENT_OVER_10_JAAR_UITNODIGINGEN.equals(reactie))
		{
			omschrijving.append(" (");
			omschrijving.append(getString(EnumStringUtil.getPropertyString(reactie)));
			if (openUitnodiging.getDatum() != null && OpenUitnodigingUitslag.CLIENT_ONDER_CONTROLE.equals(reactie))
			{
				omschrijving.append(", ");
				omschrijving.append(new SimpleDateFormat("dd-MM-yyyy").format(openUitnodiging.getDatum()));
			}
			omschrijving.append(")");
		}

		return omschrijving.toString();
	}

	public String afmeldingOmschrijving(A afmelding)
	{
		StringBuilder omschrijving = new StringBuilder();
		if (Bevolkingsonderzoek.COLON.equals(afmelding.getBevolkingsonderzoek())
			&& ColonAfmeldingReden.ONTERECHT.equals(((ColonAfmelding) HibernateHelper.deproxy(afmelding)).getReden()))
		{
			omschrijving.append(getString("onterechte_afmelding"));
		}
		else if (AanvraagBriefStatus.BRIEF.equals(afmelding.getAfmeldingStatus()))
		{
			omschrijving.append(getString("aanvraag_definitieve_afmelding"));
		}
		else
		{
			omschrijving.append(getString("afmelding"));
		}
		if (!AanvraagBriefStatus.VERWERKT.equals(afmelding.getAfmeldingStatus()) && afmelding.getAfmeldingAanvraag() != null)
		{
			BriefOmschrijvingUtil.addExtraOmschrijving(omschrijving, afmelding.getAfmeldingAanvraag(), this::getString);
		}
		else if (afmelding.getHandtekeningDocumentAfmelding() != null)
		{
			omschrijving.append(" (").append(getString("label.formulier.getekendafmeld")).append(")");
		}
		return omschrijving.toString();
	}

	private String heraanmeldingOmschrijving(A afmelding)
	{
		StringBuilder omschrijving = new StringBuilder();
		if (Bevolkingsonderzoek.COLON.equals(afmelding.getBevolkingsonderzoek())
			&& ColonAfmeldingReden.ONTERECHT.equals(((ColonAfmelding) HibernateHelper.deproxy(afmelding)).getReden()))
		{
			omschrijving.append(getString("onterechteheraanmelding"));
		}
		else if (AanvraagBriefStatus.BRIEF.equals(afmelding.getHeraanmeldStatus()))
		{
			omschrijving.append(getString("aanvraag_heraanmelding"));
		}
		else
		{
			omschrijving.append(getString("heraanmelding"));
		}
		if (!AanvraagBriefStatus.VERWERKT.equals(afmelding.getHeraanmeldStatus()) && afmelding.getAfmeldingAanvraag() != null)
		{
			BriefOmschrijvingUtil.addExtraOmschrijving(omschrijving, afmelding.getHeraanmeldAanvraag(), this::getString);
		}
		else if (afmelding.getHandtekeningDocumentHeraanmelding() != null)
		{
			omschrijving.append(" (").append(getString("label.formulier.getekendheraanmeld")).append(")");
		}
		else if (AanvraagBriefStatus.VERWERKT.equals(afmelding.getHeraanmeldStatus()))
		{
			omschrijving.append(" (").append(getString("label.status.heraangemeldZonderHandtekening")).append(")");
		}
		return omschrijving.toString();
	}

	@Override
	protected void detachModel()
	{
		super.detachModel();
		ModelUtil.nullSafeDetach(clientModel);
	}
}
