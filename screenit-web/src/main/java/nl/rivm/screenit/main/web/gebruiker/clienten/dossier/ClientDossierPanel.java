package nl.rivm.screenit.main.web.gebruiker.clienten.dossier;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.main.model.ScreeningRondeGebeurtenis;
import nl.rivm.screenit.main.model.ScreeningRondeGebeurtenissen;
import nl.rivm.screenit.main.model.TypeGebeurtenis;
import nl.rivm.screenit.main.service.ClientDossierFilter;
import nl.rivm.screenit.main.service.DossierService;
import nl.rivm.screenit.main.util.BriefOmschrijvingUtil;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.component.modal.IDialog;
import nl.rivm.screenit.main.web.component.modal.IDialogCloseCallback;
import nl.rivm.screenit.main.web.gebruiker.clienten.ClientPage;
import nl.rivm.screenit.main.web.gebruiker.clienten.ClientPaspoortPanel;
import nl.rivm.screenit.main.web.gebruiker.clienten.agenda.ClientAgendaPage;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.ClientContactPage;
import nl.rivm.screenit.main.web.gebruiker.clienten.dossier.gebeurtenissen.GebeurtenisPopupBasePanel;
import nl.rivm.screenit.main.web.gebruiker.clienten.verslag.ClientVerslagPage;
import nl.rivm.screenit.main.web.gebruiker.clienten.verslag.ClientVerslagenPage;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ScreeningRondeStatus;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.GebeurtenisBron;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.project.ProjectClient;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.util.EnumStringUtil;
import nl.rivm.screenit.util.ProjectUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import nl.topicuszorg.wicket.model.DetachableListModel;
import nl.topicuszorg.wicket.model.SortingListModel;

import org.apache.wicket.ajax.AjaxEventBehavior;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.behavior.AttributeAppender;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.EnumLabel;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.markup.html.list.PropertyListView;
import org.apache.wicket.markup.html.panel.EmptyPanel;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.markup.repeater.RepeatingView;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.datetime.markup.html.basic.DateLabel;

public class ClientDossierPanel extends GenericPanel<Client>
{
	@SpringBean
	private DossierService dossierService;

	@SpringBean
	private ICurrentDateSupplier currentDateSupplier;

	@SpringBean
	private LogService logService;

	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private ClientService clientService;

	private final BootstrapDialog dialog;

	private IModel<List<ScreeningRondeGebeurtenissen>> dossierModel;

	private ListView<ScreeningRondeGebeurtenissen> listView;

	private WebMarkupContainer gebeurtenissenContainer;

	public ClientDossierPanel(String id, IModel<Client> model)
	{
		super(id, model);

		hibernateService.reload(model.getObject());

		var client = getModelObject();
		var loggedInInstellingGebruiker = ScreenitSession.get().getLoggedInInstellingGebruiker();

		IModel<ClientDossierFilter> zoekObjectModel;
		if (!ScreenitSession.get().isZoekObjectGezetForComponent(ClientPage.class))
		{
			List<Bevolkingsonderzoek> bevolkingsonderzoeken = loggedInInstellingGebruiker.getBevolkingsonderzoeken();
			zoekObjectModel = new Model<>(new ClientDossierFilter(new ArrayList<>(bevolkingsonderzoeken), Boolean.TRUE));
			ScreenitSession.get().setZoekObject(ClientPage.class, zoekObjectModel);
		}
		else
		{
			zoekObjectModel = (IModel<ClientDossierFilter>) ScreenitSession.get().getZoekObject(ClientPage.class);
		}

		var contactBvoFilter = new ClientDossierFilterBvoPanel("contactBvoFilter", zoekObjectModel)
		{

			@Override
			protected void doFilter(IModel<ClientDossierFilter> filterModel, AjaxRequestTarget target)
			{
				ListView<ScreeningRondeGebeurtenissen> newlistView = getGebeurtenissenContainer(filterModel.getObject());
				ClientDossierPanel.this.listView.replaceWith(newlistView);
				ClientDossierPanel.this.listView = newlistView;
				target.add(gebeurtenissenContainer);
			}
		};
		add(contactBvoFilter);
		add(new ClientPaspoortPanel("passpoort", model));

		IndicatingAjaxLink<Void> contactAanmaken = new IndicatingAjaxLink<>("contactAanmaken")
		{

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				setResponsePage(new ClientContactPage(ClientDossierPanel.this.getModel()));
			}
		};

		contactAanmaken
			.setVisible(ScreenitSession.get().checkPermission(Recht.GEBRUIKER_CLIENT_CONTACT, null, model.getObject()) && !clientService.isClientOverleden(model.getObject())
				&& clientService.isClientActief(client));
		add(contactAanmaken);

		gebeurtenissenContainer = new WebMarkupContainer("gebeurtenissenContainer");
		gebeurtenissenContainer.setOutputMarkupId(true);
		listView = getGebeurtenissenContainer(zoekObjectModel.getObject());
		gebeurtenissenContainer.add(listView);
		add(gebeurtenissenContainer);

		dialog = new BootstrapDialog("dialog");
		add(dialog);

		logAction(LogGebeurtenis.SCREENINGSRONDE_INZIEN, getModelObject());
	}

	private ListView<ScreeningRondeGebeurtenissen> getGebeurtenissenContainer(final ClientDossierFilter clientDossierFilter)
	{
		dossierModel = new DetachableListModel<>(dossierService.getScreeningRondeGebeurtenissen((Client) ClientDossierPanel.this.getDefaultModelObject(), clientDossierFilter));
		var client = getModelObject();

		ListView<ScreeningRondeGebeurtenissen> rondeListView = new ListView<>("rondes", dossierModel)
		{

			@Override
			protected void populateItem(ListItem<ScreeningRondeGebeurtenissen> item)
			{
				var bevolkingsonderzoek = item.getModelObject().getScreeningRonde().getBevolkingsonderzoek();
				item.add(new Label("bvo", getString(EnumStringUtil.getPropertyString(bevolkingsonderzoek))));
				item.add(getProjectBadge(ClientDossierPanel.this.getModelObject(), bevolkingsonderzoek));
				switch (bevolkingsonderzoek)
				{
				case COLON:
					maakColonGebeurtenissen(clientDossierFilter, item);
					break;
				case CERVIX:
					maakCervixGebeurtenissen(clientDossierFilter, item);
					break;
				case MAMMA:
					maakMammaGebeurtenissen(clientDossierFilter, item);
					break;
				default:
					break;
				}
				item.add(new Label("index", item.getModelObject().getRondenr() + ""));
				boolean gepushtbadge = false;
				if (ColonScreeningRonde.class.equals(hibernateService.getDeproxiedClass(item.getModelObject().getScreeningRonde())))
				{
					gepushtbadge = ((ColonScreeningRonde) item.getModelObject().getScreeningRonde()).isGepusht();
				}
				item.add(new Label("gepusht", "Gepusht").setVisible(gepushtbadge));
			}
		};
		rondeListView.setOutputMarkupId(true);
		if (!clientService.isClientActief(client))
		{
			rondeListView.setEnabled(false);
		}
		return rondeListView;
	}

	private void maakColonGebeurtenissen(final ClientDossierFilter clientDossierFilter, ListItem<ScreeningRondeGebeurtenissen> item)
	{
		item.add(new PropertyListView<>("gebeurtenissen",
			new SortingListModel<>(new PropertyModel<>(item.getModel(), "gebeurtenissen"), new GebeurtenisComparator()))
		{

			@Override
			protected void populateItem(final ListItem<ScreeningRondeGebeurtenis> item)
			{
				var screeningRondeGebeurtenis = item.getModelObject();
				IModel<Client> clientModel = ClientDossierPanel.this.getModel();

				final TypeGebeurtenis gebeurtenis = screeningRondeGebeurtenis.getGebeurtenis();
				item.add(DateLabel.forDatePattern("datum", "dd-MM-yyyy HH:mm:ss"));
				item.add(new EnumLabel<TypeGebeurtenis>("gebeurtenis"));
				item.add(new EnumLabel<GebeurtenisBron>("bron"));

				if (screeningRondeGebeurtenis.isClickable() && gebeurtenis.getDetailPanelClass() != null
					&& ScreenitSession.get().getAuthorizationStrategy().isInstantiationAuthorized(gebeurtenis.getDetailPanelClass())
					&& (!gebeurtenis.equals(TypeGebeurtenis.UITNODIGING) || item.getIndex() == 0)
					|| (gebeurtenis.equals(TypeGebeurtenis.INTAKEAFSPRAAKGEMAAKT) || gebeurtenis.equals(TypeGebeurtenis.INTAKEAFSPRAAKAFGEZEGD))
					&& ScreenitSession.get().getAuthorizationStrategy().isInstantiationAuthorized(ClientAgendaPage.class)
					|| (gebeurtenis.equals(TypeGebeurtenis.UITSLAGCOLOSCOPIEONTVANGEN) || gebeurtenis.equals(TypeGebeurtenis.UITSLAGPATHOLOGIEONTVANGEN))
					&& ScreenitSession.get().getAuthorizationStrategy().isInstantiationAuthorized(ClientVerslagenPage.class))
				{
					item.add(new AttributeAppender("class", new Model<>("badge-clickable"), " "));
				}
				else
				{
					item.add(new AttributeAppender("class", new Model<>("badge-not-clickable"), " "));
				}

				addExtraOmschrijvingItem(item);

				if ((gebeurtenis.equals(TypeGebeurtenis.INTAKEAFSPRAAKGEMAAKT) || gebeurtenis.equals(TypeGebeurtenis.INTAKEAFSPRAAKAFGEZEGD))
					&& ScreenitSession.get().getAuthorizationStrategy().isInstantiationAuthorized(ClientAgendaPage.class))
				{
					item.add(new AjaxEventBehavior("click")
					{

						@Override
						protected void onEvent(AjaxRequestTarget target)
						{
							setResponsePage(new ClientAgendaPage((IModel<Client>) getPage().getDefaultModel()));
						}
					});
				}
				else if (gebeurtenis.equals(TypeGebeurtenis.UITSLAGCOLOSCOPIEONTVANGEN)
					&& ScreenitSession.get().checkPermission(Recht.GEBRUIKER_CLIENT_SR_UITSLAGCOLOSCOPIEONTVANGEN, Actie.INZIEN, clientModel.getObject()))
				{
					openVerslagPage(item);
				}
				else if (gebeurtenis.equals(TypeGebeurtenis.UITSLAGPATHOLOGIEONTVANGEN) &&
					ScreenitSession.get().checkPermission(Recht.GEBRUIKER_CLIENT_SR_UITSLAGPATHOLOGIEONTVANGEN, Actie.INZIEN, clientModel.getObject()))
				{
					openVerslagPage(item);
				}
				else if (screeningRondeGebeurtenis.isClickable() && gebeurtenis.getDetailPanelClass() != null
					&& ScreenitSession.get().getAuthorizationStrategy().isInstantiationAuthorized(gebeurtenis.getDetailPanelClass()))
				{
					item.add(new AjaxEventBehavior("click")
					{

						@Override
						protected void onEvent(AjaxRequestTarget target)
						{
							dialog.setCloseCallback((IDialogCloseCallback) target1 ->
							{
								ListView<ScreeningRondeGebeurtenissen> list = getGebeurtenissenContainer(clientDossierFilter);
								WebMarkupContainer nieuwGebCont = new WebMarkupContainer("gebeurtenissenContainer");
								nieuwGebCont.setOutputMarkupId(true);
								nieuwGebCont.add(list);
								gebeurtenissenContainer.replaceWith(nieuwGebCont);
								gebeurtenissenContainer = nieuwGebCont;
								target1.add(gebeurtenissenContainer);
							});
							dialog.openWith(target, new GebeurtenisPopupBasePanel(IDialog.CONTENT_ID, item.getModel()));
						}
					});
				}

				ColonScreeningRonde colonScreeningRonde = (ColonScreeningRonde) screeningRondeGebeurtenis.getScreeningRondeGebeurtenissen()
					.getScreeningRonde();
				if (gebeurtenis.equals(TypeGebeurtenis.AFGEROND))
				{
					item.add(new AttributeAppender("class", " badge-inverse"));
				}
				if (!colonScreeningRonde.getStatus().equals(ScreeningRondeStatus.AFGEROND) && item.getIndex() == 0)
				{
					item.add(new AttributeAppender("class", " badge-success"));
				}
			}
		});
	}

	private void maakCervixGebeurtenissen(final ClientDossierFilter clientDossierFilter, ListItem<ScreeningRondeGebeurtenissen> item)
	{
		item.add(new PropertyListView<>("gebeurtenissen",
			new SortingListModel<>(new PropertyModel<>(item.getModel(), "gebeurtenissen"), new GebeurtenisComparator()))
		{
			@Override
			protected void populateItem(final ListItem<ScreeningRondeGebeurtenis> item)
			{
				ScreeningRondeGebeurtenis screeningRondeGebeurtenis = item.getModelObject();
				final TypeGebeurtenis gebeurtenis = screeningRondeGebeurtenis.getGebeurtenis();

				item.add(DateLabel.forDatePattern("datum", "dd-MM-yyyy HH:mm:ss"));
				item.add(new EnumLabel<TypeGebeurtenis>("gebeurtenis"));
				item.add(new EnumLabel<GebeurtenisBron>("bron"));
				if (screeningRondeGebeurtenis.isClickable() && gebeurtenis.getDetailPanelClass() != null
					&& ScreenitSession.get().getAuthorizationStrategy().isInstantiationAuthorized(gebeurtenis.getDetailPanelClass())
					&& (!gebeurtenis.equals(TypeGebeurtenis.UITNODIGING) || item.getIndex() == 0)
					|| gebeurtenis.equals(TypeGebeurtenis.BMHK_CYTOLOGISCHE_BEOORDELING)
					&& ScreenitSession.get().checkPermission(Recht.GEBRUIKER_CERVIX_CYTOLOGIE_VERSLAG, Actie.INZIEN, ClientDossierPanel.this.getModelObject()))
				{
					item.add(new AttributeAppender("class", new Model<>("badge-clickable"), " "));
				}
				else
				{
					item.add(new AttributeAppender("class", new Model<>("badge-not-clickable"), " "));
				}

				addExtraOmschrijvingItem(item);

				if (screeningRondeGebeurtenis.isClickable() && gebeurtenis.getDetailPanelClass() != null
					&& ScreenitSession.get().getAuthorizationStrategy().isInstantiationAuthorized(gebeurtenis.getDetailPanelClass()))
				{
					item.add(new AjaxEventBehavior("click")
					{
						@Override
						protected void onEvent(AjaxRequestTarget target)
						{
							dialog.setCloseCallback((IDialogCloseCallback) target1 ->
							{
								ListView<ScreeningRondeGebeurtenissen> list = getGebeurtenissenContainer(clientDossierFilter);
								WebMarkupContainer nieuwGebCont = new WebMarkupContainer("gebeurtenissenContainer");
								nieuwGebCont.setOutputMarkupId(true);
								nieuwGebCont.add(list);
								gebeurtenissenContainer.replaceWith(nieuwGebCont);
								gebeurtenissenContainer = nieuwGebCont;
								target1.add(gebeurtenissenContainer);
							});
							dialog.openWith(target, new GebeurtenisPopupBasePanel(IDialog.CONTENT_ID, item.getModel()));
						}
					});
				}
				var cervixScreeningRonde = screeningRondeGebeurtenis.getScreeningRondeGebeurtenissen().getScreeningRonde();
				if (gebeurtenis.equals(TypeGebeurtenis.AFGEROND))
				{
					item.add(new AttributeAppender("class", " badge-inverse"));
				}
				if (!cervixScreeningRonde.getStatus().equals(ScreeningRondeStatus.AFGEROND) && item.getIndex() == 0)
				{
					item.add(new AttributeAppender("class", " badge-success"));
				}
			}
		});
	}

	private void maakMammaGebeurtenissen(final ClientDossierFilter clientDossierFilter, ListItem<ScreeningRondeGebeurtenissen> item)
	{
		item.add(new PropertyListView<>("gebeurtenissen",
			new SortingListModel<>(new PropertyModel<>(item.getModel(), "gebeurtenissen"), new GebeurtenisComparator()))
		{
			@Override
			protected void populateItem(final ListItem<ScreeningRondeGebeurtenis> item)
			{
				ScreeningRondeGebeurtenis screeningRondeGebeurtenis = item.getModelObject();
				final TypeGebeurtenis gebeurtenis = screeningRondeGebeurtenis.getGebeurtenis();

				item.add(DateLabel.forDatePattern("datum", "dd-MM-yyyy HH:mm:ss"));
				item.add(new EnumLabel<TypeGebeurtenis>("gebeurtenis"));
				item.add(new EnumLabel<GebeurtenisBron>("bron"));
				if (screeningRondeGebeurtenis.isClickable() && gebeurtenis.getDetailPanelClass() != null
					&& ScreenitSession.get().getAuthorizationStrategy().isInstantiationAuthorized(gebeurtenis.getDetailPanelClass())
					&& (!gebeurtenis.equals(TypeGebeurtenis.UITNODIGING) || item.getIndex() == 0)
					|| gebeurtenis.equals(TypeGebeurtenis.MAMMA_AFSPRAAK)
					&& ScreenitSession.get().getAuthorizationStrategy().isInstantiationAuthorized(ClientAgendaPage.class)
					|| gebeurtenis.equals(TypeGebeurtenis.MAMMA_FOLLOW_UP_PATHOLOGIE_VERSLAG)
					&& ScreenitSession.get().getAuthorizationStrategy().isInstantiationAuthorized(ClientVerslagenPage.class))
				{
					item.add(new AttributeAppender("class", new Model<>("badge-clickable"), " "));
				}
				else
				{
					item.add(new AttributeAppender("class", new Model<>("badge-not-clickable"), " "));
				}

				addExtraOmschrijvingItem(item);
				if ((gebeurtenis.equals(TypeGebeurtenis.MAMMA_AFSPRAAK))
					&& ScreenitSession.get().getAuthorizationStrategy().isInstantiationAuthorized(ClientAgendaPage.class))
				{
					item.add(new AjaxEventBehavior("click")
					{
						@Override
						protected void onEvent(AjaxRequestTarget target)
						{
							setResponsePage(new ClientAgendaPage((IModel<Client>) getPage().getDefaultModel()));
						}
					});
				}
				else if ((gebeurtenis.equals(TypeGebeurtenis.MAMMA_FOLLOW_UP_PATHOLOGIE_VERSLAG))
					&& ScreenitSession.get().getAuthorizationStrategy().isInstantiationAuthorized(ClientVerslagenPage.class))
				{
					openVerslagPage(item);
				}
				else if (screeningRondeGebeurtenis.isClickable() && gebeurtenis.getDetailPanelClass() != null
					&& ScreenitSession.get().getAuthorizationStrategy().isInstantiationAuthorized(gebeurtenis.getDetailPanelClass()))
				{
					item.add(new AjaxEventBehavior("click")
					{

						@Override
						protected void onEvent(AjaxRequestTarget target)
						{
							dialog.setCloseCallback((IDialogCloseCallback) target1 ->
							{
								ListView<ScreeningRondeGebeurtenissen> list = getGebeurtenissenContainer(clientDossierFilter);
								WebMarkupContainer nieuwGebCont = new WebMarkupContainer("gebeurtenissenContainer");
								nieuwGebCont.setOutputMarkupId(true);
								nieuwGebCont.add(list);
								gebeurtenissenContainer.replaceWith(nieuwGebCont);
								gebeurtenissenContainer = nieuwGebCont;
								target1.add(gebeurtenissenContainer);
							});
							dialog.openWith(target, new GebeurtenisPopupBasePanel(IDialog.CONTENT_ID, item.getModel()));
						}
					});
				}

				var mammaScreeningRonde = screeningRondeGebeurtenis.getScreeningRondeGebeurtenissen().getScreeningRonde();
				if (gebeurtenis.equals(TypeGebeurtenis.AFGEROND))
				{
					item.add(new AttributeAppender("class", " badge-inverse"));
				}
				if (!mammaScreeningRonde.getStatus().equals(ScreeningRondeStatus.AFGEROND) && item.getIndex() == 0)
				{
					item.add(new AttributeAppender("class", " badge-success"));
				}
			}
		});
	}

	private void openVerslagPage(ListItem<ScreeningRondeGebeurtenis> item)
	{
		item.add(new AjaxEventBehavior("click")
		{

			@Override
			protected void onEvent(AjaxRequestTarget target)
			{
				var verslag = item.getModelObject().getVerslag();
				setResponsePage(new ClientVerslagPage(ModelUtil.dModel(verslag)));
			}
		});
	}

	private void addExtraOmschrijvingItem(final ListItem<ScreeningRondeGebeurtenis> item)
	{
		item.add(new Label("extraOmschrijving", (IModel<String>) () ->
		{
			ScreeningRondeGebeurtenis screeningRondeGebeurtenis = item.getModelObject();
			String[] extraOmschrijvingen = screeningRondeGebeurtenis.getExtraOmschrijving();
			return BriefOmschrijvingUtil.verwerkExtraOmschrijvingen(extraOmschrijvingen, ClientDossierPanel.this::getString);
		})
		{
			@Override
			protected void onConfigure()
			{
				super.onConfigure();
				setVisible(item.getModelObject().getExtraOmschrijving() != null);
			}

		});
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(dossierModel);
	}

	private void logAction(LogGebeurtenis gebeurtenis, Client client)
	{
		logService.logGebeurtenis(gebeurtenis, ScreenitSession.get().getLoggedInAccount(), client);
	}

	private WebMarkupContainer getProjectBadge(Client client, Bevolkingsonderzoek onderzoek)
	{
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
			}
			else
			{
				label.add(new AttributeAppender("class", Model.of("status-inactief"), " "));
				label.add(new AttributeAppender("class", Model.of("badge-inactief"), " "));
			}
			label.add(new AttributeAppender("class", "badge", " "));
			projectBadges.add(label);
		}
		if (projectClienten.isEmpty())
		{
			projectBadges.setVisible(false);
			projectBadges.add(new EmptyPanel("isActiefInProjectLabel"));
		}
		return projectBadges;
	}

}
