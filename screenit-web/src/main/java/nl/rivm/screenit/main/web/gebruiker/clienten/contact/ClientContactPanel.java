
package nl.rivm.screenit.main.web.gebruiker.clienten.contact;

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

import java.io.Serializable;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import nl.rivm.screenit.main.service.ClientContactService;
import nl.rivm.screenit.main.service.ClientDossierFilter;
import nl.rivm.screenit.main.service.ExtraOpslaanKey;
import nl.rivm.screenit.main.service.mamma.MammaTijdNietBeschikbaarException;
import nl.rivm.screenit.main.util.EnumStringUtil;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.base.BasePage;
import nl.rivm.screenit.main.web.component.ScreenitIndicatingAjaxSubmitLink;
import nl.rivm.screenit.main.web.component.ScreenitNoBordersForm;
import nl.rivm.screenit.main.web.component.form.FilterBvoFormPanel;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.component.modal.IDialog;
import nl.rivm.screenit.main.web.component.table.AjaxLinkTableCellPanel;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.main.web.gebruiker.clienten.ClientContactActieTypeWrapper;
import nl.rivm.screenit.main.web.gebruiker.clienten.ClientPage;
import nl.rivm.screenit.main.web.gebruiker.clienten.ClientPaspoortPanel;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientContact;
import nl.rivm.screenit.model.ClientContactActie;
import nl.rivm.screenit.model.ClientContactActieType;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.MammaUitstel;
import nl.rivm.screenit.service.AutorisatieService;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.util.ExceptionConverter;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import nl.topicuszorg.wicket.search.column.DateTimePropertyColumn;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.reflect.ConstructorUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.wicket.AttributeModifier;
import org.apache.wicket.MetaDataKey;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormChoiceComponentUpdatingBehavior;
import org.apache.wicket.extensions.markup.html.repeater.data.grid.ICellPopulator;
import org.apache.wicket.extensions.markup.html.repeater.data.table.AbstractColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.EnumLabel;
import org.apache.wicket.markup.html.basic.MultiLineLabel;
import org.apache.wicket.markup.html.form.Check;
import org.apache.wicket.markup.html.form.CheckGroup;
import org.apache.wicket.markup.html.form.TextArea;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.markup.html.panel.EmptyPanel;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.markup.repeater.RepeatingView;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.request.cycle.RequestCycle;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.validation.validator.StringValidator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ClientContactPanel extends GenericPanel<Client>
{

	private static final Logger LOGGER = LoggerFactory.getLogger(ClientContactPanel.class);

	private static final long serialVersionUID = 1L;

	private final BootstrapDialog dialog;

	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private ClientContactService clientContactService;

	@SpringBean
	private ClientService clientService;

	@SpringBean
	private AutorisatieService autorisatieService;

	private IModel<ClientDossierFilter> zoekObjectModel = null;

	private ContactForm contactForm;

	private ScreenitDataTable<ClientContact, String> historie;

	private WebMarkupContainer vervolgactiesGenContainer;

	private WebMarkupContainer vervolgactiesColonContainer;

	private WebMarkupContainer vervolgactiesCervixContainer;

	private WebMarkupContainer vervolgactiesMammaContainer;

	private TextArea<String> opmerkingTextarea;

	public static final class ClientContactPanelCreateContext implements Serializable
	{
		private static final long serialVersionUID = 1L;

		public boolean bkVanuitPlanning = false;

		public boolean bkAlleenClientContact = false;

	}

	public static final MetaDataKey<ClientContactPanelCreateContext> CREATE_CONTEXT_KEY = new MetaDataKey<ClientContactPanelCreateContext>()
	{
		private static final long serialVersionUID = 1L;
	};

	public ClientContactPanel(String id, IModel<Client> clientModel, final List<Object> extraPanelParams, final ClientContactActieTypeWrapper... defaultSelectedActies)
	{
		super(id, clientModel);

		add(new ClientPaspoortPanel("passpoort", clientModel));

		if (!ScreenitSession.get().isZoekObjectGezetForComponent(ClientPage.class))
		{
			List<Bevolkingsonderzoek> bevolkingsonderzoeken = ScreenitSession.get().getLoggedInInstellingGebruiker().getBevolkingsonderzoeken();
			zoekObjectModel = Model.of(new ClientDossierFilter(new ArrayList<>(bevolkingsonderzoeken), Boolean.TRUE));
			ScreenitSession.get().setZoekObject(ClientPage.class, zoekObjectModel);
		}
		else
		{
			zoekObjectModel = (IModel<ClientDossierFilter>) ScreenitSession.get().getZoekObject(ClientPage.class);
		}

		dialog = new BootstrapDialog("dialog"); 
		add(dialog);

		contactForm = new ContactForm("form", ModelUtil.cModel(new ClientContact()), extraPanelParams, defaultSelectedActies);
		contactForm.setVisible(!clientService.isClientOverleden(clientModel.getObject()));
		add(contactForm);

		filter();

		historie = historie();
		add(historie);

	}

	private void filter()
	{
		FilterBvoFormPanel<ClientDossierFilter> bvoFilter = new FilterBvoFormPanel<ClientDossierFilter>("bvoFilterContainer", zoekObjectModel, true)
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void doFilter(IModel<ClientDossierFilter> filterModel, AjaxRequestTarget target)
			{
				contactForm.filterBVOs(target);
				refreshHistory(target);
				BasePage.markeerFormulierenOpgeslagen(target);
				ScreenitSession.get().setZoekObject(ClientContactPanel.class, filterModel);
			}

		};
		add(bvoFilter);
	}

	private void refreshHistory(AjaxRequestTarget target)
	{
		ScreenitDataTable<ClientContact, String> nieuweHistorie = historie();
		historie.replaceWith(nieuweHistorie);
		historie = nieuweHistorie;
		target.add(historie);
	}

	private ScreenitDataTable<ClientContact, String> historie()
	{
		ClientContactDataProvider provider = new ClientContactDataProvider(this.getModel(), zoekObjectModel.getObject().getBevolkingsonderzoeken());

		List<IColumn<ClientContact, String>> columns = new ArrayList<>();
		columns.add(new DateTimePropertyColumn<ClientContact, String>(Model.of("Datum / tijd"), "datum", "datum"));
		columns.add(new PropertyColumn<ClientContact, String>(Model.of("Medewerker"), "medewerker.achternaam", "instellingGebruiker.medewerker.naamVolledig"));
		columns.add(new PropertyColumn<ClientContact, String>(Model.of("Opmerking"), "opmerking"));
		columns.add(new AbstractColumn<ClientContact, String>(Model.of("Vervolgstap(pen)"))
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void populateItem(Item<ICellPopulator<ClientContact>> cellItem, String componentId, IModel<ClientContact> rowModel)
			{
				List<ClientContactActie> acties = ModelUtil.nullSafeGet(rowModel).getActies();
				StringBuilder actiesList = new StringBuilder();
				for (ClientContactActie actie : acties)
				{
					ClientContactActieType actieType = actie.getType();
					if (actieType != null)
					{
						actiesList.append(getString(EnumStringUtil.getPropertyString(actieType)));
						if (actieType.equals(ClientContactActieType.OPNIEUW_AANVRAGEN_CLIENTGEGEVENS))
						{
							actiesList.append(" (reden: " + getString(EnumStringUtil.getPropertyString(actie.getOpnieuwAanvragenClientgegevensReden())) + ")");
						}
						actiesList.append("\n");
					}
				}
				cellItem.add(new MultiLineLabel(componentId, actiesList.toString()));
			}

		});

		if (ScreenitSession.get().checkPermission(Recht.GEBRUIKER_CLIENT_CONTACT, Actie.AANPASSEN, getModelObject()))
		{
			columns.add(new AbstractColumn<ClientContact, String>(Model.of(""))
			{

				private static final long serialVersionUID = 1L;

				@Override
				public void populateItem(Item<ICellPopulator<ClientContact>> cellItem, String componentId, IModel<ClientContact> rowModel)
				{
					if (rowModel.getObject()
						.getActies()
						.stream()
						.map(ClientContactActie::getType)
						.anyMatch(ClientContactActieType.CERVIX_DEELNAME_BUITEN_BVO_BMHK::equals))
					{
						cellItem.add(new EmptyPanel(componentId));
						return;
					}
					;
					cellItem.add(new AjaxLinkTableCellPanel<ClientContact>(componentId, rowModel, "correctie")
					{

						private static final long serialVersionUID = 1L;

						@Override
						protected void onClick(AjaxRequestTarget target, IModel<ClientContact> iModel)
						{
							dialog.openWith(target, new ClientContactEditPanel(IDialog.CONTENT_ID, ModelUtil.cModel(iModel.getObject()))
							{

								private static final long serialVersionUID = 1L;

								@Override
								protected void close(AjaxRequestTarget target)
								{
									dialog.close(target);
									refreshHistory(target);
								}

							});
						}
					});
				}

			});
		}
		ScreenitDataTable<ClientContact, String> dataTable = new ScreenitDataTable<ClientContact, String>("contacten", columns, provider, Model.of("contacten"))
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected boolean isRowClickable(IModel<ClientContact> model)
			{
				return false;
			}
		};

		dataTable.setOutputMarkupId(true);
		return dataTable;
	}

	private class ContactForm extends ScreenitNoBordersForm<ClientContact>
	{

		private static final long serialVersionUID = 1L;

		private Map<ClientContactActieTypeWrapper, Panel> actiePanelsCache = new HashMap<>();

		private List<ClientContactActieTypeWrapper> selectedActies = new ArrayList<>();

		private List<ClientContactActieTypeWrapper> genActies = new ArrayList<>();

		private List<ClientContactActieTypeWrapper> colonActies = new ArrayList<>();

		private List<ClientContactActieTypeWrapper> cervixActies = new ArrayList<>();

		private List<ClientContactActieTypeWrapper> mammaActies = new ArrayList<>();

		private CheckGroup<ClientContactActieType> vervolgactiesGroup;

		public ContactForm(String id, IModel<ClientContact> model, List<Object> extraPanelParams, ClientContactActieTypeWrapper... defaultSelectedActies)
		{
			super(id, model);
			setMultiPart(true);

			if (defaultSelectedActies != null)
			{
				selectedActies.addAll(Arrays.asList(defaultSelectedActies));
			}

			Client client = ClientContactPanel.this.getModelObject();

			List<ClientContactActieType> availableActieTypes = clientContactService.getAvailableActies(client);

			for (ClientContactActieTypeWrapper actie : ClientContactActieTypeWrapper.values())
			{
				if (actie.getRecht() != null && ScreenitSession.get().checkPermission(actie.getRecht(), null) && availableActieTypes.contains(actie.getType()))
				{
					List<Bevolkingsonderzoek> actieBvos = actie.getType().getBevolkingsonderzoeken();
					if (Bevolkingsonderzoek.heeftAlleBevolkingsonderzoeken(actieBvos))
					{
						genActies.add(actie);
					}
					else if (Bevolkingsonderzoek.alleenDarmkanker(actieBvos))
					{
						colonActies.add(actie);
					}
					else if (Bevolkingsonderzoek.alleenBaarmoederhalskanker(actieBvos))
					{
						cervixActies.add(actie);
					}
					else if (Bevolkingsonderzoek.alleenBorstkanker(actieBvos))
					{
						mammaActies.add(actie);
					}
				}
			}
			genActies.add(ClientContactActieTypeWrapper.GEEN);

			add(opmerkingTextarea = new TextArea<>("opmerking"));
			opmerkingTextarea.add(StringValidator.maximumLength(2048));
			opmerkingTextarea.setOutputMarkupId(true);
			opmerkingTextarea.setOutputMarkupPlaceholderTag(true);

			vervolgactiesGroup = new CheckGroup<>("vervolgacties", new PropertyModel<List<ClientContactActieType>>(this, "vervolgacties"));
			add(vervolgactiesGroup);
			vervolgactiesGroup.setOutputMarkupId(true);
			vervolgactiesGroup.add(new AjaxFormChoiceComponentUpdatingBehavior()
			{

				private static final long serialVersionUID = 1L;

				@Override
				protected void onUpdate(AjaxRequestTarget target)
				{
					updateVervolgstappen(target);
					target.add(vervolgactiesGroup);
				}
			});

			vervolgactiesGenContainer = new WebMarkupContainer("vervolgactiesGen");
			vervolgactiesGroup.add(vervolgactiesGenContainer);
			vervolgactiesGenContainer.add(new ActieListView("vervolgactie", genActies));

			vervolgactiesColonContainer = new WebMarkupContainer("vervolgactiesColon");
			vervolgactiesGroup.add(vervolgactiesColonContainer);
			vervolgactiesColonContainer.add(new ActieListView("vervolgactie", colonActies));

			vervolgactiesCervixContainer = new WebMarkupContainer("vervolgactiesCervix");
			vervolgactiesGroup.add(vervolgactiesCervixContainer);
			vervolgactiesCervixContainer.add(new ActieListView("vervolgactie", cervixActies));

			vervolgactiesMammaContainer = new WebMarkupContainer("vervolgactiesMamma");
			vervolgactiesGroup.add(vervolgactiesMammaContainer);
			vervolgactiesMammaContainer.add(new ActieListView("vervolgactie", mammaActies));

			updateBvoVisibility();

			createInitialActiePanels(extraPanelParams);

			add(new ScreenitIndicatingAjaxSubmitLink("afronden", this)
			{

				private static final long serialVersionUID = 1L;

				@Override
				protected void onSubmit(AjaxRequestTarget target)
				{
					for (Entry<ClientContactActieTypeWrapper, Panel> entry : actiePanelsCache.entrySet())
					{
						if (selectedActies.contains(entry.getKey()))
						{
							Panel panel = entry.getValue();
							if (panel instanceof AbstractClientContactActiePanel)
							{
								((AbstractClientContactActiePanel<?>) panel).validate();
							}

							if (entry.getKey().equals(ClientContactActieTypeWrapper.GEEN) && StringUtils.isBlank(getModelObject().getOpmerking()))
							{
								error("Om het contact af te ronden moet een opmerking geplaatst worden.");
							}
						}
					}

					if (!hasError())
					{
						ArrayList<String> meldingen = new ArrayList<String>();

						for (Entry<ClientContactActieTypeWrapper, Panel> entry : actiePanelsCache.entrySet())
						{
							if (selectedActies.contains(entry.getKey()))
							{
								Panel panel = entry.getValue();
								if (panel instanceof AbstractClientContactActiePanel)
								{
									meldingen.addAll(((AbstractClientContactActiePanel<?>) panel).getOpslaanMeldingen());
								}
							}
						}
						Client client = ClientContactPanel.this.getModelObject();
						if (selectedActies.contains(ClientContactActieTypeWrapper.MAMMA_MINDER_VALIDE_ONDERZOEK_ZIEKENHUIS))
						{
							if (clientContactService.heeftOpenMammaAfspraak(client))
							{
								meldingen.add(getString("mv.onderzoek.in.ziekenhuis.afspraak.annuleren"));
							}
							MammaUitstel laatsteUitstel = client.getMammaDossier().getLaatsteScreeningRonde().getLaatsteUitstel();
							if (laatsteUitstel != null && laatsteUitstel.getGeannuleerdOp() == null && laatsteUitstel.getUitnodiging() == null)
							{
								meldingen.add(getString("mv.onderzoek.in.ziekenhuis.uitstel.annuleren"));
							}
						}
						if (meldingen.size() > 0)
						{
							dialog.openWith(target, new ClientContactAfrondenPopupPanel(IDialog.CONTENT_ID, meldingen)
							{

								private static final long serialVersionUID = 1L;

								@Override
								protected void opslaan(AjaxRequestTarget target)
								{
									contactAfronden(target);
								}
							});
						}
						else
						{
							contactAfronden(target);
						}
					}
				}
			});
		}

		private void createInitialActiePanels(List<Object> extraPanelParams)
		{
			RepeatingView actiePanels = new RepeatingView("actiePanels");

			List<ClientContactActieTypeWrapper> acties = new ArrayList<>();
			acties.addAll(genActies);
			acties.addAll(colonActies);
			acties.addAll(cervixActies);
			acties.addAll(mammaActies);

			extraPanelParams.add(dialog);
			for (ClientContactActieTypeWrapper actie : acties)
			{
				Panel actiePanel = null;
				IModel<ClientContactActie> contactActieModel = ModelUtil.cModel(new ClientContactActie(actie.getType()));
				if (selectedActies.contains(actie) && actie.getPanelClass() != null && actie.getType() != null)
				{

					Object[] initArgs = new Object[] { actiePanels.newChildId(), contactActieModel, ClientContactPanel.this.getModel(), extraPanelParams };
					try
					{
						actiePanel = (Panel) ConstructorUtils.invokeConstructor(actie.getPanelClass(), initArgs);

					}
					catch (NoSuchMethodException | IllegalAccessException | InvocationTargetException | InstantiationException e)
					{
						LOGGER.error("Fout bij aanmaken panel voor actie " + actie.getType().toString(), e);
					}
				}
				else
				{
					actiePanel = new EmptyPanel(actiePanels.newChildId());
					actiePanel.setDefaultModel(contactActieModel);
				}
				actiePanel.setOutputMarkupId(true);
				actiePanelsCache.put(actie, actiePanel);
				actiePanels.add(actiePanel);
			}
			add(actiePanels);
		}

		private void updateVervolgstappen(AjaxRequestTarget target)
		{
			List<ClientContactActieTypeWrapper> acties = new ArrayList<>();
			acties.addAll(genActies);
			acties.addAll(colonActies);
			acties.addAll(cervixActies);
			acties.addAll(mammaActies);

			List<Object> extraPanelParams = new ArrayList<>();
			extraPanelParams.add(dialog);

			for (ClientContactActieTypeWrapper typeWrapper : acties)
			{
				Panel actiePanel = actiePanelsCache.get(typeWrapper);
				Panel newActiePanel = null;

				if (actiePanel instanceof EmptyPanel && selectedActies.contains(typeWrapper) && typeWrapper.getPanelClass() != null && typeWrapper.getType() != null)
				{
					IModel<ClientContactActie> contactActieModel = ModelUtil.cModel(new ClientContactActie(typeWrapper.getType()));

					Object[] initArgs = new Object[] { actiePanel.getId(), contactActieModel, ClientContactPanel.this.getModel(), extraPanelParams };
					try
					{
						newActiePanel = (Panel) ConstructorUtils.invokeConstructor(typeWrapper.getPanelClass(), initArgs);
					}
					catch (NoSuchMethodException | IllegalAccessException | InvocationTargetException | InstantiationException e)
					{
						LOGGER.error("Fout bij aanmaken panel voor actie " + typeWrapper.getType().toString(), e);
					}
				}
				else if (!(actiePanel instanceof EmptyPanel) && !selectedActies.contains(typeWrapper))
				{
					newActiePanel = new EmptyPanel(actiePanel.getId());
				}

				if (newActiePanel != null)
				{
					actiePanelsCache.put(typeWrapper, newActiePanel);
					newActiePanel.setOutputMarkupId(true);
					actiePanel.replaceWith(newActiePanel);
					target.add(newActiePanel);
				}
			}
			if ((opmerkingTextarea.isEnabled() == !isOpmerkingToegestaan()))
			{
				target.add(opmerkingTextarea.setEnabled(isOpmerkingToegestaan()));
			}
		}

		private boolean isOpmerkingToegestaan()
		{
			return !selectedActies.containsAll(Arrays.asList(ClientContactActieTypeWrapper.CERVIX_DEELNAME_BUITEN_BVO_BMHK));
		}

		@SuppressWarnings("unused")
		public List<ClientContactActieTypeWrapper> getVervolgacties()
		{
			return new ArrayList<>(selectedActies);
		}

		@SuppressWarnings("unused")
		public void setVervolgacties(List<ClientContactActieTypeWrapper> vervolgacties)
		{
			@SuppressWarnings("unchecked")
			Collection<ClientContactActieTypeWrapper> nieuweActieTypes = CollectionUtils.subtract(vervolgacties, selectedActies);

			for (ClientContactActieTypeWrapper nieuwType : nieuweActieTypes)
			{
				for (ClientContactActieTypeWrapper exclusieType : nieuwType.getExclusie())
				{
					vervolgacties.remove(exclusieType);
				}
			}

			this.selectedActies = vervolgacties;
		}

		private void filterBVOs(AjaxRequestTarget target)
		{
			updateBvoVisibility();
			target.add(vervolgactiesGroup);

			updateVervolgstappen(target);
		}

		private void updateBvoVisibility()
		{
			List<ClientContactActieTypeWrapper> currentSelectedActies = new ArrayList<>(selectedActies);
			int aantalBvosZichtbaar = 0;
			if (!colonActies.isEmpty() && zoekObjectModel.getObject().getBevolkingsonderzoeken().contains(Bevolkingsonderzoek.COLON))
			{
				vervolgactiesColonContainer.setVisible(true);
				aantalBvosZichtbaar++;
			}
			else
			{
				vervolgactiesColonContainer.setVisible(false);
				for (ClientContactActieTypeWrapper actie : currentSelectedActies)
				{
					if (Bevolkingsonderzoek.alleenDarmkanker(actie.getType().getBevolkingsonderzoeken()))
					{
						selectedActies.remove(actie);
					}
				}
			}
			if (!cervixActies.isEmpty() && zoekObjectModel.getObject().getBevolkingsonderzoeken().contains(Bevolkingsonderzoek.CERVIX))
			{
				vervolgactiesCervixContainer.setVisible(true);
				aantalBvosZichtbaar++;
			}
			else
			{
				vervolgactiesCervixContainer.setVisible(false);
				for (ClientContactActieTypeWrapper actie : currentSelectedActies)
				{
					if (Bevolkingsonderzoek.alleenBaarmoederhalskanker(actie.getType().getBevolkingsonderzoeken()))
					{
						selectedActies.remove(actie);
					}
				}
			}
			if (!mammaActies.isEmpty() && zoekObjectModel.getObject().getBevolkingsonderzoeken().contains(Bevolkingsonderzoek.MAMMA))
			{
				vervolgactiesMammaContainer.setVisible(true);
				aantalBvosZichtbaar++;
			}
			else
			{
				vervolgactiesMammaContainer.setVisible(false);
				for (ClientContactActieTypeWrapper actie : currentSelectedActies)
				{
					if (Bevolkingsonderzoek.alleenBorstkanker(actie.getType().getBevolkingsonderzoeken()))
					{
						selectedActies.remove(actie);
					}
				}
			}
			int span = 12 / (aantalBvosZichtbaar + 1);
			vervolgactiesMammaContainer.add(new AttributeModifier("class", "span" + span));
			vervolgactiesCervixContainer.add(new AttributeModifier("class", "span" + span));
			vervolgactiesColonContainer.add(new AttributeModifier("class", "span" + span));
			vervolgactiesGenContainer.add(new AttributeModifier("class", "span" + span));
		}

		private void contactAfronden(AjaxRequestTarget target)
		{
			Client client = ClientContactPanel.this.getModelObject();
			ClientContact contact = ContactForm.this.getModelObject();
			if (contact.getId() != null)
			{
				ScreenitSession.get().warn("Het contact was al afgerond en daarom is er een nieuwe contact scherm voor u geopened.");
				BasePage.markeerFormulierenOpgeslagen(RequestCycle.get().find(AjaxRequestTarget.class).get());
				contactAfgerond();
				return;
			}
			client.getContacten().add(contact);
			contact.setClient(client);
			contact.setDatum(new Date());
			List<ClientContactActie> acties = new ArrayList<>();
			Map<ClientContactActieType, Map<ExtraOpslaanKey, Object>> extraOpslaanObjecten = new HashMap<>();
			try
			{

				for (Entry<ClientContactActieTypeWrapper, Panel> entry : actiePanelsCache.entrySet())
				{
					if (selectedActies.contains(entry.getKey()))
					{
						Panel panel = entry.getValue();
						ClientContactActie actie = (ClientContactActie) panel.getDefaultModelObject();
						ClientContactActieType actieType = entry.getKey().getType();
						if (panel instanceof AbstractClientContactActiePanel)
						{
							extraOpslaanObjecten.put(actieType, ((AbstractClientContactActiePanel<?>) panel).getOpslaanObjecten());
						}
						actie.setType(actieType);
						actie.setContact(contact);
						contact.getActies().add(actie);
						acties.add(actie);
					}
				}
				if (acties.isEmpty())
				{
					error(getString("contact.geen.vervolgstappen"));
				}
				else
				{
					clientContactService.saveClientContact(contact, extraOpslaanObjecten, ScreenitSession.get().getLoggedInInstellingGebruiker());
					BasePage.markeerFormulierenOpgeslagen(target);
					contactAfgerond();
				}
			}
			catch (MammaTijdNietBeschikbaarException e)
			{
				ScreenitSession.get().error(getString("tijd.niet.beschikbaar"));
				handleContactAfrondenFout(target, contact);
			}
			catch (RuntimeException e)
			{
				ScreenitSession.get().error(getString("contact.afronden.niet.gelukt") + ExceptionConverter.getGebruikerMeldingUitTriggerMessage(e));
				handleContactAfrondenFout(target, contact);
			}
		}

		private void handleContactAfrondenFout(AjaxRequestTarget target, ClientContact contact)
		{
			hibernateService.reload(ScreenitSession.get().getLoggedInInstellingGebruiker());
			BasePage.markeerFormulierenOpgeslagen(target);
			IModel<Client> clientIModel = ClientContactPanel.this.getModel();
			contact.setInstellingGebruiker(null);
			Client client1 = hibernateService.load(Client.class, clientIModel.getObject().getId());
			contactNietAfgerond(client1);
		}

		private class ActieListView extends ListView<ClientContactActieTypeWrapper>
		{

			public ActieListView(String id, List<ClientContactActieTypeWrapper> list)
			{
				super(id, list);
			}

			@Override
			protected void populateItem(ListItem<ClientContactActieTypeWrapper> item)
			{
				Check<ClientContactActieTypeWrapper> check = new Check<>("checkbox", item.getModel());
				item.add(check);
				item.add(new EnumLabel<ClientContactActieType>("label", item.getModelObject().getType()));
			}
		}
	}

	protected void contactAfgerond()
	{
		setResponsePage(new ClientContactPage(getModel()));
	}

	protected void contactNietAfgerond(Client client)
	{
		setResponsePage(new ClientContactPage(ModelUtil.sModel(client)));
	}
}
