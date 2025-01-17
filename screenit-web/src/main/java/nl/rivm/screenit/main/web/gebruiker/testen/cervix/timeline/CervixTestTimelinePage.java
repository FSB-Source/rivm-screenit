package nl.rivm.screenit.main.web.gebruiker.testen.cervix.timeline;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

import nl.rivm.screenit.main.model.ScreeningRondeGebeurtenis;
import nl.rivm.screenit.main.model.ScreeningRondeGebeurtenissen;
import nl.rivm.screenit.main.model.TypeGebeurtenis;
import nl.rivm.screenit.main.model.testen.TestTimelineModel;
import nl.rivm.screenit.main.model.testen.TestTimelineRonde;
import nl.rivm.screenit.main.service.cervix.CervixTestTimelineService;
import nl.rivm.screenit.main.util.BriefOmschrijvingUtil;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.component.modal.IDialog;
import nl.rivm.screenit.main.web.gebruiker.clienten.dossier.GebeurtenisComparator;
import nl.rivm.screenit.main.web.gebruiker.testen.TestenBasePage;
import nl.rivm.screenit.main.web.gebruiker.testen.gedeeld.timeline.TestVervolgKeuzeOptie;
import nl.rivm.screenit.main.web.gebruiker.testen.gedeeld.timeline.components.TestEnumRadioChoice;
import nl.rivm.screenit.main.web.gebruiker.testen.gedeeld.timeline.components.TestVervolgKeuzeKnop;
import nl.rivm.screenit.main.web.gebruiker.testen.gedeeld.timeline.popups.BijzondereClientDatumPopup;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.Gemeente;
import nl.rivm.screenit.model.Gemeente_;
import nl.rivm.screenit.model.ScreeningRonde;
import nl.rivm.screenit.model.ScreeningRondeStatus;
import nl.rivm.screenit.model.cervix.CervixDossier;
import nl.rivm.screenit.model.cervix.enums.CervixAfmeldingReden;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Deelnamemodus;
import nl.rivm.screenit.model.enums.GebeurtenisBron;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.repository.algemeen.GemeenteRepository;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.cervix.CervixTestService;
import nl.rivm.screenit.specification.algemeen.GemeenteSpecification;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.TestBsnGenerator;
import nl.topicuszorg.patientregistratie.persoonsgegevens.model.Geslacht;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;
import nl.topicuszorg.wicket.hibernate.cglib.ModelProxyHelper;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import nl.topicuszorg.wicket.model.DetachableListModel;
import nl.topicuszorg.wicket.model.SortingListModel;

import org.apache.wicket.ajax.AjaxEventBehavior;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormComponentUpdatingBehavior;
import org.apache.wicket.ajax.markup.html.form.AjaxButton;
import org.apache.wicket.behavior.AttributeAppender;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.EnumLabel;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.ChoiceRenderer;
import org.apache.wicket.markup.html.form.DropDownChoice;
import org.apache.wicket.markup.html.form.EnumChoiceRenderer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.RadioChoice;
import org.apache.wicket.markup.html.form.TextArea;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.markup.html.list.PropertyListView;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.springframework.data.domain.Sort;
import org.wicketstuff.datetime.markup.html.basic.DateLabel;
import org.wicketstuff.shiro.ShiroConstraint;
import org.wicketstuff.wiquery.ui.datepicker.DatePicker;

@SecurityConstraint(
	actie = Actie.VERWIJDEREN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = Recht.TESTEN,
	bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.CERVIX })
public class CervixTestTimelinePage extends TestenBasePage
{
	@SpringBean
	private CervixTestTimelineService testTimelineService;

	@SpringBean
	private ICurrentDateSupplier dateSupplier;

	@SpringBean
	private CervixTestService testService;

	@SpringBean
	private GemeenteRepository gemeenteRepository;

	private final IModel<TestTimelineModel> model;

	private IModel<List<Client>> clientModel;

	private IModel<List<TestTimelineRonde>> rondesModel;

	private final IModel<List<Gemeente>> gemeentenModel;

	private final Form<TestTimelineModel> form;

	private WebMarkupContainer formComponents;

	private final BootstrapDialog dialog;

	private final Form<Void> clientDefinitiefAfmeldenForm;

	private IModel<CervixAfmeldingReden> afmeldingReden = new Model<>(CervixAfmeldingReden.ANDERS);

	public CervixTestTimelinePage()
	{
		dialog = new BootstrapDialog("dialog")
		{
			@Override
			public boolean fade()
			{
				return false;
			}
		};
		add(dialog);

		List<Gemeente> gemeenten = gemeenteRepository.findAll(GemeenteSpecification.heeftScreeningOrganisatie().and(GemeenteSpecification.heeftBmhkLaboratorium()),
			Sort.by(Sort.Order.asc(Gemeente_.NAAM)));

		gemeentenModel = ModelUtil.listRModel(gemeenten, false);

		TestTimelineModel testTimelineModel = new TestTimelineModel();
		testTimelineModel.setGeslacht(Geslacht.VROUW);
		testTimelineModel.setGeboortedatum(DateUtil.minusTijdseenheid(dateSupplier.getDate(), 30, ChronoUnit.YEARS));
		testTimelineModel.setLeeftijd(30);

		model = new CompoundPropertyModel<>(testTimelineModel);
		TestTimelineModel object = model.getObject();
		object.setBsn(TestBsnGenerator.getValideBsn());

		form = new Form<>("form", model);
		form.setOutputMarkupId(true);
		add(form);

		formComponents = getFormComponentsContainer();
		form.add(formComponents);

		gebeurtenissenContainer = getGebeurtenissenContainer();
		form.add(gebeurtenissenContainer);

		Form<Void> clientResetForm = new ScreenitForm<>("clientResetForm");
		final IModel<String> bsns = new Model<>("");
		clientResetForm.add(new TextArea<>("bsns", bsns).setRequired(true));
		clientResetForm.add(new AjaxButton("resetten", clientResetForm)
		{
			@Override
			public void onSubmit(AjaxRequestTarget target)
			{
				String message = testService.clientenResetten(bsns.getObject());
				if (message.contains("Succesvol"))
				{
					info(message);
				}
				else
				{
					error(message);
				}
			}
		});

		add(clientResetForm);

		clientDefinitiefAfmeldenForm = new ScreenitForm<>("definitiefAfmeldenForm")
		{
			@Override
			public boolean isVisible()
			{
				List<Client> clienten = ModelUtil.nullSafeGet(clientModel);
				return clienten != null && !clienten.isEmpty() && clienten.get(0).getCervixDossier().getCisHistorie() != null;
			}
		};
		clientDefinitiefAfmeldenForm.setOutputMarkupId(true);
		clientDefinitiefAfmeldenForm.setOutputMarkupPlaceholderTag(true);
		clientDefinitiefAfmeldenForm.add(new AjaxButton("definitiefAfmelden", clientDefinitiefAfmeldenForm)
		{
			@Override
			public void onSubmit(AjaxRequestTarget target)
			{
				int aantal = testService.clientenDefinitiefAfmelden(ModelUtil.nullSafeGet(clientModel), afmeldingReden.getObject());
				info(aantal + " clienten definitief afgemeld");
			}
		});
		List<CervixAfmeldingReden> afmeldingRedenen = new ArrayList<>(Arrays.asList(CervixAfmeldingReden.values()));
		final DropDownChoice<CervixAfmeldingReden> afmeldingRedenDropdown = new DropDownChoice<>("afmeldingReden", afmeldingReden, afmeldingRedenen);
		afmeldingRedenDropdown.setNullValid(false);
		afmeldingRedenDropdown.setOutputMarkupId(true);
		clientDefinitiefAfmeldenForm.add(afmeldingRedenDropdown);

		add(clientDefinitiefAfmeldenForm);
	}

	private WebMarkupContainer getFormComponentsContainer()
	{
		WebMarkupContainer container = new WebMarkupContainer("formComponents");
		container.setOutputMarkupId(true);

		bsnField = new TextField<>("bsn");
		bsnField.setRequired(true);
		bsnField.setOutputMarkupId(true);
		container.add(bsnField);

		Label aNummer = new Label("aNummer");
		container.add(aNummer);

		Label leeftijd = new Label("leeftijd");
		leeftijd.setOutputMarkupId(true);
		container.add(leeftijd);

		bsnField.add(new AjaxEventBehavior("change")
		{
			@Override
			protected void onEvent(AjaxRequestTarget target)
			{
				WebMarkupContainer geContainer = getGebeurtenissenContainer();
				gebeurtenissenContainer.replaceWith(geContainer);
				gebeurtenissenContainer = geContainer;
				gebeurtenissenContainer.setVisible(false);
				target.add(gebeurtenissenContainer);
			}
		});

		addClientBsnGenererenButtons(container, model);

		DatePicker<Date> geboortedatum = ComponentHelper.monthYearDatePicker("geboortedatum");
		geboortedatum.setOutputMarkupId(true);
		geboortedatum.add(new AjaxFormComponentUpdatingBehavior("change")
		{
			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				TestTimelineModel timelineModel = model.getObject();
				timelineModel.setLeeftijd(DateUtil.getLeeftijd(DateUtil.toLocalDate(timelineModel.getGeboortedatum()), dateSupplier.getLocalDate()));
				target.add(leeftijd);
			}
		});
		container.add(geboortedatum);

		List<Geslacht> geslachten = new ArrayList<>(Arrays.asList(Geslacht.values()));
		geslachten.remove(Geslacht.NIET_GESPECIFICEERD);
		RadioChoice<Geslacht> geslachtRadio = new TestEnumRadioChoice<>("geslacht", geslachten, new EnumChoiceRenderer<>(this));
		geslachtRadio.setPrefix("<label class=\"radio\">");
		geslachtRadio.setSuffix("</label>");
		geslachtRadio.setOutputMarkupId(true);
		container.add(geslachtRadio);

		container.add(new DropDownChoice<>("gemeente", gemeentenModel, new ChoiceRenderer<>("naam")));

		IndicatingAjaxSubmitLink clientVindOfMaak = new IndicatingAjaxSubmitLink("clientVindOfMaak")
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				TestTimelineModel timelineModel = model.getObject();
				List<Client> clienten = testTimelineService.maakOfVindClienten(timelineModel);
				List<String> warnings = testTimelineService.validateTestClienten(clienten);
				for (String warning : warnings)
				{
					CervixTestTimelinePage.this.warn(warning);
				}
				if (warnings.isEmpty())
				{
					submitClients(target, timelineModel, clienten);
				}
			}

			private void submitClients(AjaxRequestTarget target, TestTimelineModel timelineModel, List<Client> clienten)
			{
				clientModel = ModelUtil.listModel(clienten);
				CervixTestTimelinePage.this.info("Client(en) zijn gevonden en/of succesvol aangemaakt");

				if (!clienten.isEmpty())
				{
					refreshTimelineModel(timelineModel, clienten);
					WebMarkupContainer fCcontainer = getFormComponentsContainer();
					formComponents.replaceWith(fCcontainer);
					formComponents = fCcontainer;
					target.add(formComponents);
				}

				WebMarkupContainer geContainer = getGebeurtenissenContainer();
				gebeurtenissenContainer.replaceWith(geContainer);
				gebeurtenissenContainer = geContainer;
				target.add(gebeurtenissenContainer);
			}
		};
		form.setDefaultButton(clientVindOfMaak);
		container.add(clientVindOfMaak);

		IndicatingAjaxSubmitLink clientWijzigOfMaak = new IndicatingAjaxSubmitLink("clientWijzigOfMaak")
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				TestTimelineModel timelineModel = model.getObject();
				List<Client> clienten = testTimelineService.maakOfWijzigClienten(timelineModel);
				List<String> errors = testTimelineService.validateTestClienten(clienten);
				errors.forEach(this::error);
				clientModel = ModelUtil.listModel(clienten);
				CervixTestTimelinePage.this.info("Client(en) zijn succesvol gewijzigd of aangemaakt");

				if (!clienten.isEmpty())
				{
					refreshTimelineModel(timelineModel, clienten);
					WebMarkupContainer fCcontainer = getFormComponentsContainer();
					formComponents.replaceWith(fCcontainer);
					formComponents = fCcontainer;
					target.add(formComponents);
				}

				WebMarkupContainer geContainer = getGebeurtenissenContainer();
				gebeurtenissenContainer.replaceWith(geContainer);
				gebeurtenissenContainer = geContainer;
				target.add(gebeurtenissenContainer);
			}
		};
		container.add(clientWijzigOfMaak);
		return container;
	}

	@Override
	protected WebMarkupContainer getGebeurtenissenContainer()
	{

		WebMarkupContainer container = new WebMarkupContainer("gebeurtenissenContainer");
		container.setOutputMarkupPlaceholderTag(true);

		container.setVisible(clientModel != null);
		if (clientModel != null)
		{
			List<TestTimelineRonde> rondes = testTimelineService.getTimelineRondes(clientModel.getObject().get(0));
			rondes.sort((o1, o2) -> o2.getRondeNummer().compareTo(o1.getRondeNummer()));
			rondesModel = new DetachableListModel<>(rondes);

			container.add(new TestVervolgKeuzeKnop("nieuweRonde", clientModel, dialog)
			{
				@Override
				public boolean refreshContainer(AjaxRequestTarget target)
				{
					List<Client> clienten = testTimelineService.maakOfVindClienten(model.getObject());
					List<String> errors = testTimelineService.validateTestClienten(clienten);
					for (String error : errors)
					{
						error(error);
					}
					clientModel = ModelUtil.listModel(clienten);

					WebMarkupContainer geContainer = getGebeurtenissenContainer();
					gebeurtenissenContainer.replaceWith(geContainer);
					gebeurtenissenContainer = geContainer;
					target.add(gebeurtenissenContainer);
					target.add(clientDefinitiefAfmeldenForm);
					return true;
				}

				@Override
				public List<TestVervolgKeuzeOptie> getOptions()
				{
					var keuzes = new ArrayList<TestVervolgKeuzeOptie>();
					CervixDossier dossier = clientModel.getObject().get(0).getCervixDossier();
					if (!Deelnamemodus.SELECTIEBLOKKADE.equals(dossier.getDeelnamemodus()))
					{
						if (testTimelineService.magNieuweRondeStarten(dossier))
						{
							keuzes.add(TestVervolgKeuzeOptie.CERVIX_NIEUWE_RONDE);
							if (dossier.getLaatsteScreeningRonde() == null)
							{
								keuzes.add(TestVervolgKeuzeOptie.CERVIX_NIEUWE_RONDE_MET_VOORAANKONDIGING);
							}
						}
						keuzes.add(TestVervolgKeuzeOptie.CERVIX_NIEUWE_CISHISTORIE);
						keuzes.add(TestVervolgKeuzeOptie.CERVIX_NIEUWE_CIS_RONDE0);
						keuzes.add(TestVervolgKeuzeOptie.CERVIX_VERZET_TIJD);
					}
					else
					{
						keuzes.add(TestVervolgKeuzeOptie.CERVIX_DEELNAME_WENS);
					}
					return keuzes;
				}

				@Override
				public boolean isVisible()
				{
					GbaPersoon persoon = clientModel.getObject().get(0).getPersoon();
					boolean isOverleden = persoon.getOverlijdensdatum() != null;
					return !isOverleden;
				}

				@Override
				public String getNameAttribuut()
				{
					return "snelkeuze_ronde";
				}
			});

			ListView<TestTimelineRonde> listView = getListView();
			listView.setOutputMarkupId(true);
			container.add(listView);
		}

		addButtons(container);
		return container;
	}

	private ListView<TestTimelineRonde> getListView()
	{
		return new ListView<>("rondes", rondesModel)
		{
			@Override
			protected void populateItem(ListItem<TestTimelineRonde> item)
			{
				item.add(new Label("rondeNummer", item.getModelObject().getRondeNummer()));
				item.add(new TestVervolgKeuzeKnop("snelKeuzeCervix", clientModel, dialog)
				{
					@Override
					public List<TestVervolgKeuzeOptie> getOptions()
					{
						return testTimelineService.getSnelKeuzeOpties(ModelProxyHelper.deproxy(clientModel.getObject().get(0)));
					}

					@Override
					public boolean refreshContainer(AjaxRequestTarget target)
					{
						WebMarkupContainer geContainer = getGebeurtenissenContainer();
						gebeurtenissenContainer.replaceWith(geContainer);
						gebeurtenissenContainer = geContainer;
						target.add(gebeurtenissenContainer);
						return true;
					}

					@Override
					public boolean isVisible()
					{
						Client client = clientModel.getObject().get(0);
						GbaPersoon persoon = client.getPersoon();
						boolean isOverleden = persoon.getOverlijdensdatum() != null;
						TestTimelineRonde timeLineRonde = item.getModelObject();
						ScreeningRondeGebeurtenissen gebeurtenissen = timeLineRonde.getCervixScreeningRondeDossier();
						ScreeningRonde<?, ?, ?, ?> ronde = gebeurtenissen.getScreeningRonde();
						boolean isLopend = ScreeningRondeStatus.LOPEND.equals(ronde.getStatus());
						boolean isAangemeld = ronde.getAangemeld();
						return !isOverleden && (isLopend || !isAangemeld);
					}

					@Override
					public String getNameAttribuut()
					{
						return "snelkeuze_gebeurtenis";
					}
				});

				SortingListModel<ScreeningRondeGebeurtenis> sortingListModel = new SortingListModel<>(
					new PropertyModel<>(item.getModel(), "cervixScreeningRondeDossier.gebeurtenissen"),
					new GebeurtenisComparator());

				item.add(new PropertyListView<>("gebeurtenissen", sortingListModel)
				{
					@Override
					protected void populateItem(final ListItem<ScreeningRondeGebeurtenis> item)
					{
						item.add(DateLabel.forDatePattern("datum", "dd-MM-yyyy HH:mm:ss"));
						item.add(new EnumLabel<TypeGebeurtenis>("gebeurtenis"));
						item.add(new EnumLabel<GebeurtenisBron>("bron"));
						item.add(new AttributeAppender("class", new Model<>("badge-not-clickable"), " "));
						item.add(new Label("extraOmschrijving", (IModel<String>) () ->
						{
							ScreeningRondeGebeurtenis gebeurtenis = item.getModelObject();
							String[] extraOmschrijvingen = gebeurtenis.getExtraOmschrijving();
							return BriefOmschrijvingUtil.verwerkExtraOmschrijvingen(extraOmschrijvingen, CervixTestTimelinePage.this::getString);
						}));
					}
				});
			}
		};
	}

	private void addButtons(WebMarkupContainer container)
	{
		container.add(new AjaxButton("bijzondereClientData", form)
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				dialog.openWith(target, new BijzondereClientDatumPopup(IDialog.CONTENT_ID, clientModel)
				{
					@Override
					public void close(AjaxRequestTarget target)
					{
						dialog.close(target);
						WebMarkupContainer geContainer = getGebeurtenissenContainer();
						gebeurtenissenContainer.replaceWith(geContainer);
						gebeurtenissenContainer = geContainer;
						target.add(gebeurtenissenContainer);
						CervixTestTimelinePage.this.info("Client aangepast.");
					}
				});
			}

			@Override
			public boolean isVisible()
			{
				return clientModel.getObject().get(0).getPersoon().getOverlijdensdatum() == null;
			}
		});
		addGaNaarButtons(container, form, model);
	}

	@Override
	protected void detachModel()
	{
		super.detachModel();
		ModelUtil.nullSafeDetach(model);
		ModelUtil.nullSafeDetach(gemeentenModel);
		ModelUtil.nullSafeDetach(clientModel);
		ModelUtil.nullSafeDetach(rondesModel);
	}
}
