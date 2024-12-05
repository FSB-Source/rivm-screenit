package nl.rivm.screenit.main.web.gebruiker.testen.mamma.timeline;

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

import java.io.File;
import java.math.BigDecimal;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import nl.rivm.screenit.main.model.ScreeningRondeGebeurtenis;
import nl.rivm.screenit.main.model.TypeGebeurtenis;
import nl.rivm.screenit.main.model.testen.TestTimelineModel;
import nl.rivm.screenit.main.model.testen.TestTimelineRonde;
import nl.rivm.screenit.main.service.mamma.MammaScreeningsEenheidService;
import nl.rivm.screenit.main.service.mamma.MammaTestTimelineService;
import nl.rivm.screenit.main.util.BriefOmschrijvingUtil;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.PercentageBigDecimalField;
import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.main.web.component.form.PostcodeField;
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
import nl.rivm.screenit.model.Gemeente;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Deelnamemodus;
import nl.rivm.screenit.model.enums.GebeurtenisBron;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.enums.MammaDoelgroep;
import nl.rivm.screenit.service.GemeenteService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.TestBsnGenerator;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.patientregistratie.persoonsgegevens.model.Geslacht;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import nl.topicuszorg.wicket.model.DetachableListModel;
import nl.topicuszorg.wicket.model.SortingListModel;

import org.apache.wicket.ajax.AjaxEventBehavior;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.form.AjaxButton;
import org.apache.wicket.ajax.markup.html.form.AjaxCheckBox;
import org.apache.wicket.behavior.AttributeAppender;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxButton;
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
import org.apache.wicket.markup.html.form.upload.FileUpload;
import org.apache.wicket.markup.html.form.upload.FileUploadField;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.markup.html.list.PropertyListView;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.model.util.ListModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.datetime.markup.html.basic.DateLabel;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.VERWIJDEREN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = Recht.TESTEN,
	bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.MAMMA })
public class MammaTestTimelinePage extends TestenBasePage
{
	@SpringBean
	private MammaTestTimelineService testTimelineService;

	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private GemeenteService gemeenteService;

	@SpringBean
	private ICurrentDateSupplier dateSupplier;

	@SpringBean
	private MammaScreeningsEenheidService screeningsEenheidService;

	private final IModel<TestTimelineModel> model;

	private IModel<List<Client>> clientModel;

	private IModel<List<TestTimelineRonde>> rondesModel;

	private final IModel<List<Gemeente>> gemeentenModel;

	private final Form<TestTimelineModel> form;

	private WebMarkupContainer formComponents;

	private final BootstrapDialog dialog;

	private final IModel<List<FileUpload>> pocfilesUploaded = new ListModel<>();

	private IModel<MammaScreeningsEenheid> screeningsEenheid;

	private final IModel<ImportPocOpties> importPocOptiesIModel;

	public MammaTestTimelinePage()
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

		var gemeenten = gemeenteService.getGemeentesMetScreeningOrganisatie();

		gemeentenModel = ModelUtil.listRModel(gemeenten, false);

		var testTimelineModel = new TestTimelineModel();
		testTimelineModel.setGeslacht(Geslacht.VROUW);
		testTimelineModel.setGeboortedatum(DateUtil.minusTijdseenheid(dateSupplier.getDate(), 50, ChronoUnit.YEARS));
		testTimelineModel.setDeelnamekans(BigDecimal.valueOf(1));
		testTimelineModel.setDoelgroep(MammaDoelgroep.REGULIER);
		testTimelineModel.setPostcode("1234AA");

		model = new CompoundPropertyModel<>(testTimelineModel);
		var object = model.getObject();
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
		clientResetForm.add(new IndicatingAjaxButton("resetten", clientResetForm)
		{
			@Override
			public void onSubmit(AjaxRequestTarget target)
			{
				var message = testTimelineService.clientenResetten(bsns.getObject());
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

		Form<Void> pocClienten = new ScreenitForm<>("pocClienten");
		pocClienten.add(new FileUploadField("csv", pocfilesUploaded));
		IModel<List<MammaScreeningsEenheid>> listRModel = ModelUtil
			.listRModel(screeningsEenheidService.getActieveScreeningsEenhedenVoorScreeningOrganisatie(ScreenitSession.get().getScreeningOrganisatie()));
		var screeningsEenheidDropDown = ComponentHelper.newDropDownChoice("screeningsEenheid", listRModel, new ChoiceRenderer<>("naam"), true);

		screeningsEenheidDropDown.setModel(new PropertyModel<>(this, "screeningsEenheid"));
		pocClienten.add(screeningsEenheidDropDown);

		importPocOptiesIModel = new Model<>();
		var laatsteOnderzoekenKlaarzettenVoorSe = ComponentHelper.newDropDownChoice("laatsteRonde",
			new ListModel<>(Arrays.asList(ImportPocOpties.values())), new EnumChoiceRenderer<>(), true);
		laatsteOnderzoekenKlaarzettenVoorSe.setModel(importPocOptiesIModel);
		pocClienten.add(laatsteOnderzoekenKlaarzettenVoorSe);

		pocClienten.add(new IndicatingAjaxButton("import", pocClienten)
		{
			@Override
			public void onSubmit(AjaxRequestTarget target)
			{
				File file = null;
				var aantal = 0;
				try
				{
					var upload = pocfilesUploaded.getObject().get(0);
					file = upload.writeToTempFile();
					aantal = testTimelineService.importPocClienten(file, ScreenitSession.get().getLoggedInInstellingGebruiker(), getScreeningsEenheid(),
						importPocOptiesIModel.getObject());
				}
				catch (Exception e)
				{
					error("Er is een fout opgetreden: " + e.getMessage());
				}
				finally
				{
					if (file != null && file.exists())
					{
						file.delete();
					}
				}
				info(aantal + " onderzoeken voor de PoC geimporteerd.");
			}
		});

		add(pocClienten);

		add(new MammaTestBulkDeelnamekansPanel("deelnamekansen"));

		add(new MammaTestSluitDagenSEPanel("sluitDagenSE"));

		add(new MammaTestClientenMetBeeldenBeschikbaarPanel("clientenMetBeeldenBeschikbaar"));
	}

	private WebMarkupContainer getFormComponentsContainer()
	{
		var container = new WebMarkupContainer("formComponents");
		container.setOutputMarkupId(true);

		bsnField = new TextField<>("bsn");
		bsnField.setRequired(true);
		bsnField.setOutputMarkupId(true);
		container.add(bsnField);

		var aNummer = new Label("aNummer");
		container.add(aNummer);

		bsnField.add(new AjaxEventBehavior("change")
		{
			@Override
			protected void onEvent(AjaxRequestTarget target)
			{
				var geContainer = getGebeurtenissenContainer();
				gebeurtenissenContainer.replaceWith(geContainer);
				gebeurtenissenContainer = geContainer;
				gebeurtenissenContainer.setVisible(false);
				target.add(gebeurtenissenContainer);
			}
		});

		addClientBsnGenererenButtons(container, model);

		var geboortedatum = ComponentHelper.monthYearDatePicker("geboortedatum");
		geboortedatum.setOutputMarkupId(true);
		container.add(geboortedatum);

		List<Geslacht> geslachten = new ArrayList<>(Arrays.asList(Geslacht.values()));
		geslachten.remove(Geslacht.NIET_GESPECIFICEERD);
		RadioChoice<Geslacht> geslachtRadio = new TestEnumRadioChoice<>("geslacht", geslachten, new EnumChoiceRenderer<>(this));
		geslachtRadio.setPrefix("<label class=\"radio\">");
		geslachtRadio.setSuffix("</label>");
		geslachtRadio.setOutputMarkupId(true);
		container.add(geslachtRadio);

		TextField<BigDecimal> deelnamekansField = new PercentageBigDecimalField("deelnamekans");

		container.add(deelnamekansField);

		container.add(new DropDownChoice<>("gemeente", gemeentenModel, new ChoiceRenderer<>("naam")));

		container.add(new DropDownChoice<>("doelgroep", Arrays.asList(MammaDoelgroep.values()), new EnumChoiceRenderer<>()).setNullValid(true));

		container.add(new PostcodeField("postcode"));

		var clientVindOfMaak = new IndicatingAjaxSubmitLink("clientVindOfMaak")
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				var timelineModel = model.getObject();
				if ((timelineModel.getDoelgroep() != null) != (timelineModel.getDeelnamekans() != null))
				{
					error("Als de doelgroep ingevuld is moet ook de deelnamekans ingevuld worden en vice versa");
					return;
				}
				var clienten = testTimelineService.maakOfVindClienten(timelineModel);
				var errors = testTimelineService.validateTestClienten(clienten);
				for (var error : errors)
				{
					MammaTestTimelinePage.this.error(error);
				}
				clientModel = ModelUtil.listModel(clienten);
				MammaTestTimelinePage.this.info("Client(en) zijn gevonden en/of succesvol aangemaakt");

				if (!clienten.isEmpty())
				{
					refreshTimelineModel(timelineModel, clienten);
					var fCcontainer = getFormComponentsContainer();
					formComponents.replaceWith(fCcontainer);
					formComponents = fCcontainer;
					target.add(formComponents);
				}

				var geContainer = getGebeurtenissenContainer();
				gebeurtenissenContainer.replaceWith(geContainer);
				gebeurtenissenContainer = geContainer;
				target.add(gebeurtenissenContainer);
			}
		};
		form.setDefaultButton(clientVindOfMaak);
		container.add(clientVindOfMaak);
		var clientWijzigOfMaak = new IndicatingAjaxSubmitLink("clientWijzigOfMaak")
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				var timelineModel = model.getObject();
				if ((timelineModel.getDoelgroep() == null) == (timelineModel.getDeelnamekans() != null))
				{
					error("Als de doelgroep ingevuld is moet ook de deelnamekans ingevuld worden en vice versa");
					return;
				}
				var clienten = testTimelineService.maakOfWijzigClienten(timelineModel);
				var errors = testTimelineService.validateTestClienten(clienten);
				for (var error : errors)
				{
					MammaTestTimelinePage.this.error(error);
				}
				clientModel = ModelUtil.listModel(clienten);
				MammaTestTimelinePage.this.info("Client(en) zijn succesvol gewijzigd of aangemaakt");

				if (!clienten.isEmpty())
				{
					refreshTimelineModel(timelineModel, clienten);
					var fCcontainer = getFormComponentsContainer();
					formComponents.replaceWith(fCcontainer);
					formComponents = fCcontainer;
					target.add(formComponents);
				}

				var geContainer = getGebeurtenissenContainer();
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
		var container = new WebMarkupContainer("gebeurtenissenContainer");
		container.setOutputMarkupPlaceholderTag(true);

		container.setVisible(clientModel != null);
		if (clientModel != null)
		{
			reloadClienten();
			var rondes = testTimelineService.getTimelineRondes(clientModel.getObject().get(0));
			rondes.sort((o1, o2) -> o2.getRondeNummer().compareTo(o1.getRondeNummer()));
			rondesModel = new DetachableListModel<>(rondes);

			var snelkeuzeRonde = new TestVervolgKeuzeKnop("nieuweRonde", clientModel, dialog)
			{
				@Override
				public boolean refreshContainer(AjaxRequestTarget target)
				{
					var clienten = testTimelineService.maakOfVindClienten(model.getObject());
					var errors = testTimelineService.validateTestClienten(clienten);
					for (var error : errors)
					{
						error(error);
					}
					clientModel = ModelUtil.listModel(clienten);

					var geContainer = getGebeurtenissenContainer();
					gebeurtenissenContainer.replaceWith(geContainer);
					gebeurtenissenContainer = geContainer;
					target.add(gebeurtenissenContainer);
					return true;
				}

				@Override
				public List<TestVervolgKeuzeOptie> getOptions()
				{
					List<TestVervolgKeuzeOptie> keuzes = new ArrayList<>();
					if (!Deelnamemodus.SELECTIEBLOKKADE.equals(clientModel.getObject().get(0).getMammaDossier().getDeelnamemodus()))
					{
						keuzes.add(TestVervolgKeuzeOptie.MAMMA_NIEUWE_RONDE_MET_AFSPRAAK_UITNODIGING);
						keuzes.add(TestVervolgKeuzeOptie.MAMMA_NIEUWE_RONDE_MET_OPEN_UITNODIGING);
					}
					else
					{
						keuzes.add(TestVervolgKeuzeOptie.MAMMA_DEELNAME_WENS);
					}
					if (clientModel.getObject().get(0).getMammaDossier().getLaatsteScreeningRonde() != null)
					{
						keuzes.add(TestVervolgKeuzeOptie.MAMMA_VERZET_TIJD);
					}
					return keuzes;
				}

				@Override
				public boolean isVisible()
				{
					var persoon = clientModel.getObject().get(0).getPersoon();
					var isOverleden = persoon.getOverlijdensdatum() != null;
					return !isOverleden;
				}

				@Override
				public String getNameAttribuut()
				{
					return "snelkeuze_ronde";
				}
			};
			container.add(snelkeuzeRonde);

			var listView = getListView();
			listView.setOutputMarkupId(true);
			container.add(listView);
		}

		addbuttons(container);
		return container;
	}

	private void reloadClienten()
	{
		List<Client> reloadedClienten = new ArrayList<>();
		for (var client : clientModel.getObject())
		{
			reloadedClienten.add(hibernateService.load(Client.class, client.getId()));
		}
		clientModel = ModelUtil.listModel(reloadedClienten);
	}

	private ListView<TestTimelineRonde> getListView()
	{
		return new ListView<>("rondes", rondesModel)
		{
			@Override
			protected void populateItem(ListItem<TestTimelineRonde> item)
			{
				item.add(new Label("rondeNummer", item.getModelObject().getRondeNummer()));
				item.add(new TestVervolgKeuzeKnop("snelKeuzeMamma", clientModel, dialog)
				{

					@Override
					public List<TestVervolgKeuzeOptie> getOptions()
					{
						return testTimelineService.getSnelKeuzeOpties(clientModel.getObject().get(0));
					}

					@Override
					public boolean refreshContainer(AjaxRequestTarget target)
					{
						var geContainer = getGebeurtenissenContainer();
						gebeurtenissenContainer.replaceWith(geContainer);
						gebeurtenissenContainer = geContainer;
						target.add(gebeurtenissenContainer);
						return true;
					}

					@Override
					public boolean isVisible()
					{
						var client = clientModel.getObject().get(0);
						var timeLineRonde = item.getModelObject();
						return testTimelineService.isSnelkeuzeKnopMammaBeschikbaar(client, timeLineRonde);
					}

					@Override
					public String getNameAttribuut()
					{
						return "snelkeuze_gebeurtenis";
					}
				});
				var sortingListModel = new SortingListModel<ScreeningRondeGebeurtenis>(
					new PropertyModel<>(item.getModel(), "mammaScreeningRondeDossier.gebeurtenissen"), new GebeurtenisComparator());

				item.add(new PropertyListView<>("gebeurtenissen", sortingListModel)
				{

					@Override
					protected void populateItem(final ListItem<ScreeningRondeGebeurtenis> item)
					{
						item.add(DateLabel.forDatePattern("datum", "dd-MM-yyyy HH:mm:ss"));
						item.add(new EnumLabel<TypeGebeurtenis>("gebeurtenis"));
						item.add(new EnumLabel<GebeurtenisBron>("bron"));
						item.add(new AttributeAppender("class", new Model<String>("badge-not-clickable"), " "));
						item.add(new Label("extraOmschrijving", new IModel<String>()
						{

							@Override
							public String getObject()
							{
								var gebeurtenis2 = item.getModelObject();
								var extraOmschrijvingen = gebeurtenis2.getExtraOmschrijving();
								return BriefOmschrijvingUtil.verwerkExtraOmschrijvingen(extraOmschrijvingen, MammaTestTimelinePage.this::getString);
							}
						}));
					}
				});
			}
		};
	}

	private void addbuttons(WebMarkupContainer container)
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
						var geContainer = getGebeurtenissenContainer();
						gebeurtenissenContainer.replaceWith(geContainer);
						gebeurtenissenContainer = geContainer;
						target.add(gebeurtenissenContainer);
						info("Client aangepast.");
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
		final var verstuurHl7BerichtenCheckbox = new AjaxCheckBox("verstuurHl7Berichten", verstuurHl7Berichten)
		{
			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{

			}
		};
		container.add(verstuurHl7BerichtenCheckbox);
	}

	private Model<Boolean> verstuurHl7Berichten = Model.of(false);

	public Model<Boolean> getVerstuurHl7Berichten()
	{
		return verstuurHl7Berichten;
	}

	public void setVerstuurHl7Berichten(Model<Boolean> verstuurHl7Berichten)
	{
		this.verstuurHl7Berichten = verstuurHl7Berichten;
	}

	public MammaScreeningsEenheid getScreeningsEenheid()
	{
		return ModelUtil.nullSafeGet(screeningsEenheid);
	}

	public void setScreeningsEenheid(MammaScreeningsEenheid screeningsEenheid)
	{
		this.screeningsEenheid = ModelUtil.sModel(screeningsEenheid);
	}

	@Override
	protected void detachModel()
	{
		super.detachModel();
		ModelUtil.nullSafeDetach(model);
		ModelUtil.nullSafeDetach(gemeentenModel);
		ModelUtil.nullSafeDetach(clientModel);
		ModelUtil.nullSafeDetach(rondesModel);
		ModelUtil.nullSafeDetach(screeningsEenheid);
		ModelUtil.nullSafeDetach(importPocOptiesIModel);
	}

}
