package nl.rivm.screenit.main.web.gebruiker.testen.colon.timeline;

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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.List;

import nl.rivm.screenit.main.model.ScreeningRondeGebeurtenis;
import nl.rivm.screenit.main.model.ScreeningRondeGebeurtenissen;
import nl.rivm.screenit.main.model.TypeGebeurtenis;
import nl.rivm.screenit.main.model.testen.TestTimelineModel;
import nl.rivm.screenit.main.model.testen.TestTimelineRonde;
import nl.rivm.screenit.main.service.TestTimelineService;
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
import nl.rivm.screenit.model.ScreeningRonde;
import nl.rivm.screenit.model.ScreeningRondeStatus;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.colon.ColonUitnodiging;
import nl.rivm.screenit.model.colon.enums.IFOBTTestStatus;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.GebeurtenisBron;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.colon.ColonTestService;
import nl.rivm.screenit.util.TestBsnGenerator;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.patientregistratie.persoonsgegevens.model.Geslacht;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import nl.topicuszorg.wicket.model.DetachableListModel;
import nl.topicuszorg.wicket.model.SortingListModel;

import org.apache.commons.lang.StringUtils;
import org.apache.wicket.ajax.AjaxEventBehavior;
import org.apache.wicket.ajax.AjaxRequestTarget;
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
import org.hibernate.criterion.Order;
import org.hibernate.criterion.Restrictions;
import org.wicketstuff.datetime.markup.html.basic.DateLabel;
import org.wicketstuff.shiro.ShiroConstraint;
import org.wicketstuff.wiquery.ui.datepicker.DatePicker;

@SecurityConstraint(
	actie = Actie.INZIEN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = Recht.TESTEN,
	bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON })
public class ColonTestTimelinePage extends TestenBasePage
{

	private static final long serialVersionUID = 1L;

	@SpringBean
	private TestTimelineService testTimelineService;

	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private ClientService clientService;

	@SpringBean
	private ColonTestService colonTestService;

	private IModel<TestTimelineModel> model;

	private IModel<List<Client>> clientModel;

	private IModel<List<TestTimelineRonde>> rondesModel;

	private Form<TestTimelineModel> form;

	private WebMarkupContainer formComponents;

	private DatePicker<Date> geboortedatum;

	private final BootstrapDialog dialog;

	public ColonTestTimelinePage()
	{
		dialog = new BootstrapDialog("dialog");
		add(dialog);

		model = new CompoundPropertyModel<TestTimelineModel>(new TestTimelineModel());
		TestTimelineModel object = model.getObject();
		object.setBsn(TestBsnGenerator.getValideBsn());

		form = new Form<TestTimelineModel>("form", model);
		form.setOutputMarkupId(true);
		add(form);

		formComponents = getFormComponentsContainer();
		form.add(formComponents);

		gebeurtenissenContainer = getGebeurtenissenContainer();
		form.add(gebeurtenissenContainer);

		Form<Void> clientResetForm = new ScreenitForm<>("clientResetForm");
		final IModel<String> bsns = new Model<String>("");
		clientResetForm.add(new TextArea<String>("bsns", bsns).setRequired(true));
		clientResetForm.add(new AjaxButton("resetten", clientResetForm)
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onSubmit(AjaxRequestTarget target)
			{
				String message = colonTestService.clientenResetten(bsns.getObject());
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
	}

	private WebMarkupContainer getFormComponentsContainer()
	{
		WebMarkupContainer container = new WebMarkupContainer("formComponents");
		container.setOutputMarkupId(true);

		bsnField = new TextField<>("bsn");
		bsnField.setRequired(true);
		bsnField.setOutputMarkupId(true);
		container.add(bsnField);

		bsnField.add(new AjaxEventBehavior("change")
		{

			private static final long serialVersionUID = 1L;

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

		geboortedatum = ComponentHelper.monthYearDatePicker("geboortedatum");
		geboortedatum.setOutputMarkupId(true);
		container.add(geboortedatum);

		List<Geslacht> geslachten = new ArrayList<Geslacht>(Arrays.asList(Geslacht.values()));
		geslachten.remove(Geslacht.NIET_GESPECIFICEERD);
		geslachten.remove(Geslacht.ONBEKEND);
		RadioChoice<Geslacht> geslachtRadio = new TestEnumRadioChoice<>("geslacht", geslachten, new EnumChoiceRenderer<>(this));
		geslachtRadio.setPrefix("<label class=\"radio\">");
		geslachtRadio.setSuffix("</label>");
		geslachtRadio.setOutputMarkupId(true);
		container.add(geslachtRadio);

		container.add(new DropDownChoice<Gemeente>("gemeente",
			ModelUtil.listRModel(
				hibernateService.getHibernateSession().createCriteria(Gemeente.class).add(Restrictions.isNotNull("screeningOrganisatie")).addOrder(Order.asc("naam")).list(),
				false),
			new ChoiceRenderer<Gemeente>("naam")));

		IndicatingAjaxSubmitLink clientVindOfMaak = new IndicatingAjaxSubmitLink("clientVindOfMaak")
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				TestTimelineModel timelineModel = model.getObject();
				List<Client> clienten = testTimelineService.maakOfVindClienten(timelineModel);
				List<String> warnings = testTimelineService.validateTestClienten(clienten);
				warnings.forEach(ColonTestTimelinePage.this::warn);
				if (warnings.isEmpty())
				{
					submitClients(target, timelineModel, clienten);
				}
			}

			private void submitClients(AjaxRequestTarget target, TestTimelineModel timelineModel, List<Client> clienten)
			{
				clientModel = ModelUtil.listRModel(clienten);
				ColonTestTimelinePage.this.info("Client(en) zijn gevonden en/of succesvol aangemaakt");

				if (clienten != null && clienten.size() > 0)
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
		return container;
	}

	@Override
	protected WebMarkupContainer getGebeurtenissenContainer()
	{

		WebMarkupContainer container = new WebMarkupContainer("gebeurtenissenContainer");
		container.setOutputMarkupPlaceholderTag(true);

		container.setVisible(clientModel != null);
		if (clientModel != null && clientModel.getObject().size() > 0)
		{
			List<TestTimelineRonde> rondes = testTimelineService.getTimelineRondes(clientModel.getObject().get(0));
			Collections.sort(rondes, (o1, o2) -> o2.getRondeNummer().compareTo(o1.getRondeNummer()));
			rondesModel = new DetachableListModel<>(rondes);

			container.add(new TestVervolgKeuzeKnop("nieuweRonde", clientModel, dialog)
			{

				private static final long serialVersionUID = 1L;

				@Override
				public boolean refreshContainer(AjaxRequestTarget target)
				{
					List<Client> clienten = testTimelineService.maakOfVindClienten(model.getObject());
					List<String> errors = testTimelineService.validateTestClienten(clienten);
					for (String error : errors)
					{
						error(error);
					}
					clientModel = ModelUtil.listRModel(clienten);

					WebMarkupContainer geContainer = getGebeurtenissenContainer();
					gebeurtenissenContainer.replaceWith(geContainer);
					gebeurtenissenContainer = geContainer;
					target.add(gebeurtenissenContainer);
					return true;
				}

				@Override
				public List<TestVervolgKeuzeOptie> getOptions()
				{
					List<TestVervolgKeuzeOptie> keuzes = new ArrayList<>();
					keuzes.add(TestVervolgKeuzeOptie.NIEUWESCREENINGRONDEDARMKANKER);
					keuzes.add(TestVervolgKeuzeOptie.VERZET_TIJD);
					return keuzes;
				}

				@Override
				public boolean isVisible()
				{
					boolean isOverleden = false;
					if (clientModel.getObject().size() > 0)
					{
						GbaPersoon persoon = clientModel.getObject().get(0).getPersoon();
						isOverleden = persoon.getOverlijdensdatum() != null;
					}
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

		addbuttons(container);
		return container;
	}

	private ListView<TestTimelineRonde> getListView()
	{
		ListView<TestTimelineRonde> listView = new ListView<TestTimelineRonde>("rondes", rondesModel)
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void populateItem(ListItem<TestTimelineRonde> item)
			{
				item.add(new Label("rondeNummer", item.getModelObject().getRondeNummer()));
				item.add(new TestVervolgKeuzeKnop("snelKeuze", clientModel, dialog)
				{

					private static final long serialVersionUID = 1L;

					@Override
					public List<TestVervolgKeuzeOptie> getOptions()
					{
						return testTimelineService.getSnelKeuzeOpties(getModelObject().get(0));
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
						ScreeningRondeGebeurtenissen gebeurtenissen = timeLineRonde.getColonScreeningRondeDossier();
						ScreeningRonde<?, ?, ?, ?> ronde = gebeurtenissen.getScreeningRonde();
						ColonScreeningRonde colonRonde = (ColonScreeningRonde) ronde;
						boolean isLopend = ScreeningRondeStatus.LOPEND.equals(ronde.getStatus()) || heeftIfobtMetWijzigbareStatus(colonRonde);
						boolean isAangemeld = ronde.getAangemeld().booleanValue();
						return !isOverleden && (isLopend || !isAangemeld && !isLopend);
					}

					private boolean heeftIfobtMetWijzigbareStatus(ColonScreeningRonde colonRonde)
					{
						for (ColonUitnodiging uitnodiging : colonRonde.getUitnodigingen())
						{
							if (uitnodiging.getGekoppeldeTest() != null && IFOBTTestStatus.isMutableEindStatus(uitnodiging.getGekoppeldeTest().getStatus()))
							{
								return true;
							}
						}
						return false;
					}

					@Override
					public String getNameAttribuut()
					{
						return "snelkeuze_gebeurtenis";
					}
				});

				SortingListModel<ScreeningRondeGebeurtenis> sortingListModel = new SortingListModel<ScreeningRondeGebeurtenis>(
					new PropertyModel<List<ScreeningRondeGebeurtenis>>(item.getModel(), "colonScreeningRondeDossier.gebeurtenissen"), new GebeurtenisComparator());

				item.add(new PropertyListView<ScreeningRondeGebeurtenis>("gebeurtenissen", sortingListModel)
				{

					private static final long serialVersionUID = 1L;

					@Override
					protected void populateItem(final ListItem<ScreeningRondeGebeurtenis> item)
					{
						item.add(DateLabel.forDatePattern("datum", "dd-MM-yyyy HH:mm:ss"));
						item.add(new EnumLabel<TypeGebeurtenis>("gebeurtenis"));
						item.add(new EnumLabel<GebeurtenisBron>("bron"));
						item.add(new AttributeAppender("class", new Model<String>("badge-not-clickable"), " "));
						item.add(new Label("extraOmschrijving", new IModel<String>()
						{

							private static final long serialVersionUID = 1L;

							@Override
							public String getObject()
							{
								ScreeningRondeGebeurtenis gebeurtenis2 = item.getModelObject();
								String[] extraOmschrijvingen = gebeurtenis2.getExtraOmschrijving();
								String extraOmschrijving = "";
								if (extraOmschrijvingen != null)
								{
									for (String omschrijving : extraOmschrijvingen)
									{
										if (omschrijving != null)
										{
											if (StringUtils.isNotBlank(extraOmschrijving))
											{
												if (extraOmschrijving.trim().endsWith(":"))
												{
													if (!extraOmschrijving.endsWith(":"))
													{
														extraOmschrijving += " ";
													}
												}
												else
												{
													extraOmschrijving += ", ";
												}
											}
											else
											{
												extraOmschrijving = "(";
											}
											extraOmschrijving += getString(omschrijving, null, omschrijving);
										}
									}
								}
								if (StringUtils.isNotBlank(extraOmschrijving))
								{
									extraOmschrijving += ")";
								}
								return extraOmschrijving;
							}
						}));
					}
				});
			}
		};
		return listView;
	}

	private void addbuttons(WebMarkupContainer container)
	{
		container.add(new AjaxButton("bijzondereClientData", form)
		{

			private static final long serialVersionUID = 1L;

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

	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(clientModel);
	}
}
