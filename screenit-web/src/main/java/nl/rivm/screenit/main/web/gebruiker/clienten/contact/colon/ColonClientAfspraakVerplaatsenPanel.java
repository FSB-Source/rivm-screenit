package nl.rivm.screenit.main.web.gebruiker.clienten.contact.colon;

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

import java.math.BigDecimal;
import java.math.BigInteger;
import java.text.SimpleDateFormat;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.SimpleStringResourceModel;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.model.Gemeente;
import nl.rivm.screenit.model.colon.ColonConclusie;
import nl.rivm.screenit.model.colon.ColonIntakeAfspraak;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.colon.ColoscopieCentrum;
import nl.rivm.screenit.model.colon.Kamer;
import nl.rivm.screenit.model.colon.enums.ColonConclusieType;
import nl.rivm.screenit.model.colon.planning.AfspraakDefinitie;
import nl.rivm.screenit.model.colon.planning.AfspraakStatus;
import nl.rivm.screenit.model.colon.planning.VrijSlotZonderKamer;
import nl.rivm.screenit.model.colon.planning.VrijSlotZonderKamerFilter;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.ExtraOpslaanKey;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.colon.AfspraakService;
import nl.rivm.screenit.service.colon.ColonUitnodigingService;
import nl.rivm.screenit.service.colon.PlanningService;
import nl.rivm.screenit.util.AdresUtil;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import nl.topicuszorg.wicket.input.validator.DependantDateValidator;
import nl.topicuszorg.wicket.input.validator.DependantDateValidator.Operator;
import nl.topicuszorg.wicket.planning.model.Discipline;
import nl.topicuszorg.wicket.planning.web.component.DateTimeField;
import nl.topicuszorg.wicket.planning.web.component.TimeField;
import nl.topicuszorg.wicket.search.column.DateTimePropertyColumn;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormComponentUpdatingBehavior;
import org.apache.wicket.ajax.markup.html.form.AjaxSubmitLink;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.extensions.markup.html.repeater.data.grid.ICellPopulator;
import org.apache.wicket.extensions.markup.html.repeater.data.table.AbstractColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.CheckBox;
import org.apache.wicket.markup.html.form.ChoiceRenderer;
import org.apache.wicket.markup.html.form.DropDownChoice;
import org.apache.wicket.markup.html.form.EnumChoiceRenderer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.FormComponent;
import org.apache.wicket.markup.html.form.RadioChoice;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.markup.html.panel.EmptyPanel;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.model.util.ListModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.validation.validator.DateValidator;
import org.wicketstuff.datetime.markup.html.basic.DateLabel;
import org.wicketstuff.wiquery.ui.datepicker.DatePicker;

public class ColonClientAfspraakVerplaatsenPanel extends GenericPanel<ColonIntakeAfspraak>
{

	@SpringBean
	private ICurrentDateSupplier currentDateSupplier;

	@SpringBean
	private SimplePreferenceService preferenceService;

	@SpringBean
	private ColonUitnodigingService colonUitnodigingService;

	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private PlanningService planningService;

	@SpringBean
	private AfspraakService afspraakService;

	private final boolean magAfspraakBinnenIntakeNietWijzigPeriodePlaatsen = ScreenitSession.get()
		.checkPermission(Recht.GEBRUIKER_CLIENT_SR_INTAKE_VERPLAATS_BINNEN_INTAKE_NIET_WIJZIGBAAR_PERIODE, Actie.AANPASSEN);

	private final boolean locatieWijzigen;

	private final IModel<Boolean> binnenNietWijzigbaarPeriode = Model.of(Boolean.FALSE);

	private final IModel<Boolean> buitenRooster = Model.of(Boolean.FALSE);

	private VrijSlotZonderKamer gekozenVrijSlotZonderKamer;

	private IModel<BriefType> briefType;

	private WebMarkupContainer momentZoekenContainer;

	private IModel<VrijSlotZonderKamerFilter> filterModel;

	private Form<VrijSlotZonderKamerFilter> filterForm;

	private ScreenitDataTable<VrijSlotZonderKamer, String> nieuweAfspraakTable;

	@Getter
	@Setter
	private Date datumTijdBuitenRooster = null;

	private final IModel<Boolean> briefTegenhouden = Model.of(Boolean.FALSE);

	private final WebMarkupContainer nieuweAfspraakContainer;

	private IModel<ColonIntakeAfspraak> nieuweAfspraakModel;

	private final IModel<Boolean> isDoorverwezenOmMedischeRedenenModel = Model.of();

	private final boolean isDoorverwezen;

	public ColonClientAfspraakVerplaatsenPanel(String id, IModel<ColonIntakeAfspraak> model, boolean locatieWijzigen)
	{
		super(id, model);

		this.locatieWijzigen = locatieWijzigen;

		gekozenVrijSlotZonderKamer = new VrijSlotZonderKamer();
		if (!locatieWijzigen || AfspraakStatus.GEANNULEERD_AFMELDEN.equals(getModelObject().getStatus()))
		{
			gekozenVrijSlotZonderKamer.setIntakeLocatieId(BigInteger.valueOf(getModelObject().getLocation().getColoscopieCentrum().getId()));
		}

		nieuweAfspraakContainer = new WebMarkupContainer("nieuweAfspraak");
		nieuweAfspraakContainer.setOutputMarkupId(true);
		add(nieuweAfspraakContainer);

		isDoorverwezen = afspraakService.isDoorverwezenOmMedischeRedenenZonderNieuweAfspraak(model.getObject().getClient());

		addBriefType();
		addHuidigeAfspraak();
		addOrReplaceTegenhoudenContainer(null);
		addOrReplaceDatumTijdBuitenRoosterContainer(null);
		addOrReplaceAfspraakDetailsContainer(null);
		addMomentZoeken();
		addOrReplaceMedischeRedenKeuze(null);
	}

	private void addBriefType()
	{
		List<BriefType> choices = getBriefTypes();
		briefType = Model.of(choices.get(0));
		if (choices.contains(BriefType.COLON_INTAKE_GEWIJZIGD))
		{
			briefType = Model.of(BriefType.COLON_INTAKE_GEWIJZIGD);
		}
		ScreenitDropdown<BriefType> briefTypeDropDown = new ScreenitDropdown<>("briefType", briefType, choices, new EnumChoiceRenderer<BriefType>(this));
		briefTypeDropDown.setVisible(
			ScreenitSession.get().checkPermission(Recht.GEBRUIKER_CLIENT_SR_INTAKE_WIJZIGEN_ANDER_BRIEF, Actie.AANPASSEN) && !isDoorverwezen);
		briefTypeDropDown.setRequired(true);
		add(briefTypeDropDown);
	}

	private List<BriefType> getBriefTypes()
	{
		ColonIntakeAfspraak afspraak = getModelObject();
		ColonScreeningRonde ronde = afspraak.getColonScreeningRonde();

		List<BriefType> choices = new ArrayList<BriefType>();
		if (ronde.getOpenUitnodiging() != null && ronde.getLaatsteAfspraak() == null)
		{
			choices.add(BriefType.COLON_BEVESTIGING_INTAKE_AFSRPAAK_NA_OPEN_UITNODIGING);
		}
		else
		{
			choices.add(BriefType.COLON_INTAKE_GEWIJZIGD);
			choices.add(BriefType.COLON_UITNODIGING_INTAKE);
		}
		return choices;
	}

	private void addHuidigeAfspraak()
	{
		ColonIntakeAfspraak afspraak = getModelObject();
		AfspraakStatus status = afspraak.getStatus();
		ColonConclusie conclusie = afspraak.getConclusie();
		ColonConclusieType colonConclusieType = conclusie != null ? conclusie.getType() : null;
		boolean toonHuidigeAfspraak = AfspraakStatus.GEPLAND.equals(status)
			|| AfspraakStatus.UITGEVOERD.equals(status) && ColonConclusieType.NO_SHOW.equals(colonConclusieType)
			|| afspraakService.heeftOnafgerondeVerwijzingOmMedischeRedenen(afspraak);

		add(new Label("title").setVisible(toonHuidigeAfspraak));

		IModel<ColoscopieCentrum> centrum = () -> getModelObject().getLocation().getColoscopieCentrum();
		add(new Label("ccNaam", new PropertyModel<String>(centrum, "naam")));

		add(new Label("ccAdres", new IModel<String>()
		{
			@Override
			public String getObject()
			{
				return org.apache.wicket.util.string.Strings
					.escapeMarkup(AdresUtil.getVolledigeAdresString(getModelObject().getLocation().getColoscopieCentrum().getAdressen().get(0))).toString();
			}
		}).setEscapeModelStrings(false));
		add(DateLabel.forDatePattern("startTime", "EEEE dd-MM-yyyy HH:mm"));
	}

	private void addOrReplaceMedischeRedenKeuze(AjaxRequestTarget target)
	{
		var radioContainer = new WebMarkupContainer("isVerwezenMedischeRedenenContainer");
		var verwezenMedischeRedenRadioChoice = new RadioChoice<>("isVerwezenMedischeRedenen", isDoorverwezenOmMedischeRedenenModel,
			new ListModel<>(Arrays.asList(Boolean.TRUE, Boolean.FALSE)),
			new ChoiceRenderer<>()
			{
				@Override
				public Object getDisplayValue(Boolean object)
				{
					return Boolean.TRUE.equals(object) ? "Ja" : "Nee";
				}
			});
		var afspraak = getModelObject();
		var isVerlopenAfspraakZonderConclusie = afspraak.getConclusie() == null
			&& afspraak.getStartTime() != null && DateUtil.compareAfter(currentDateSupplier.getDate(), afspraak.getStartTime())
			&& !AfspraakStatus.isGeannuleerd(afspraak.getStatus());
		var isGekozenIntakeLocatieAndereDanOudeAfspraak = gekozenVrijSlotZonderKamer.getIntakeLocatieId() != null
			&& !afspraak.getLocation().getColoscopieCentrum().getId().equals(gekozenVrijSlotZonderKamer.getIntakeLocatieId());
		verwezenMedischeRedenRadioChoice.setRequired(true);
		verwezenMedischeRedenRadioChoice.setPrefix("<label class=\"radio span1\">");
		verwezenMedischeRedenRadioChoice.setSuffix("</label>");
		radioContainer.setVisible(isVerlopenAfspraakZonderConclusie && isGekozenIntakeLocatieAndereDanOudeAfspraak);
		radioContainer.setOutputMarkupPlaceholderTag(true);
		radioContainer.addOrReplace(verwezenMedischeRedenRadioChoice);
		addOrReplace(radioContainer);
		if (target != null)
		{
			target.add(radioContainer);
		}
	}

	private void addOrReplaceTegenhoudenContainer(AjaxRequestTarget target)
	{
		Boolean rechtVoorBriefTegenhouden = ScreenitSession.get().checkPermission(Recht.GEBRUIKER_CLIENT_SR_BRIEVEN_TEGENHOUDEN, Actie.AANPASSEN);
		WebMarkupContainer briefTegenhoudenContainer = new WebMarkupContainer("briefTegenhoudenContainer");
		briefTegenhoudenContainer.setVisible(Boolean.TRUE.equals(binnenNietWijzigbaarPeriode.getObject()) ||
			rechtVoorBriefTegenhouden);
		briefTegenhoudenContainer.setOutputMarkupPlaceholderTag(true);
		nieuweAfspraakContainer.addOrReplace(briefTegenhoudenContainer);

		CheckBox briefTegenhoudenCheckBox = ComponentHelper.newCheckBox("briefTegenhouden", briefTegenhouden);
		briefTegenhoudenCheckBox.setOutputMarkupPlaceholderTag(true);
		briefTegenhoudenContainer.addOrReplace(briefTegenhoudenCheckBox);

		if (target != null)
		{
			target.add(nieuweAfspraakContainer);
		}
	}

	private void addOrReplaceDatumTijdBuitenRoosterContainer(AjaxRequestTarget target)
	{
		WebMarkupContainer datumTijdBuitenRoosterContainer = new WebMarkupContainer("datumTijdBuitenRoosterContainer");
		datumTijdBuitenRoosterContainer.setOutputMarkupPlaceholderTag(true);
		DateTimeField datumTijdBuitenRooster = new DateTimeField("datumTijdBuitenRooster",
			new PropertyModel<Date>(ColonClientAfspraakVerplaatsenPanel.this, "datumTijdBuitenRooster"))
		{
			@Override
			protected DatePicker<Date> newDatePicker(String wicketId, IModel<Date> model)
			{
				DatePicker<Date> datePicker = ComponentHelper.newDatePicker(wicketId, model);
				datePicker.setLabel(new Model<>("Datum/tijd vanaf"));
				return datePicker;
			}

			@Override
			protected TimeField newTimeField(String wicketId, IModel<Date> model)
			{
				TimeField timeField = new TimeField(wicketId, model)
				{
					@Override
					protected TextField<Integer> getHoursField()
					{
						TextField<Integer> hours = super.getHoursField();
						hours.setLabel(new Model<>("Datum/tijd vanaf uren"));
						return hours;
					}

					@Override
					protected TextField<Integer> getMinutesField()
					{
						TextField<Integer> minutes = super.getMinutesField();
						minutes.setLabel(new Model<>("Datum/tijd vanaf minuten"));
						return minutes;
					}

					@Override
					public void convertInput()
					{
						Integer h = getHoursField().getConvertedInput();
						Integer m = getMinutesField().getConvertedInput();

						if (h != null && m != null)
						{
							super.convertInput();
						}
					}
				};
				return timeField;
			}
		};
		datumTijdBuitenRooster.setRequired(true);
		datumTijdBuitenRoosterContainer.setVisible(Boolean.TRUE.equals(buitenRooster.getObject()));
		datumTijdBuitenRoosterContainer.add(datumTijdBuitenRooster);
		nieuweAfspraakContainer.addOrReplace(datumTijdBuitenRoosterContainer);
		if (target != null)
		{
			target.add(datumTijdBuitenRoosterContainer);
		}
	}

	private void addOrReplaceAfspraakDetailsContainer(AjaxRequestTarget target)
	{
		WebMarkupContainer afspraakDetails = new WebMarkupContainer("afspraakDetails");
		afspraakDetails.setOutputMarkupPlaceholderTag(true);

		if (gekozenVrijSlotZonderKamer.getIntakeLocatieId() == null)
		{
			afspraakDetails.add(new EmptyPanel("naam"));
			afspraakDetails.add(new EmptyPanel("adres"));
			afspraakDetails.add(new EmptyPanel("locatieBeschrijving"));
		}
		else
		{
			ColoscopieCentrum intakeLocatie = hibernateService.get(ColoscopieCentrum.class, gekozenVrijSlotZonderKamer.getIntakeLocatieId());
			afspraakDetails.add(new Label("naam", intakeLocatie.getNaam()));
			afspraakDetails.add(new Label("adres", AdresUtil.getVolledigeAdresString(intakeLocatie.getAdressen().get(0))));
			afspraakDetails.add(new Label("locatieBeschrijving", intakeLocatie.getLocatieBeschrijving()).setVisible(intakeLocatie.getLocatieBeschrijving() != null));
		}

		IndicatingAjaxLink wijzigLocatie = new IndicatingAjaxLink<Void>("wijzigLocatie")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				gekozenVrijSlotZonderKamer.setIntakeLocatieId(null);
				gekozenVrijSlotZonderKamer.setStartTijd(null);
				addOrReplaceAfspraakDetailsContainer(target);
				addOrReplaceMedischeRedenKeuze(target);
				momentZoekenContainer.setVisible(true);
				target.add(momentZoekenContainer);
			}
		};
		wijzigLocatie.setVisible(locatieWijzigen && buitenRooster.getObject() && gekozenVrijSlotZonderKamer.getIntakeLocatieId() != null);
		afspraakDetails.add(wijzigLocatie);

		WebMarkupContainer tijdContainer = new WebMarkupContainer("tijdContainer");
		tijdContainer.setVisible(!buitenRooster.getObject());
		afspraakDetails.add(tijdContainer);

		DateLabel startTime = DateLabel.forDatePattern("startTime", Model.of(gekozenVrijSlotZonderKamer.getStartTijd()), "EEEE dd-MM-yyyy HH:mm");
		tijdContainer.add(startTime);

		IndicatingAjaxLink tijdWijzigen = new IndicatingAjaxLink<Void>("tijdWijzigen")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				gekozenVrijSlotZonderKamer.setStartTijd(null);
				addOrReplaceAfspraakDetailsContainer(target);
				addOrReplaceMedischeRedenKeuze(target);
				momentZoekenContainer.setVisible(true);
				target.add(momentZoekenContainer);
			}
		};
		tijdWijzigen.setVisible(gekozenVrijSlotZonderKamer.getStartTijd() != null);
		tijdContainer.add(tijdWijzigen);

		CheckBox binnenNietWijzigbaarPeriodeCheckBox = ComponentHelper.newCheckBox("binnenNietWijzigbaarPeriode", binnenNietWijzigbaarPeriode);
		binnenNietWijzigbaarPeriodeCheckBox.setVisible(magAfspraakBinnenIntakeNietWijzigPeriodePlaatsen);
		binnenNietWijzigbaarPeriodeCheckBox.add(new AjaxFormComponentUpdatingBehavior("change")
		{
			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				gekozenVrijSlotZonderKamer.setStartTijd(null);
				buitenRooster.setObject(false);

				momentZoekenContainer.setVisible(true);
				target.add(momentZoekenContainer);

				addOrReplaceTegenhoudenContainer(target);
				addOrReplaceAfspraakDetailsContainer(target);
				addOrReplaceMedischeRedenKeuze(target);
				addOrReplaceMomentZoekenFilter(target);
				addOrReplaceMomentZoekenTable(target);

				if (binnenNietWijzigbaarPeriode.getObject())
				{
					briefTegenhouden.setObject(Boolean.TRUE);
				}
				else
				{
					briefTegenhouden.setObject(Boolean.FALSE);
				}
			}
		});
		afspraakDetails.add(binnenNietWijzigbaarPeriodeCheckBox);

		CheckBox buitenRoosterCheckBox = ComponentHelper.newCheckBox("buitenRooster", buitenRooster);
		buitenRoosterCheckBox.setVisible(Boolean.TRUE.equals(binnenNietWijzigbaarPeriode.getObject()));
		buitenRoosterCheckBox.add(new AjaxFormComponentUpdatingBehavior("change")
		{
			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				gekozenVrijSlotZonderKamer.setStartTijd(null);

				momentZoekenContainer.setVisible(!buitenRooster.getObject() || gekozenVrijSlotZonderKamer.getIntakeLocatieId() == null);
				target.add(momentZoekenContainer);

				addOrReplaceDatumTijdBuitenRoosterContainer(target);
				addOrReplaceAfspraakDetailsContainer(target);
				addOrReplaceMedischeRedenKeuze(target);
				addOrReplaceMomentZoekenFilter(target);
				addOrReplaceMomentZoekenTable(target);
			}
		});
		afspraakDetails.add(buitenRoosterCheckBox);

		nieuweAfspraakContainer.addOrReplace(afspraakDetails);
		if (target != null)
		{
			target.add(afspraakDetails);
		}
	}

	private void addMomentZoeken()
	{
		momentZoekenContainer = new WebMarkupContainer("momentZoekenContainer");
		momentZoekenContainer.setOutputMarkupPlaceholderTag(true);
		add(momentZoekenContainer);

		VrijSlotZonderKamerFilter vrijSlotZonderKamerFilter = new VrijSlotZonderKamerFilter();
		ColoscopieCentrum intakelocatie = getModelObject().getLocation().getColoscopieCentrum();
		if (!locatieWijzigen)
		{
			vrijSlotZonderKamerFilter.setIntakeLocatieId(intakelocatie.getId());
		}
		else if (AfspraakStatus.GEANNULEERD_AFMELDEN.equals(getModelObject().getStatus()))
		{
			vrijSlotZonderKamerFilter.setNaam(intakelocatie.getNaam());
		}
		if (afspraakService.heeftOnafgerondeVerwijzingOmMedischeRedenen(getModelObject()))
		{
			vrijSlotZonderKamerFilter.setNietIntakeLocatieId(intakelocatie.getId());
		}
		filterModel = new CompoundPropertyModel<>(vrijSlotZonderKamerFilter);
		addOrReplaceMomentZoekenFilter(null);

		addOrReplaceMomentZoekenTable(null);
	}

	private void addOrReplaceMomentZoekenFilter(AjaxRequestTarget target)
	{
		Integer intakeNietWijzigbaar = preferenceService.getInteger(PreferenceKey.INTAKE_NIET_WIJZIGBAAR.name());
		Date vanaf;
		Date totEnMet;
		if (Boolean.FALSE.equals(binnenNietWijzigbaarPeriode.getObject()))
		{
			vanaf = DateUtil.plusWerkdagen(currentDateSupplier.getDateMidnight(), intakeNietWijzigbaar);
			totEnMet = DateUtil.toUtilDate(colonUitnodigingService.getGeprognotiseerdeIntakeDatum(true));
		}
		else
		{
			vanaf = currentDateSupplier.getDateMidnight();
			totEnMet = DateUtil.plusWerkdagen(currentDateSupplier.getDateMidnight(), intakeNietWijzigbaar - 1);
		}

		filterModel.getObject().setVanaf(vanaf);
		filterModel.getObject().setTotEnMet(totEnMet);

		filterModel.getObject().setAlleenIntakeLokaties(buitenRooster.getObject());

		Form<VrijSlotZonderKamerFilter> newForm = new Form<>("filterForm", filterModel);

		WebMarkupContainer vanafTotEnMetContainer = new WebMarkupContainer("vanafTotEnMetContainer");
		vanafTotEnMetContainer.setVisible(!buitenRooster.getObject());
		newForm.add(vanafTotEnMetContainer);

		DatePicker<Date> vanafField = ComponentHelper.newDatePicker("vanaf", null, !binnenNietWijzigbaarPeriode.getObject());
		vanafField.setRequired(true);
		vanafField.add(DateValidator.minimum(vanaf));
		vanafTotEnMetContainer.add(vanafField);

		DatePicker<Date> totEnMetField = ComponentHelper.newDatePicker("totEnMet", null, !binnenNietWijzigbaarPeriode.getObject());
		totEnMetField.setRequired(true);
		vanafTotEnMetContainer.add(totEnMetField);
		newForm.add(new DependantDateValidator(vanafField, totEnMetField, Operator.AFTER));

		FormComponent<String> naamField = ComponentHelper.addTextField(newForm, "naam", false, 40, String.class, false);
		naamField.setVisible(locatieWijzigen);

		FormComponent<String> plaatsField = ComponentHelper.addTextField(newForm, "plaats", false, 40, String.class, false);
		plaatsField.setVisible(locatieWijzigen);

		Integer[] afstanden = new Integer[] { 5, 10, 15, 20, 25, 30, 35, 40, 45 };
		newForm.add(new DropDownChoice<>("afstand", Arrays.asList(afstanden)).setNullValid(true).setVisible(locatieWijzigen));

		AjaxSubmitLink zoeken = new AjaxSubmitLink("zoeken", newForm)
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				target.add(nieuweAfspraakTable);
			}
		};
		zoeken.setEnabled(!binnenNietWijzigbaarPeriode.getObject() || locatieWijzigen);
		newForm.add(zoeken);

		if (filterForm == null)
		{
			momentZoekenContainer.add(newForm);
		}
		else
		{
			filterForm.replaceWith(newForm);
			target.add(newForm);
		}
		filterForm = newForm;
	}

	private void addOrReplaceMomentZoekenTable(AjaxRequestTarget target)
	{
		List<IColumn<VrijSlotZonderKamer, String>> columns = new ArrayList<>();
		if (!buitenRooster.getObject())
		{
			columns.add(new DateTimePropertyColumn<>(new SimpleStringResourceModel("label.datumtijd"), "startTijd", "startTime",
				new SimpleDateFormat("EEEE dd-MM-yyyy HH:mm", ScreenitSession.get().getLocale())));
		}
		columns.add(new AbstractColumn<>(new SimpleStringResourceModel("label.naam"), "naam")
		{
			@Override
			public void populateItem(Item<ICellPopulator<VrijSlotZonderKamer>> cellItem, String componentId, IModel<VrijSlotZonderKamer> rowModel)
			{
				cellItem.add(new Label(componentId, hibernateService.load(ColoscopieCentrum.class, rowModel.getObject().getIntakeLocatieId()).getNaam()));
			}
		});
		columns.add(new PropertyColumn<>(new SimpleStringResourceModel("label.plaats"), "plaats", "plaats"));
		columns.add(new AbstractColumn<>(new SimpleStringResourceModel("label.afstand"), "afstand")
		{
			@Override
			public void populateItem(Item<ICellPopulator<VrijSlotZonderKamer>> cellItem, String componentId, IModel<VrijSlotZonderKamer> rowModel)
			{
				cellItem.add(new Label(componentId, rowModel.getObject().getAfstand()));
			}
		});

		VrijSlotZonderKamerDataProvider vrijSlotZonderKamerDataProvider = new VrijSlotZonderKamerDataProvider(filterModel.getObject(),
			new PropertyModel<>(getModel(), "client"));

		ScreenitDataTable<VrijSlotZonderKamer, String> newTable = new ScreenitDataTable<VrijSlotZonderKamer, String>("vrijeSloten", columns, vrijSlotZonderKamerDataProvider, 10,
			Model.of("Vrije sloten"))
		{
			@Override
			public void onClick(AjaxRequestTarget target, IModel<VrijSlotZonderKamer> model)
			{
				gekozenVrijSlotZonderKamer = model.getObject();
				addOrReplaceAfspraakDetailsContainer(target);
				addOrReplaceMedischeRedenKeuze(target);
				momentZoekenContainer.setVisible(false);
				target.add(momentZoekenContainer);
			}
		};

		if (nieuweAfspraakTable == null)
		{
			momentZoekenContainer.add(newTable);
		}
		else
		{
			nieuweAfspraakTable.replaceWith(newTable);
			target.add(newTable);
		}
		nieuweAfspraakTable = newTable;
	}

	public List<String> getOpslaanMeldingen()
	{
		nieuweAfspraakModel = null;
		if (gekozenVrijSlotZonderKamer.getIntakeLocatieId() != null && (gekozenVrijSlotZonderKamer.getStartTijd() != null || datumTijdBuitenRooster != null))
		{
			ColoscopieCentrum intakeLocatie = hibernateService.get(ColoscopieCentrum.class, gekozenVrijSlotZonderKamer.getIntakeLocatieId());

			Kamer beschikbareKamer = null;
			if (!buitenRooster.getObject())
			{
				beschikbareKamer = planningService.getBeschikbareKamer(gekozenVrijSlotZonderKamer.getStartTijd(), gekozenVrijSlotZonderKamer.getIntakeLocatieId());
				if (beschikbareKamer == null)
				{
					return List.of(getString("vrije.slot.intussen.bezet"));
				}
			}
			else
			{
				List<Kamer> kamers = intakeLocatie.getKamers();
				for (Kamer kamer : kamers)
				{
					if (Boolean.TRUE.equals(kamer.getActief()))
					{
						beschikbareKamer = kamer;
						break;
					}
				}
			}

			nieuweAfspraakModel = ModelUtil.cModel(new ColonIntakeAfspraak());
			ColonIntakeAfspraak nieuweAfspraak = nieuweAfspraakModel.getObject();
			nieuweAfspraak.setLocation(beschikbareKamer);
			nieuweAfspraak.setActief(true);
			nieuweAfspraak.setBezwaar(false);
			nieuweAfspraak.setDatumLaatsteWijziging(currentDateSupplier.getDate());
			nieuweAfspraak.setAfspraaknummer(System.currentTimeMillis());
			nieuweAfspraak.setStatus(AfspraakStatus.GEPLAND);

			if (gekozenVrijSlotZonderKamer.getAfstand() != null)
			{
				nieuweAfspraak.setAfstand(BigDecimal.valueOf(gekozenVrijSlotZonderKamer.getAfstand()));
			}
			else
			{
				nieuweAfspraak.setAfstand(BigDecimal.valueOf(45));
			}
			nieuweAfspraak.addDiscipline(hibernateService.loadAll(Discipline.class).get(0));

			ColonIntakeAfspraak oudeAfspraak = getModelObject();
			nieuweAfspraak.setColonScreeningRonde(oudeAfspraak.getColonScreeningRonde());
			nieuweAfspraak.setClient(oudeAfspraak.getClient());
			nieuweAfspraak.setDefinition(oudeAfspraak.getDefinition());

			AfspraakDefinitie afspraakDefinitie = intakeLocatie.getAfspraakDefinities().get(0);
			Integer duurAfspraakInMinuten = afspraakDefinitie.getDuurAfspraakInMinuten();
			if (!buitenRooster.getObject())
			{
				nieuweAfspraak.setStartTime(gekozenVrijSlotZonderKamer.getStartTijd());
				nieuweAfspraak.setEndTime(gekozenVrijSlotZonderKamer.getEindTijd());
			}
			else
			{
				nieuweAfspraak.setStartTime(datumTijdBuitenRooster);
				nieuweAfspraak.setEndTime(DateUtil.plusTijdseenheid(nieuweAfspraak.getStartTime(), duurAfspraakInMinuten, ChronoUnit.MINUTES));
			}

			SimpleDateFormat format = new SimpleDateFormat("dd-MM-yyyy HH:mm");
			ColonIntakeAfspraak laatsteAfspraak = nieuweAfspraak.getColonScreeningRonde().getLaatsteAfspraak();
			if (laatsteAfspraak != null)
			{
				return List.of(String.format("coloscopie intake afspraak van %1$s in %2$s van %3$s verplaatsen naar %4$s in %5$s van %6$s",
					format.format(laatsteAfspraak.getStartTime()), laatsteAfspraak.getLocation().getName(), laatsteAfspraak.getLocation().getColoscopieCentrum().getNaam(),
					format.format(nieuweAfspraak.getStartTime()), nieuweAfspraak.getLocation().getName(), nieuweAfspraak.getLocation().getColoscopieCentrum().getNaam()));
			}
			else
			{
				return List.of(String.format("coloscopie intake afspraak wilt maken op %1$s in %2$s van %3$s", format.format(nieuweAfspraak.getStartTime()),
					nieuweAfspraak.getLocation().getName(), nieuweAfspraak.getLocation().getColoscopieCentrum().getNaam()));
			}
		}
		else
		{
			return List.of("Coloscopie intake afspraak kan niet worden verplaatst. Niet alle gegevens zijn geselecteerd");
		}
	}

	public Map<ExtraOpslaanKey, Object> getOpslaanObjecten()
	{
		Boolean rechtVoorBriefTegenhouden = ScreenitSession.get().checkPermission(Recht.GEBRUIKER_CLIENT_SR_BRIEVEN_TEGENHOUDEN, Actie.AANPASSEN);
		Map<ExtraOpslaanKey, Object> opslaanObjecten = new HashMap<>();
		if (nieuweAfspraakModel != null)
		{
			ColonIntakeAfspraak nieuweAfspraak = nieuweAfspraakModel.getObject();
			if (nieuweAfspraak != null)
			{
				if (ScreenitSession.get().checkPermission(Recht.GEBRUIKER_CLIENT_SR_INTAKE_WIJZIGEN_ANDER_BRIEF, Actie.AANPASSEN))
				{
					opslaanObjecten.put(ExtraOpslaanKey.AFSPRAAK_BRIEF, briefType.getObject());
				}
				else
				{
					opslaanObjecten.put(ExtraOpslaanKey.AFSPRAAK_BRIEF, null);
				}
				opslaanObjecten.put(ExtraOpslaanKey.AFSPRAAK, nieuweAfspraak);
				opslaanObjecten.put(ExtraOpslaanKey.AFSPRAAK_STATUS, AfspraakStatus.VERPLAATST);
				opslaanObjecten.put(ExtraOpslaanKey.AFSPRAAK_BRIEF_TEGENHOUDEN,
					binnenNietWijzigbaarPeriode.getObject() && briefTegenhouden.getObject() || rechtVoorBriefTegenhouden && briefTegenhouden.getObject());
				opslaanObjecten.put(ExtraOpslaanKey.AFSPRAAK_UIT_ROOSTER, !buitenRooster.getObject());
				opslaanObjecten.put(ExtraOpslaanKey.COLON_VERWIJZING_MEDISCHE_REDENEN_INFOLIJN, isDoorverwezenOmMedischeRedenenModel.getObject());
			}
		}
		return opslaanObjecten;
	}

	public void validate()
	{
		if (getModelObject().getClient().getPersoon().getGbaAdres().getGbaGemeente().getCode().equals(Gemeente.RNI_CODE))
		{
			error(getString("error.vertrokken.uit.nederland"));
		}
		else if (gekozenVrijSlotZonderKamer.getIntakeLocatieId() == null)
		{
			error(getString("error.geen.locatie.gekozen"));
		}
		else if (gekozenVrijSlotZonderKamer.getStartTijd() == null && datumTijdBuitenRooster == null)
		{
			error(getString("error.geen.datumtijd.gekozen"));
		}
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(nieuweAfspraakModel);
	}
}
