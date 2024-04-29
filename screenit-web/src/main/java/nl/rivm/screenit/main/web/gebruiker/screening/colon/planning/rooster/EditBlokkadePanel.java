package nl.rivm.screenit.main.web.gebruiker.screening.colon.planning.rooster;

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

import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Objects;
import java.util.TimeZone;

import nl.rivm.screenit.exceptions.OpslaanVerwijderenTijdBlokException;
import nl.rivm.screenit.main.exception.ValidatieException;
import nl.rivm.screenit.main.service.colon.ColonBlokkadeService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.component.form.ScreenITDateTimeField;
import nl.rivm.screenit.model.colon.Kamer;
import nl.rivm.screenit.model.colon.planning.ColonBlokkade;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.EntityAuditUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import nl.topicuszorg.wicket.planning.model.appointment.recurrence.AbstractRecurrence;
import nl.topicuszorg.wicket.planning.model.appointment.recurrence.NoRecurrence;
import nl.topicuszorg.wicket.planning.web.component.DatePickerHelper;
import nl.topicuszorg.wicket.planning.web.component.DateTimeField;
import nl.topicuszorg.wicket.planning.web.component.TimeField;
import nl.topicuszorg.wicket.planning.web.panel.recurrence.HerhalingPanel;

import org.apache.commons.lang.StringUtils;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormComponentUpdatingBehavior;
import org.apache.wicket.ajax.form.AjaxFormSubmitBehavior;
import org.apache.wicket.ajax.form.OnChangeAjaxBehavior;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.CheckBox;
import org.apache.wicket.markup.html.form.ChoiceRenderer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.FormComponent;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.markup.html.form.validation.AbstractFormValidator;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.request.cycle.RequestCycle;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.validation.validator.DateValidator;
import org.hibernate.Hibernate;
import org.joda.time.DateTime;
import org.joda.time.DateTimeFieldType;
import org.joda.time.DateTimeZone;
import org.joda.time.MutableDateTime;
import org.wicketstuff.wiquery.ui.datepicker.DateOption;
import org.wicketstuff.wiquery.ui.datepicker.DatePicker;

public abstract class EditBlokkadePanel extends AbstractEditTijdSlotPanel<ColonBlokkade>
{

	@SpringBean
	private LogService logService;

	@SpringBean
	private ICurrentDateSupplier currentDateSupplier;

	@SpringBean
	private ColonBlokkadeService blokkadeService;

	@SpringBean
	private HibernateService hibernateService;

	protected IModel<Boolean> alleKamers;

	private Model<Boolean> heleDag;

	private final boolean magVerwijderen;

	public EditBlokkadePanel(String id, IModel<ColonBlokkade> model)
	{
		super(id, model);
		magVerwijderen = super.isDeleteButtonVisible() && getModelObject().getStartTime().after(currentDateSupplier.getDate());
	}

	@Override
	protected void initForm(final Form<ColonBlokkade> form)
	{
		final boolean magAanpassen = ScreenitSession.get().checkPermission(Recht.GEBRUIKER_LOCATIE_ROOSTER, Actie.AANPASSEN);

		ColonBlokkade blokkade = form.getModelObject();
		boolean isNieuw = blokkade.getId() == null;

		Kamer kamer = blokkade.getLocation();
		String wijzigOfNieuw = "Wijzig";

		if (isNieuw)
		{
			wijzigOfNieuw = "Nieuwe";
		}
		wijzigOfNieuw += " blokkade - " + ScreenitSession.get().getColoscopieCentrum().getNaam();

		final boolean magDatumWijzigen = ((isNieuw || blokkade.getRecurrence() == null || NoRecurrence.class.isAssignableFrom(blokkade.getRecurrence().getClass())))
			&& magAanpassen;

		String kamerString = "";
		final boolean heeftKamer = kamer != null;
		if (kamer != null)
		{
			kamerString = blokkade.getLocation().getName() + ":";
		}

		AbstractRecurrence rec = blokkade.getRecurrence();
		String herhalingsPeriodeString = "";
		if (rec != null && !NoRecurrence.class.isAssignableFrom(Hibernate.getClass(rec)))
		{
			herhalingsPeriodeString = DateUtil.formatShortDate(rec.getFirstOccurrence().getStartTime()) + " t/m " + DateUtil.formatShortDate(rec.getEndDate());
		}

		form.add(new Label("actie", wijzigOfNieuw));
		form.add(new Label("kamer", kamerString));
		form.add(new Label("tijd", blokkadeService.getPeriodeTekst(blokkade)).setVisible(!isNieuw));
		form.add(new Label("periode", herhalingsPeriodeString).setVisible(!herhalingsPeriodeString.isEmpty()));

		form.add(new Label("title"));

		List<Kamer> activeKamers = getActieveKamers();
		final WebMarkupContainer kamerContainer = new WebMarkupContainer("locationContainer");
		kamerContainer.setOutputMarkupId(true);
		form.add(kamerContainer);
		ScreenitDropdown<Kamer> kamerField = ComponentHelper.newDropDownChoice("location", ModelUtil.listRModel(activeKamers), new ChoiceRenderer<Kamer>("name", "id"));
		kamerField.setEnabled(!heeftKamer && magAanpassen);
		kamerContainer.add(kamerField);

		alleKamers = Model.of();
		CheckBox alleKamersCheckbox = new CheckBox("alleKamers", alleKamers);
		alleKamersCheckbox.setVisible(isNieuw);
		alleKamersCheckbox.add(new AjaxFormComponentUpdatingBehavior("change")
		{
			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				if (alleKamers.getObject())
				{
					kamerField.setEnabled(false);
				}
				else
				{
					kamerField.setEnabled(!heeftKamer && magAanpassen);
				}
				target.add(kamerContainer);
			}

		});
		form.add(alleKamersCheckbox);

		TextField<String> omschrijving = new TextField<>("description");
		omschrijving.setEnabled(magAanpassen);
		form.add(omschrijving);

		final TimeField endTimeField = new TimeField("endTime")
		{
			@Override
			public void convertInput()
			{
				MutableDateTime d = new MutableDateTime(getDefaultModelObject());
				Integer h = getHoursField().getConvertedInput();
				Integer m = getMinutesField().getConvertedInput();

				try
				{
					if (h != null)
					{
						d.set(DateTimeFieldType.hourOfDay(), h.intValue() % 24);
					}
					if (m != null)
					{
						d.setMinuteOfHour(m.intValue());
					}
					else
					{
						d.setMinuteOfHour(0);
					}

					TimeZone zone = getClientTimeZone();
					if (zone != null)
					{
						d.setMillis(DateTimeZone.forTimeZone(TimeZone.getDefault()).getMillisKeepLocal(DateTimeZone.forTimeZone(zone), d.getMillis()));
					}

					d.setSecondOfMinute(0);
					d.setMillisOfSecond(0); 

					setConvertedInput(d.toDate());

				}
				catch (RuntimeException e)
				{
					invalid();
				}
			}
		};
		endTimeField.setEnabled(magAanpassen);
		endTimeField.setOutputMarkupId(true);
		form.add(endTimeField);
		final ScreenITDateTimeField startTimeField = new ScreenITDateTimeField("startTime")
		{
			@Override
			public String getDatePickerLabel()
			{
				return "Datum van 'Periode'";
			}

			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				Date startTime = getConvertedInput();
				Date endTime = form.getModelObject().getEndTime();
				if (endTime != null && !heleDag.getObject())
				{
					form.getModelObject().setEndTime(DateUtil.plusTijdseenheid(startTime, duurAfspraakInMinuten, ChronoUnit.MINUTES));
					target.add(endTimeField);
				}

			}

			@Override
			protected DatePicker<Date> newDatePicker(String wicketId, IModel<Date> model)
			{
				DatePicker<Date> datePicker = super.newDatePicker(wicketId, model);
				datePicker.setMinDate((new DateOption(currentDateSupplier.getDate())));
				return datePicker;
			}
		};
		startTimeField.setEnabled(magAanpassen);
		startTimeField.setOutputMarkupId(true);
		startTimeField.add(DateValidator.minimum(currentDateSupplier.getDate()));
		form.add(startTimeField);

		DateTime end = new DateTime(endTimeField.getModelObject());
		DateTime start = new DateTime(startTimeField.getModelObject());
		heleDag = Model.of((isNieuw ? true : Boolean.valueOf(start.plusDays(1).equals(end))));
		final CheckBox heleDagBox = new CheckBox("heleDag", heleDag);
		heleDagBox.setEnabled(magAanpassen);
		heleDagBox.setOutputMarkupId(true);
		form.add(heleDagBox);

		startTimeField.add(new AjaxFormSubmitBehavior("change")
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				DateTime start = new DateTime(startTimeField.getModelObject());
				if (endTimeField.getModelObject() != null)
				{
					MutableDateTime end = new MutableDateTime(endTimeField.getModelObject());
					if (Boolean.TRUE.equals(heleDag.getObject()))
					{
						endTimeField.setModelObject(start.plusDays(1).withTimeAtStartOfDay().toDate());
						startTimeField.setModelObject(start.withTimeAtStartOfDay().toDate());
					}
					else
					{
						end.setDayOfYear(start.getDayOfYear());
						end.setYear(start.getYear());
						endTimeField.setModelObject(end.toDate());
					}
				}
				target.appendJavaScript(toggleStartTijdScript(heleDagBox, Boolean.TRUE.equals(heleDag.getObject())));
				target.add(startTimeField, endTimeField);
			}
		});

		heleDagBox.add(new OnChangeAjaxBehavior()
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				if (Boolean.TRUE.equals(heleDag.getObject()))
				{
					DateTime start = new DateTime(startTimeField.getModelObject()).withTimeAtStartOfDay().toDateTime();
					startTimeField.setModelObject(start.toDate());
					endTimeField.setModelObject(start.plusDays(1).toDate());
				}
				else
				{
					endTimeField.setModelObject(startTimeField.getModelObject()); 

				}
				target.appendJavaScript(toggleStartTijdScript(heleDagBox, Boolean.TRUE.equals(heleDag.getObject())));
				target.add(startTimeField, endTimeField);
			}
		});

		final AjaxRequestTarget target = RequestCycle.get().find(AjaxRequestTarget.class).orElse(null);
		if (target != null)
		{
			if (Boolean.TRUE.equals(heleDag.getObject()))
			{
				target.appendJavaScript(toggleStartTijdScript(heleDagBox, true));
			}
			if (!magDatumWijzigen)
			{
				target.appendJavaScript("$('.datum-tijd .add-on').hide();");
			}
		}
		addHerhaling(form, magAanpassen, isNieuw, startTimeField);

	}

	@Override
	protected boolean isDeleteButtonVisible()
	{
		return magVerwijderen;
	}

	private String toggleStartTijdScript(final CheckBox heleDagBox, boolean disable)
	{
		return String.format("$( document ).ready(function() {$('#" + heleDagBox.getMarkupId() + "').closest('.edit-blokkade').find('.tijd').prop('disabled', %s); });", disable);
	}

	private void addHerhaling(final Form<ColonBlokkade> form, final boolean magAanpassen, boolean isNieuw, DateTimeField startTime)
	{
		final DatePicker<Date> herhalingDatePicker = DatePickerHelper.newDatePicker("recurrence.endDate");
		herhalingDatePicker.add(ComponentHelper.newDbRangeValidator());

		final WebMarkupContainer herhalingContainer = new WebMarkupContainer("herhalingContainer");
		herhalingContainer.setVisible(isNieuw);
		herhalingContainer.setEnabled(magAanpassen);
		HerhalingPanel herhalingPanel = new HerhalingPanel("herhaling", getRecurrence())
		{
			@Override
			protected void onChangeHerhalingType(AjaxRequestTarget target, boolean showHerhalingEind)
			{
				getModelObject().getRecurrence().setEndDate(getModelObject().getStartTime());
				addOrReplaceHerhalingEindDatum(herhalingContainer, herhalingDatePicker, target, showHerhalingEind);
			}

		};
		herhalingContainer.add(herhalingPanel);
		form.add(herhalingContainer);

		addOrReplaceHerhalingEindDatum(herhalingContainer, herhalingDatePicker, null, false);

		form.add(new AbstractFormValidator()
		{
			@Override
			public void validate(Form<?> form)
			{
				Date startDatumTijd = startTime.getConvertedInput();
				if (startDatumTijd != null)
				{
					Date herhalingDatum = herhalingDatePicker.getConvertedInput();
					if (herhalingDatum != null && DateUtil.startDag(herhalingDatum).before(DateUtil.startDag(startDatumTijd)))
					{
						form.error(getString("error.herhaling.voor.startdatum"));
					}
				}

			}

			@Override
			public FormComponent<?>[] getDependentFormComponents()
			{
				return new FormComponent[] { startTime, herhalingDatePicker };
			}
		});
	}

	@Override
	protected boolean onBeforeOpslaan(ColonBlokkade blokkade) throws ValidatieException, OpslaanVerwijderenTijdBlokException
	{
		super.onBeforeOpslaan(blokkade);
		if (blokkade.getLocation() == null && !Boolean.TRUE.equals(alleKamers.getObject()))
		{
			throw new ValidatieException("error.kamer.of.alle.kamers.verplicht");
		}
		if (StringUtils.isBlank(blokkade.getDescription()))
		{
			throw new ValidatieException("error.omschrijving.verplicht");
		}
		return true;
	}

	@Override
	protected List<ColonBlokkade> transformTijdSlot(ColonBlokkade unsavedObject)
	{
		List<ColonBlokkade> list = new ArrayList<>();
		if (unsavedObject.getLocation() != null)
		{
			list.add(unsavedObject);
		}
		if (alleKamers.getObject())
		{
			for (Kamer kamer : getActieveKamers())
			{
				if (!Objects.equals(kamer, unsavedObject.getLocation()))
				{
					ColonBlokkade blokkade = unsavedObject.transientClone();
					var recurrence = unsavedObject.getRecurrence();
					if (recurrence != null && !NoRecurrence.class.isAssignableFrom(Hibernate.getClass(recurrence)))
					{
						var clonedRecurrence = recurrence.transientClone();
						clonedRecurrence.setFirstAppointment(blokkade);
						blokkade.setRecurrence(clonedRecurrence);
					}
					blokkade.setLocation(kamer);
					list.add(blokkade);
				}
			}
		}
		return list;
	}

	@Override
	protected void logAction(ColonBlokkade unsavedObject)
	{
		var instellingGebruiker = ScreenitSession.get().getLoggedInInstellingGebruiker();
		var intakelocatie = roosterService.getIntakelocatieVanInstellingGebruiker(instellingGebruiker);
		var logGebeurtenis = unsavedObject.getId() != null ? LogGebeurtenis.COLON_BLOKKADES_WIJZIG : LogGebeurtenis.COLON_BLOKKADES_NIEUW;
		var origBlokkade = EntityAuditUtil.getPreviousVersionOfEntity(unsavedObject, hibernateService.getHibernateSession());

		blokkadeService.logAction(unsavedObject, instellingGebruiker, intakelocatie, origBlokkade, logGebeurtenis, null, null);
	}
}
