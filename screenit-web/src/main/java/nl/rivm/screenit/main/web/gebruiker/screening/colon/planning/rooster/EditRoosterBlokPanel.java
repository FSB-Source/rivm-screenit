package nl.rivm.screenit.main.web.gebruiker.screening.colon.planning.rooster;

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

import java.text.SimpleDateFormat;
import java.time.LocalDateTime;
import java.util.Date;
import java.util.List;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.exceptions.OpslaanVerwijderenTijdBlokException;
import nl.rivm.screenit.main.exception.ValidatieException;
import nl.rivm.screenit.main.model.RecurrenceOption;
import nl.rivm.screenit.main.service.colon.ColonAfspraakSlotService;
import nl.rivm.screenit.main.service.colon.RoosterService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.component.form.ScreenITDateTimeField;
import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.colon.Kamer;
import nl.rivm.screenit.model.colon.RoosterItemStatus;
import nl.rivm.screenit.model.colon.planning.RoosterItem;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import nl.topicuszorg.wicket.input.validator.ValueValidator;
import nl.topicuszorg.wicket.planning.model.appointment.recurrence.AbstractRecurrence;
import nl.topicuszorg.wicket.planning.model.appointment.recurrence.NoRecurrence;
import nl.topicuszorg.wicket.planning.web.component.DatePickerHelper;
import nl.topicuszorg.wicket.planning.web.component.DateTimeField;
import nl.topicuszorg.wicket.planning.web.panel.recurrence.HerhalingPanel;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormComponentUpdatingBehavior;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.ChoiceRenderer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.FormComponent;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.markup.html.form.validation.AbstractFormValidator;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.request.cycle.RequestCycle;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.hibernate.Hibernate;
import org.joda.time.DateTime;
import org.joda.time.LocalDate;
import org.joda.time.Minutes;
import org.wicketstuff.datetime.markup.html.basic.DateLabel;
import org.wicketstuff.wiquery.ui.datepicker.DateOption;
import org.wicketstuff.wiquery.ui.datepicker.DatePicker;

public abstract class EditRoosterBlokPanel extends AbstractEditTijdSlotPanel<RoosterItem>
{
	@SpringBean
	private RoosterService roosterService;

	@SpringBean
	private ColonAfspraakSlotService afspraakSlotService;

	@SpringBean
	private LogService logService;

	private final IModel<Integer> aantalBlokken = Model.of(1);

	private RoosterItemStatus roosterItemStatus;

	private final Date origStartTime;

	public EditRoosterBlokPanel(String id, IModel<RoosterItem> model)
	{
		super(id, model);

		origStartTime = model.getObject().getStartTime();
	}

	@Override
	protected void initForm(final Form<RoosterItem> form)
	{
		final boolean magAanpassen = ScreenitSession.get().checkPermission(Recht.GEBRUIKER_LOCATIE_ROOSTER, Actie.AANPASSEN);

		RoosterItem roosterItem = form.getModelObject();
		boolean isNieuw = roosterItem.getId() == null;

		int diffStartEndMinutes = Math.abs(Minutes.minutesBetween(new DateTime(origStartTime), new DateTime(roosterItem.getEndTime())).getMinutes());
		int berekendAantalBlokken = diffStartEndMinutes / duurAfspraakInMinuten;
		if (berekendAantalBlokken * duurAfspraakInMinuten < diffStartEndMinutes)
		{
			berekendAantalBlokken++;
		}
		if (berekendAantalBlokken < 1)
		{
			berekendAantalBlokken = 1;
		}
		aantalBlokken.setObject(berekendAantalBlokken);
		if (isNieuw)
		{
			roosterItem.setEndTime(new DateTime(origStartTime).plusMinutes(berekendAantalBlokken * duurAfspraakInMinuten).toDate());
		}

		Kamer roosterKamer = roosterItem.getLocation();
		String wijzigOfNieuw = "Wijzig";

		if (isNieuw)
		{
			wijzigOfNieuw = "Nieuw";
		}
		wijzigOfNieuw += " roosterblok - " + ScreenitSession.get().getColoscopieCentrum().getNaam();

		roosterItemStatus = roosterService.getRoosterItemStatus(roosterItem);
		final boolean magDatumWijzigen = ((isNieuw || roosterItem.getRecurrence() == null || NoRecurrence.class.isAssignableFrom(roosterItem.getRecurrence().getClass()))
			&& !roosterItemStatus
			.equals(RoosterItemStatus.INTAKE_GEPLAND))
			&& magAanpassen;

		String kamerString = "";
		if (roosterKamer != null)
		{
			kamerString = roosterItem.getLocation().getName() + ":";
		}

		String tijdString = Constants.getDateTimeFormat().format(roosterItem.getStartTime()) + "-" + Constants.getTimeFormat().format(roosterItem.getEndTime());

		AbstractRecurrence rec = roosterItem.getRecurrence();
		String periodeString = "";
		if (rec != null && !NoRecurrence.class.isAssignableFrom(Hibernate.getClass(rec)))
		{
			periodeString = DateUtil.formatShortDate(rec.getFirstOccurrence().getStartTime()) + " t/m " + DateUtil.formatShortDate(rec.getEndDate());
		}

		form.add(new Label("actie", wijzigOfNieuw));
		form.add(new Label("kamer", kamerString));
		form.add(new Label("tijd", tijdString));
		form.add(new Label("periode", periodeString).setVisible(!periodeString.isEmpty()));
		form.add(new Label("scheduleSet.title"));

		List<Kamer> activeKamers = getActieveKamers();
		ScreenitDropdown<Kamer> kamers = ComponentHelper.newDropDownChoice("location", ModelUtil.listRModel(activeKamers), new ChoiceRenderer<Kamer>("name", "id"));
		kamers.setRequired(true);
		kamers.setEnabled(roosterKamer == null && magAanpassen);
		form.add(kamers);

		final Label endTimeField = DateLabel.forDatePattern("endTime", "HH:mm");
		endTimeField.setOutputMarkupId(true);
		endTimeField.setEnabled(magAanpassen);
		form.add(endTimeField);

		DateTimeField startTimeField = new ScreenITDateTimeField("startTime", magDatumWijzigen)
		{
			@Override
			public String getDatePickerLabel()
			{
				return "Datum van 'Starttijd'";
			}

			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				Date startTime = getConvertedInput();
				form.getModelObject().setEndTime(DateUtil.toUtilDate(calcEindtijd(startTime)));
				target.add(endTimeField);
			}

			@Override
			protected DatePicker<Date> newDatePicker(String wicketId, IModel<Date> model)
			{
				DatePicker<Date> datePicker = super.newDatePicker(wicketId, model);
				datePicker.setMinDate((new DateOption(currentDateSupplier.getDate())));
				return datePicker;
			}
		};

		startTimeField.setOutputMarkupId(true);
		startTimeField.setRequired(true);
		startTimeField.setEnabled(!roosterItemStatus.equals(RoosterItemStatus.INTAKE_GEPLAND) && magAanpassen);
		form.add(startTimeField);
		final AjaxRequestTarget target = RequestCycle.get().find(AjaxRequestTarget.class).orElse(null);
		if (target != null && !magDatumWijzigen)
		{
			target.appendJavaScript("$('.datum-tijd .add-on').hide();");
		}

		final TextField<Integer> aantalBlokkenField = new TextField<>("aantalBlokken", aantalBlokken, Integer.class);
		aantalBlokkenField.add(new AjaxFormComponentUpdatingBehavior("blur")
		{
			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				Date startTime = startTimeField.getModelObject();
				LocalDateTime startDateTime = DateUtil.toLocalDateTime(startTime);
				LocalDateTime nieuwEndTimeLocalDateTime = calcEindtijd(startTime);
				checkEindTijdOpZelfdeDag(startDateTime, nieuwEndTimeLocalDateTime);
				form.getModelObject().setEndTime(DateUtil.toUtilDate(nieuwEndTimeLocalDateTime));
				target.add(endTimeField);
			}

		});
		aantalBlokkenField.setRequired(true);
		aantalBlokkenField.setVisible(isNieuw);
		aantalBlokkenField.setEnabled(magAanpassen);
		aantalBlokkenField.add(ValueValidator.minimum(1));
		form.add(aantalBlokkenField);

		final Label duur = new Label("duur", duurAfspraakInMinuten);
		form.add(duur);

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
				Date startDatumTijd = startTimeField.getConvertedInput();
				if (startDatumTijd != null)
				{
					Date herhalingDatum = herhalingDatePicker.getConvertedInput();
					if (herhalingDatum != null && new LocalDate(herhalingDatum).isBefore(new LocalDate(startDatumTijd)))
					{
						form.error("De einddatum van de herhaling moet na de startdatum liggen");
					}
				}

			}

			@Override
			public FormComponent<?>[] getDependentFormComponents()
			{
				return new FormComponent[] { startTimeField, aantalBlokkenField, herhalingDatePicker };
			}
		});

	}

	private void checkEindTijdOpZelfdeDag(LocalDateTime startDateTime, LocalDateTime endDateTime)
	{
		try
		{
			afspraakSlotService.checkEindTijdOpZelfdeDag(startDateTime, endDateTime, ScreenitSession.get().getColoscopieCentrum());
		}
		catch (ValidatieException ex)
		{
			error(String.format(getString(ex.getMessage()), ex.getFormatArguments()));
		}
	}

	private LocalDateTime calcEindtijd(Date startTime)
	{
		return DateUtil.toLocalDateTime(startTime).plusMinutes(duurAfspraakInMinuten * aantalBlokken.getObject());
	}

	@Override
	protected List<RoosterItem> transformTijdSlot(RoosterItem unsavedObject)
	{
		return afspraakSlotService.splitAfspraakSlot(unsavedObject, aantalBlokken.getObject(), ScreenitSession.get().getColoscopieCentrum());
	}

	@Override
	protected boolean onBeforeOpslaan(RoosterItem roosteritem) throws ValidatieException, OpslaanVerwijderenTijdBlokException
	{
		super.onBeforeOpslaan(roosteritem);
		afspraakSlotService.checkCapaciteitBerekening(roosteritem, ScreenitSession.get().getColoscopieCentrum());
		var startDateTime = DateUtil.toLocalDateTime(roosteritem.getStartTime());
		var nieuwEndTimeLocalDateTime = calcEindtijd(roosteritem.getEndTime());
		afspraakSlotService.checkEindTijdOpZelfdeDag(startDateTime, nieuwEndTimeLocalDateTime, ScreenitSession.get().getColoscopieCentrum());
		return true;
	}

	@Override
	protected boolean isDeleteButtonVisible()
	{
		return super.isDeleteButtonVisible() && !roosterItemStatus.equals(RoosterItemStatus.GEBRUIKT_VOOR_CAPACITEIT);
	}

	@Override
	protected void logAction(RoosterItem unsavedObject)
	{
		Account account = ScreenitSession.get().getLoggedInAccount();
		Kamer kamer = unsavedObject.getLocation();
		SimpleDateFormat dateTimeFormat = Constants.getDateTimeFormat();
		SimpleDateFormat dateFormat = Constants.getDateFormat();
		String melding = dateTimeFormat.format(unsavedObject.getStartTime()) + ", " + kamer.getName() + ", " + kamer.getColoscopieCentrum().getNaam();
		if (unsavedObject.getRecurrence() != null && !NoRecurrence.class.isAssignableFrom(unsavedObject.getRecurrence().getClass()))
		{
			melding += ", in " + unsavedObject.getRecurrence().getName() + " reeks ";
			if (unsavedObject.getRecurrence().getEndDate() != null)
			{
				melding += " t/m " + dateFormat.format(unsavedObject.getRecurrence().getEndDate());
			}
		}
		if (unsavedObject.getId() == null)
		{
			logService.logGebeurtenis(LogGebeurtenis.ROOSTERBLOK_NIEUW, account, "#" + aantalBlokken.getObject() + " sloten, " + melding, Bevolkingsonderzoek.COLON);
		}
		else
		{
			melding = dateTimeFormat.format(origStartTime) + " -> " + melding;
			if (unsavedObject.getRecurrence() != null && !NoRecurrence.class.isAssignableFrom(unsavedObject.getRecurrence().getClass()))
			{
				melding += ", " + getString("label.recurrenceoption." + getRecurrenceOption().name().toLowerCase());
				if (getRecurrenceOption().equals(RecurrenceOption.WIJZIG_OF_VERWIJDER_TOT))
				{
					melding += dateFormat.format(getRecurrenceEditEnd());
				}
			}
			logService.logGebeurtenis(LogGebeurtenis.ROOSTERBLOK_WIJZIG, account, melding, Bevolkingsonderzoek.COLON);
		}

	}

}
