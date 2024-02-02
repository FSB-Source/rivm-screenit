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

import java.time.DayOfWeek;
import java.time.format.TextStyle;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.List;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.exceptions.OpslaanVerwijderenTijdBlokException;
import nl.rivm.screenit.main.exception.ValidatieException;
import nl.rivm.screenit.main.model.RecurrenceOption;
import nl.rivm.screenit.main.service.colon.RoosterService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.ConfirmingIndicatingAjaxLink;
import nl.rivm.screenit.main.web.component.SimpleStringResourceModel;
import nl.rivm.screenit.main.web.component.form.NonValidateForm;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.model.colon.Kamer;
import nl.rivm.screenit.model.colon.planning.ColonBlokkade;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.planning.model.ILocation;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;
import nl.topicuszorg.wicket.hibernate.cglib.ModelProxyHelper;
import nl.topicuszorg.wicket.planning.model.appointment.AbstractAppointment;
import nl.topicuszorg.wicket.planning.model.appointment.recurrence.AbstractRecurrence;
import nl.topicuszorg.wicket.planning.model.appointment.recurrence.DailyRecurrence;
import nl.topicuszorg.wicket.planning.model.appointment.recurrence.MonthlyRecurrence;
import nl.topicuszorg.wicket.planning.model.appointment.recurrence.NoRecurrence;
import nl.topicuszorg.wicket.planning.model.appointment.recurrence.WeeklyRecurrence;
import nl.topicuszorg.wicket.planning.model.appointment.recurrence.YearlyRecurrence;
import nl.topicuszorg.wicket.planning.services.RecurrenceService;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormChoiceComponentUpdatingBehavior;
import org.apache.wicket.ajax.form.AjaxFormComponentUpdatingBehavior;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.IChoiceRenderer;
import org.apache.wicket.markup.html.form.RadioChoice;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.validation.validator.DateValidator;
import org.hibernate.Hibernate;
import org.springframework.beans.support.PropertyComparator;
import org.wicketstuff.wiquery.core.javascript.JsQuery;
import org.wicketstuff.wiquery.core.javascript.JsStatement;
import org.wicketstuff.wiquery.ui.datepicker.DatePicker;

public abstract class AbstractEditTijdSlotPanel<T extends AbstractAppointment> extends GenericPanel<T>
{
	private static final long serialVersionUID = 1L;

	@SpringBean
	private RecurrenceService recurrenceService;

	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private LogService logService;

	@SpringBean
	protected RoosterService roosterService;

	@SpringBean
	protected ICurrentDateSupplier currentDateSupplier;

	private final PropertyModel<AbstractRecurrence> recurrence;

	private RecurrenceOption recurrenceOption = RecurrenceOption.WIJZIG_OF_VERWIJDER_ALLEEN_DEZE_AFSPRAAK;

	private Date recurrenceEditEnd;

	private boolean foutInRecurrenceEditEnd = false;

	private BootstrapDialog confirmPopup;

	protected final Integer duurAfspraakInMinuten;

	public AbstractEditTijdSlotPanel(String id, IModel<T> model)
	{
		super(id, model);
		duurAfspraakInMinuten = ScreenitSession.get().getColoscopieCentrum().getAfspraakDefinities().get(0).getDuurAfspraakInMinuten();
		confirmPopup = new BootstrapDialog("confirmPopup");
		confirmPopup.setOutputMarkupPlaceholderTag(true);
		add(confirmPopup);

		T abstractAppointment = model.getObject();
		if (abstractAppointment.getRecurrence() == null)
		{
			abstractAppointment.setRecurrence(new NoRecurrence());
		}
		this.recurrence = new PropertyModel<>(model, "recurrence");
	}

	protected String getTitle()
	{
		return getAppointmentOmschrijving(getModelObject());
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();

		final NonValidateForm<T> editForm = new NonValidateForm<>("editForm", getModel());
		add(editForm);

		initForm(editForm);

		addRecurrenceForm(editForm);
		addOpslaanLink(editForm);
		addDeleteLink(editForm);
	}

	protected abstract void initForm(Form<T> form);

	private void addOpslaanLink(final Form<T> editForm)
	{
		IndicatingAjaxSubmitLink opslaanLink = new IndicatingAjaxSubmitLink("opslaanLink", editForm)
		{
			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				boolean hasError = false;

				if (getForm() instanceof NonValidateForm)
				{
					NonValidateForm<T> nonValidateForm = (NonValidateForm<T>) editForm;
					nonValidateForm.setBypassValidation(false);
					nonValidateForm.process(null);
					hasError = nonValidateForm.hasError();
				}

				if (hasError)
				{
					return;
				}
				@SuppressWarnings("unchecked")
				T unsavedObject = (T) getForm().getModelObject();
				unsavedObject = ModelProxyHelper.deproxy(unsavedObject);

				try
				{
					onBeforeOpslaan(unsavedObject);
					logAction(unsavedObject);
					if (unsavedObject.getId() != null && unsavedObject.getRecurrence() != null
						&& !NoRecurrence.class.isAssignableFrom(unsavedObject.getRecurrence().getClass()))
					{
						if (getRecurrenceOption().equals(RecurrenceOption.WIJZIG_OF_VERWIJDER_ALLEEN_DEZE_AFSPRAAK))
						{
							hibernateService.update(unsavedObject);
						}
						else if (getRecurrenceOption().equals(RecurrenceOption.WIJZIG_OF_VERWIJDER_HELE_SERIE))
						{
							recurrenceService.changeHerhalingChain(unsavedObject, ModelProxyHelper.deproxy(AbstractEditTijdSlotPanel.this.recurrence.getObject()), null);
						}
						else if (getRecurrenceOption().equals(RecurrenceOption.WIJZIG_OF_VERWIJDER_VANAF_HIER))
						{
							recurrenceService.changeHerhalingChainVanaf(unsavedObject, null);
						}
						else if (getRecurrenceOption().equals(RecurrenceOption.WIJZIG_OF_VERWIJDER_TOT))
						{
							Date eind = DateUtil.eindDag(getRecurrenceEditEnd());
							recurrenceService.changeHerhalingChainVanafEnTot(unsavedObject, eind, null);
						}

						onClose(target, new CalendarRefresher<>(false, unsavedObject, getRecurrenceOption()));
					}
					else if (unsavedObject.getRecurrence() != null && !NoRecurrence.class.isAssignableFrom(unsavedObject.getRecurrence().getClass()))
					{
						onToevoegen(target, unsavedObject);
					}
					else if (unsavedObject.getId() == null)
					{
						List<T> transformedTijdSloten = transformTijdSlot(unsavedObject);
						for (T transformedTijdSlot : transformedTijdSloten)
						{
							hibernateService.saveOrUpdate(transformedTijdSlot);
						}
						onClose(target, new CalendarRefresher<>(false, transformedTijdSloten, null));
					}
					else
					{
						hibernateService.saveOrUpdate(unsavedObject);
						onClose(target, new CalendarRefresher<>(false, unsavedObject, null));
					}
				}
				catch (ValidatieException ex)
				{
					error(String.format(getString(ex.getMessage()), ex.getFormatArguments()));
				}
				catch (OpslaanVerwijderenTijdBlokException ex)
				{
					error(getString(ex.getMessage()) + " " + ex.getAdditionalMessageInfo());
				}
			}

			private void onToevoegen(AjaxRequestTarget target, T unsavedObject)
			{
				List<T> transformedTijdSloten = transformTijdSlot(unsavedObject);
				for (T transformedTijdSlot : transformedTijdSloten)
				{
					roosterService.toevoegenHerhaling(transformedTijdSlot);
				}
				onClose(target, new CalendarRefresher<>(false, transformedTijdSloten, null));
			}

			@Override
			public boolean isVisible()
			{
				return isOpslaanButtonVisible();
			}
		};

		opslaanLink.setDefaultFormProcessing(false);
		add(opslaanLink);

		opslaanLink.add(new Label("opslaanTekst", getOpslaanTekst()));
	}

	protected boolean isOpslaanButtonVisible()
	{
		return ScreenitSession.get().checkPermission(Recht.GEBRUIKER_LOCATIE_ROOSTER, Actie.AANPASSEN);
	}

	protected abstract List<T> transformTijdSlot(T unsavedObject);

	private void addDeleteLink(final Form<T> editForm)
	{
		final IndicatingAjaxSubmitLink deleteSubmit = new IndicatingAjaxSubmitLink("deleteSubmit", editForm)
		{
			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				if (editForm instanceof NonValidateForm)
				{
					NonValidateForm<T> nonValidateForm = (NonValidateForm<T>) editForm;
					nonValidateForm.setBypassValidation(true);
					nonValidateForm.process(null);
				}

				T abstractAppointmentToDelete = AbstractEditTijdSlotPanel.this.getModelObject();
				abstractAppointmentToDelete = ModelProxyHelper.deproxy(abstractAppointmentToDelete);
				var kamer = (Kamer) abstractAppointmentToDelete.getLocation();
				var melding = Constants.getDateTimeFormat().format(abstractAppointmentToDelete.getStartTime()) + ", " + kamer.getName() + ", "
					+ kamer.getColoscopieCentrum().getNaam();
				CalendarRefresher<T> refresher = new CalendarRefresher<>(true, abstractAppointmentToDelete, getRecurrenceOption());
				logAction(abstractAppointmentToDelete);

				if (abstractAppointmentToDelete.getRecurrence() != null && !NoRecurrence.class.isAssignableFrom(abstractAppointmentToDelete.getRecurrence().getClass()))
				{
					if (getRecurrenceOption().equals(RecurrenceOption.WIJZIG_OF_VERWIJDER_ALLEEN_DEZE_AFSPRAAK))
					{
						recurrenceService.deleteHerhaling(abstractAppointmentToDelete);
					}
					else if (getRecurrenceOption().equals(RecurrenceOption.WIJZIG_OF_VERWIJDER_VANAF_HIER))
					{
						recurrenceService.deleteHerhalingChainVanaf(abstractAppointmentToDelete);
					}
					else if (getRecurrenceOption().equals(RecurrenceOption.WIJZIG_OF_VERWIJDER_HELE_SERIE))
					{
						recurrenceService.deleteHerhalingChain(abstractAppointmentToDelete.getRecurrence());
					}
					else if (getRecurrenceOption().equals(RecurrenceOption.WIJZIG_OF_VERWIJDER_TOT))
					{
						Date eind = DateUtil.eindDag(getRecurrenceEditEnd());
						recurrenceService.deleteHerhalingChainVanafEnTot(abstractAppointmentToDelete, eind);
						refresher.setAangepastTot(eind);
					}
				}
				else
				{
					hibernateService.delete(abstractAppointmentToDelete);
				}
				if (abstractAppointmentToDelete instanceof ColonBlokkade)
				{
					logService.logGebeurtenis(LogGebeurtenis.COLON_BLOKKADES_VERWIJDEREN, ScreenitSession.get().getLoggedInAccount(), melding, Bevolkingsonderzoek.COLON);
				}
				else
				{
					logService.logGebeurtenis(LogGebeurtenis.ROOSTERBLOK_VERWIJDEREN, ScreenitSession.get().getLoggedInAccount(), melding, Bevolkingsonderzoek.COLON);
				}
				onClose(target, refresher);
			}

			@Override
			public boolean isVisible()
			{
				return isDeleteButtonVisible();
			}

		};
		add(deleteSubmit);

		IndicatingAjaxLink<T> deleteLink = new ConfirmingIndicatingAjaxLink<>("deleteLink", confirmPopup, getConfirmPopupKey())
		{
			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{

				JsStatement statement = new JsQuery(deleteSubmit).$().chain("click");

				target.appendJavaScript(statement.render());
			}

			@Override
			protected boolean continueConfirmation()
			{
				T abstractAppointmentToDelete = AbstractEditTijdSlotPanel.this.getModelObject();
				abstractAppointmentToDelete = ModelProxyHelper.deproxy(abstractAppointmentToDelete);
				return onBeforeDelete(abstractAppointmentToDelete);
			}

			@Override
			protected IModel<String> getContentStringModel()
			{
				return Model.of(getTitle());
			}

			@Override
			public boolean isVisible()
			{
				return isDeleteButtonVisible();
			}

		};
		add(deleteLink);
		deleteLink.add(new Label("deleteTekst", getDeleteTekst()));
	}

	protected boolean isDeleteButtonVisible()
	{
		T modelobject = getModelObject();
		return modelobject.getId() != null && ScreenitSession.get().checkPermission(Recht.GEBRUIKER_LOCATIE_ROOSTER, Actie.VERWIJDEREN);
	}

	protected String getConfirmPopupKey()
	{
		return "verwijder.popup";
	}

	private void addRecurrenceForm(Form<T> form)
	{
		WebMarkupContainer recurrenceOptionContainer = new WebMarkupContainer("recurrenceOptionContainer");
		form.add(recurrenceOptionContainer);
		final DatePicker<Date> datePicker = ComponentHelper.newYearDatePicker("recurrenceEditEnd", new PropertyModel<>(this, "recurrenceEditEnd"));
		T modelObject = getModelObject();
		Date startTime = modelObject.getStartTime();
		if (startTime == null)
		{
			startTime = currentDateSupplier.getDate();
		}
		datePicker.add(DateValidator.minimum(DateUtil.toUtilDateMidnight(startTime)));
		final WebMarkupContainer recurrenceEditEndContainer = new WebMarkupContainer("recurrenceEditEndContainer");
		recurrenceEditEndContainer.setVisible(RecurrenceOption.WIJZIG_OF_VERWIJDER_TOT.equals(getRecurrenceOption()));
		recurrenceEditEndContainer.setOutputMarkupPlaceholderTag(true);
		Date endTime = modelObject.getEndTime();
		if (endTime == null)
		{
			endTime = currentDateSupplier.getDate();
		}
		setRecurrenceEditEnd(DateUtil.eindDag(endTime));

		recurrenceEditEndContainer.add(datePicker);
		recurrenceOptionContainer.add(recurrenceEditEndContainer);
		datePicker.add(new AjaxFormComponentUpdatingBehavior("change")
		{
			private static final long serialVersionUID = 1L;

			@Override
			protected void onError(AjaxRequestTarget target, RuntimeException e)
			{
				foutInRecurrenceEditEnd = true;
			}

			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				foutInRecurrenceEditEnd = false;
			}
		});
		List<RecurrenceOption> choices = new ArrayList<>(Arrays.asList(RecurrenceOption.values()));
		choices.remove(RecurrenceOption.WIJZIG_OF_VERWIJDER_HELE_SERIE);
		RadioChoice<RecurrenceOption> radioChoice = new RadioChoice<>("recurrenceOption", new PropertyModel<>(this, "recurrenceOption"), choices,
			new IChoiceRenderer<>()
			{

				private static final long serialVersionUID = 1L;

				@Override
				public Object getDisplayValue(RecurrenceOption object)
				{
					return getString("label.recurrenceoption." + object.name().toLowerCase());
				}

				@Override
				public String getIdValue(RecurrenceOption object, int index)
				{
					return object.name();
				}

				@Override
				public RecurrenceOption getObject(String id, IModel<? extends List<? extends RecurrenceOption>> choices)
				{
					if (id != null)
					{
						return choices.getObject().stream().filter(r -> r.name().equals(id)).findFirst().orElse(null);
					}
					return null;
				}
			});
		recurrenceOptionContainer.add(radioChoice);
		radioChoice.add(new AjaxFormChoiceComponentUpdatingBehavior()
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				recurrenceEditEndContainer.setVisible(RecurrenceOption.WIJZIG_OF_VERWIJDER_TOT.equals(getRecurrenceOption()));
				target.add(recurrenceEditEndContainer);
			}
		});
		radioChoice.setPrefix("<div class='controls'><label class='radio'>");
		radioChoice.setSuffix("</label></div>");
		recurrenceOptionContainer.setVisible(getModelObject().getId() != null && !NoRecurrence.class.isAssignableFrom(recurrence.getObject().getClass())
			&& ScreenitSession.get().checkPermission(Recht.GEBRUIKER_LOCATIE_ROOSTER, Actie.AANPASSEN));
		add(form);
	}

	protected void addOrReplaceHerhalingEindDatum(WebMarkupContainer herhalingContainer, DatePicker<Date> datePicker, AjaxRequestTarget target, boolean showHerhalingEind)
	{
		WebMarkupContainer herhalingEindDatum = new WebMarkupContainer("herhalingEindDatum");
		herhalingEindDatum.setOutputMarkupPlaceholderTag(true);
		herhalingEindDatum.setVisible(getModelObject().getId() == null);

		herhalingEindDatum.setVisible(showHerhalingEind);
		herhalingEindDatum.addOrReplace(datePicker);
		herhalingContainer.addOrReplace(herhalingEindDatum);

		if (target != null)
		{
			target.add(herhalingEindDatum);
		}
	}

	protected PropertyModel<AbstractRecurrence> getRecurrence()
	{
		return recurrence;
	}

	@Override
	protected void onDetach()
	{
		recurrence.detach();
		super.onDetach();
	}

	protected IModel<String> getDeleteTekst()
	{
		return new SimpleStringResourceModel("delete.tekst");
	}

	protected IModel<String> getOpslaanTekst()
	{
		return new SimpleStringResourceModel("opslaan.tekst");
	}

	protected boolean onBeforeDelete(T objectToDelete)
	{
		return !foutInRecurrenceEditEnd;
	}

	protected boolean onBeforeOpslaan(T appointment) throws ValidatieException, OpslaanVerwijderenTijdBlokException
	{
		roosterService.valideerTijdslot(appointment);
		return true;
	}

	protected abstract void onClose(AjaxRequestTarget target, CalendarRefresher<T> refresher);

	public void setRecurrenceOption(RecurrenceOption recurrenceOption)
	{
		this.recurrenceOption = recurrenceOption;
	}

	public RecurrenceOption getRecurrenceOption()
	{
		return recurrenceOption;
	}

	public void setRecurrenceEditEnd(Date recurrenceEditEnd)
	{
		this.recurrenceEditEnd = recurrenceEditEnd;
	}

	public Date getRecurrenceEditEnd()
	{
		return recurrenceEditEnd;
	}

	protected abstract void logAction(T unsavedObject);

	protected ArrayList<Kamer> getActieveKamers()
	{
		ArrayList<Kamer> activeKamers = new ArrayList<>();
		for (Kamer kamer : ScreenitSession.get().getColoscopieCentrum().getKamers())
		{
			if (!Boolean.FALSE.equals(kamer.getActief()))
			{
				activeKamers.add(kamer);
			}
		}
		activeKamers.sort(new PropertyComparator<>("name", false, true));
		return activeKamers;
	}

	protected String getAppointmentOmschrijving(T appointment)
	{
		String appointmentOmschrijving = "";
		ILocation roosterKamer = appointment.getLocation();
		AbstractRecurrence rec = appointment.getRecurrence();
		String recString = "";
		if (rec != null && !NoRecurrence.class.isAssignableFrom(Hibernate.getClass(rec)))
		{
			String beginDatum = DateUtil.formatShortDate(appointment.getStartTime());
			String eindDatum = DateUtil.formatShortDate(rec.getEndDate());
			switch (getRecurrenceOption())
			{
			case WIJZIG_OF_VERWIJDER_ALLEEN_DEZE_AFSPRAAK:
				break;
			case WIJZIG_OF_VERWIJDER_TOT:
				eindDatum = DateUtil.formatShortDate(getRecurrenceEditEnd());

			case WIJZIG_OF_VERWIJDER_VANAF_HIER:
				recString += "<br/> <strong>Frequentie: </strong>";
				Object recDeproxy = HibernateHelper.deproxy(rec);
				if (recDeproxy instanceof WeeklyRecurrence)
				{
					WeeklyRecurrence weeklyRec = (WeeklyRecurrence) recDeproxy;
					int omDeAantalWeken = weeklyRec.getRecurrenceInterval();
					if (omDeAantalWeken == 1)
					{
						recString += "Wekelijks";
					}
					else
					{
						recString += "Om de " + omDeAantalWeken + " weken";
					}

					List<Integer> dagen = new ArrayList<>(weeklyRec.getDagen());
					recString += "<br/> <strong>Dag";
					if (dagen.size() > 1)
					{
						recString += "en";
					}
					recString += ":</strong>";
					Collections.sort(dagen);
					boolean first = true;
					for (Integer dagIndex : dagen)
					{

						if (first)
						{
							recString += " ";
							first = false;
						}
						else
						{
							recString += ", ";
						}
						recString += getDagVanIndex(dagIndex);
					}
				}
				else if (recDeproxy instanceof MonthlyRecurrence)
				{
					MonthlyRecurrence monthlyRec = (MonthlyRecurrence) recDeproxy;
					int omDeAantalMaanden = monthlyRec.getRecurrenceInterval();
					if (omDeAantalMaanden == 1)
					{
						recString += "Maandelijks";
					}
					else
					{
						recString += "Om de " + omDeAantalMaanden + " maanden";
					}

					recString += "<br/> <strong>Dag: </strong>" + "De " + monthlyRec.getXthWeekDay() + "e " + getDagVanIndex(monthlyRec.getDay()) + " van de maand";
				}
				else if (recDeproxy instanceof DailyRecurrence)
				{
					DailyRecurrence dailyRec = (DailyRecurrence) recDeproxy;
					recString += "Dagelijks,";
					if (dailyRec.getBusinessDaysOnly())
					{
						recString += " alleen op werkdagen";
					}
					else
					{
						recString += " ook in het weekend";
					}
				}
				else if (recDeproxy instanceof YearlyRecurrence)
				{
					YearlyRecurrence yearlyRecurrence = (YearlyRecurrence) recDeproxy;
					int omDeAantalJaren = yearlyRecurrence.getRecurrenceInterval();
					if (omDeAantalJaren == 1)
					{
						recString += "Jaarlijks";
					}
					else
					{
						recString += "Om de " + omDeAantalJaren + " jaar";
					}
				}
				recString += "<br/> <strong>Periode: </strong>" + beginDatum + " t/m " + eindDatum;
				break;
			}
		}
		appointmentOmschrijving += ScreenitSession.get().getColoscopieCentrum().getNaam();
		if (roosterKamer != null)
		{
			appointmentOmschrijving += " - " + appointment.getLocation().getName();
		}
		appointmentOmschrijving += ": " + DateUtil.formatShortDate(appointment.getStartTime()) + " van " + DateUtil.formatTime(appointment.getStartTime()) + " tot "
			+ DateUtil.formatTime(appointment.getEndTime()) + recString;
		return appointmentOmschrijving;
	}

	private static String getDagVanIndex(Integer index)
	{
		return DayOfWeek.of(index).getDisplayName(TextStyle.FULL, Constants.LOCALE_NL);
	}

}
