package nl.rivm.screenit.main.web.gebruiker.screening.colon.planning.rooster;

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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import nl.rivm.screenit.exceptions.OpslaanVerwijderenTijdBlokException;
import nl.rivm.screenit.main.service.colon.RoosterService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.component.modal.IDialog;
import nl.rivm.screenit.main.web.gebruiker.screening.colon.planning.RoosterAantallenPerJaarPanel;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.colon.ColoscopieCentrum;
import nl.rivm.screenit.model.colon.Kamer;
import nl.rivm.screenit.model.colon.RoosterItemStatus;
import nl.rivm.screenit.model.colon.enums.ColonTijdSlotType;
import nl.rivm.screenit.model.colon.planning.ColonBlokkade;
import nl.rivm.screenit.model.colon.planning.RoosterItem;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.colon.ColonUitnodigingService;
import nl.topicus.wicket.calendar.CalendarAjaxBehavior.CalendarCallbackEvents;
import nl.topicus.wicket.calendar.ICalendarCallback;
import nl.topicus.wicket.calendar.ICalendarResource;
import nl.topicus.wicket.calendar.model.CalendarEventProperties;
import nl.topicus.wicket.calendar.navigation.NavigatieStatus;
import nl.topicus.wicket.calendar.options.BusinessHoursOption;
import nl.topicus.wicket.calendar.schedule.Schedule;
import nl.topicus.wicket.calendar.schedule.ScheduleOptions;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.planning.model.IAppointment;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import nl.topicuszorg.wicket.planning.model.appointment.AbstractAppointment;
import nl.topicuszorg.wicket.planning.model.appointment.Location;
import nl.topicuszorg.wicket.planning.model.appointment.recurrence.AbstractRecurrence;
import nl.topicuszorg.wicket.planning.model.appointment.recurrence.NoRecurrence;
import nl.topicuszorg.wicket.planning.model.schedule.ScheduleItem;
import nl.topicuszorg.wicket.planning.model.schedule.ScheduleSet;
import nl.topicuszorg.wicket.planning.services.AppointmentService;
import nl.topicuszorg.wicket.planning.services.ScheduleService;
import nl.topicuszorg.wicket.planning.util.Periode;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.codehaus.jackson.JsonNode;
import org.hibernate.ObjectNotFoundException;
import org.joda.time.DateTime;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.wicketstuff.shiro.ShiroConstraint;
import org.wicketstuff.wiquery.core.javascript.JsQuery;
import org.wicketstuff.wiquery.core.javascript.JsScope;
import org.wicketstuff.wiquery.core.javascript.JsScopeContext;
import org.wicketstuff.wiquery.core.javascript.JsUtils;

@SecurityConstraint(
	actie = Actie.INZIEN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = Recht.GEBRUIKER_LOCATIE_ROOSTER,
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.COLON })
public class LocatieRooster extends GenericPanel<Kamer> implements ICalendarCallback
{

	private static final long serialVersionUID = 1L;

	private static final String CALENDAR_ID = "calendar";

	private static final Logger LOG = LoggerFactory.getLogger(LocatieRooster.class);

	@SpringBean
	private AppointmentService appointmentService;

	@SpringBean
	private RoosterService roosterService;

	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private ScheduleService scheduleService;

	@SpringBean
	private ColonUitnodigingService colonUitnodigingService;

	private Schedule calendar;

	private final WebMarkupContainer calendarContainer;

	private BootstrapDialog dialog;

	private Date start;

	private Date end;

	private final NavigatieStatus navigatieStatus = new NavigatieStatus();

	private final RoosterAantallenPerJaarPanel roosterAantallenPerJaarPanel;

	public LocatieRooster(String id, final RoosterAantallenPerJaarPanel roosterAantallenPerJaarPanel, BootstrapDialog dialog)
	{
		super(id);
		this.roosterAantallenPerJaarPanel = roosterAantallenPerJaarPanel;
		this.dialog = dialog;
		navigatieStatus.setCalenderDatum(colonUitnodigingService.getGeprognotiseerdeIntakeDatum(true).dayOfWeek().withMinimumValue().getMillis());

		calendarContainer = new WebMarkupContainer("calendarContainer");
		calendarContainer.setOutputMarkupId(true);
		add(calendarContainer);
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();
		addCalendar();
	}

	public void editRoosteritem(AjaxRequestTarget target, RoosterItem item)
	{
		IModel<RoosterItem> itemModel = ModelUtil.cModel(item);

		try
		{
			final Date origRecEndDateTime = getOrigRecEndDateTime(item);

			dialog.setContent(new EditRoosterBlokPanel(IDialog.CONTENT_ID, itemModel)
			{

				private static final long serialVersionUID = 1L;

				@Override
				protected void onClose(AjaxRequestTarget target, CalendarRefresher<RoosterItem> refresher)
				{
					if (refresher.isVerwijderd())
					{
						updateCalendar(target);
						info(getString("roosteritem.verwijderd"));
					}
					else
					{
						updateCalendar(target);
						target.appendJavaScript(calendar.refresh(getRefreshMap(getModelObject().getLocation(), start, end)).getStatement());
						info(getString("roosteritem.opgeslagen"));
					}
					dialog.close(target);
					target.add(roosterAantallenPerJaarPanel);
				}

				@Override
				protected boolean onBeforeOpslaan(RoosterItem roosteritem)
				{
					boolean isValideRoosterItem = super.onBeforeOpslaan(roosteritem);
					if (isValideRoosterItem)
					{
						try
						{
							roosterService.magRoosterItemOpslaanVerwijderen(roosteritem, getRecurrenceOption(), getRecurrenceEditEnd(), origRecEndDateTime, true);
						}
						catch (OpslaanVerwijderenTijdBlokException e)
						{
							isValideRoosterItem = false;
							error(createErrorMessage(e));
						}
					}
					return isValideRoosterItem;
				}

				@Override
				protected boolean onBeforeDelete(RoosterItem objectToDelete)
				{
					boolean isValideRoosterItem = super.onBeforeDelete(objectToDelete);
					if (isValideRoosterItem)
					{
						try
						{
							roosterService.magRoosterItemOpslaanVerwijderen(objectToDelete, getRecurrenceOption(), getRecurrenceEditEnd(), origRecEndDateTime, false);
						}
						catch (OpslaanVerwijderenTijdBlokException e)
						{
							isValideRoosterItem = false;
							error(createErrorMessage(e));
						}
					}
					return isValideRoosterItem;
				}

			});

			dialog.open(target);
		}
		catch (ObjectNotFoundException e)
		{
			LOG.error("Kan roosterblok edit popup niet openen", e);
		}

	}

	private static Date getOrigRecEndDateTime(AbstractAppointment item)
	{
		Date eindTime = item.getEndTime();
		if (item.getRecurrence() != null && !NoRecurrence.class.isAssignableFrom(item.getRecurrence().getClass()))
		{
			AbstractRecurrence recurrence = item.getRecurrence();
			AbstractAppointment last = (AbstractAppointment) recurrence.getLastOccurrence();
			if (last != null)
			{
				eindTime = last.getEndTime();
			}
			else
			{
				eindTime = ((AbstractAppointment) recurrence.getFirstOccurrence()).getEndTime();
			}
		}
		return eindTime;
	}

	protected void addCalendar()
	{
		calendar = newSchedule(CalendarCallbackEvents.values(), getSpecificScheduleOptions());
		calendarContainer.add(calendar);
	}

	@Override
	public Map<ICalendarResource, List<? extends IAppointment>> getAppointments(Date startp, Date endp)
	{
		this.start = startp;
		this.end = endp;
		this.navigatieStatus.setCalenderDatum(startp.getTime());
		return getAppointments();
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	private Map<ICalendarResource, List<? extends IAppointment>> getAppointments()
	{
		Map<ICalendarResource, List<? extends IAppointment>> kamerEventMap = new TreeMap<>(new KamerCalendarResourceComparator());

		List<Kamer> kamers = new ArrayList<>();
		ColoscopieCentrum intakelocatie = ScreenitSession.get().getColoscopieCentrum();
		if (intakelocatie != null)
		{
			for (Kamer kamer : intakelocatie.getKamers())
			{
				if (Boolean.TRUE.equals(kamer.getActief()))
				{
					kamers.add(kamer);
				}
			}
		}

		if (CollectionUtils.isNotEmpty(kamers))
		{

			for (Kamer kamer : kamers)
			{
				kamerEventMap.put(kamer, new ArrayList<IAppointment>());
			}

			List<RoosterItem> items = roosterService.getRooster(new Periode(start, end), kamers);

			for (RoosterItem item : items)
			{
				List afspraken = kamerEventMap.get(item.getLocation());
				afspraken.add(item);
				RoosterItemStatus roosterItemStatus = roosterService.getRoosterItemStatus(item);
				item.setEventType(roosterItemStatus.getEventType());
			}

			List<ColonBlokkade> blokkades = roosterService.getBlokkades(new Periode(start, end), kamers);
			for (ColonBlokkade blokkade : blokkades)
			{
				List afspraken = kamerEventMap.get(blokkade.getLocation());
				afspraken.add(blokkade);
			}
		}

		return kamerEventMap;
	}

	void updateCalendar(AjaxRequestTarget target)
	{
		target.appendJavaScript(new JsQuery(calendar).$().chain("schedule", JsUtils.quotes("renderCalender")).render());
	}

	private Schedule newSchedule(final CalendarCallbackEvents[] callBackEvents, ScheduleOptions scheduleOptions)
	{
		Schedule nieuweSchedule = new Schedule(CALENDAR_ID, this, scheduleOptions, callBackEvents)
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onEvent(CalendarEventProperties eventProperties, AjaxRequestTarget target, CalendarCallbackEvents callbackEvent)
			{
				if (eventProperties.getId() == null)
				{
					if (ScreenitSession.get().checkPermission(Recht.GEBRUIKER_LOCATIE_ROOSTER, Actie.TOEVOEGEN))
					{
						RoosterItem item = new RoosterItem();
						item.setRecurrence(new NoRecurrence());
						item.setStartTime(eventProperties.getStart());
						item.setEndTime(eventProperties.getEind());

						List<ScheduleSet> alleRoosterblokken = scheduleService.getAlleRoosterblokken(null);
						ScheduleSet scheduleSet = alleRoosterblokken.get(0);
						item.setScheduleSet(scheduleSet);
						item.setTitle(scheduleSet.getTitle());

						if (StringUtils.isNotBlank(eventProperties.getRow()))
						{
							Kamer locatie = hibernateService.load(Kamer.class, Long.parseLong(eventProperties.getRow()));
							item.setLocation(locatie);
						}
						editRoosteritem(target, item);
					}
				}
				else
				{
					JsonNode jsonNode = eventProperties.getNode().findValue("title");
					String text = jsonNode.asText();
					ColonTijdSlotType slotType = ColonTijdSlotType.getTypeOnTitle(text);
					if (slotType == ColonTijdSlotType.ROOSTER_ITEM)
					{
						RoosterItem item = hibernateService.load(RoosterItem.class, eventProperties.getId());
						editRoosteritem(target, item);
					}
					else if (slotType == ColonTijdSlotType.BLOKKADE)
					{
						ColonBlokkade blokkade = hibernateService.load(ColonBlokkade.class, eventProperties.getId());
						editBlokkade(target, blokkade);
					}
				}
			}

			@Override
			protected NavigatieStatus getScheduleNavigatieStatus()
			{
				return navigatieStatus;
			}

			@Override
			protected void replaceSchedule(AjaxRequestTarget target, ScheduleOptions replaceOptions)
			{
				target.prependJavaScript(destroy().render());
				calendarContainer.addOrReplace(newSchedule(callBackEvents, replaceOptions));
				target.add(calendarContainer);
			}

			@Override
			protected ScheduleOptions getScheduleOptions()
			{
				return getSpecificScheduleOptions();
			}
		};

		return nieuweSchedule;
	}

	private ScheduleOptions getSpecificScheduleOptions()
	{
		ColoscopieCentrum instantie = ScreenitSession.get().getColoscopieCentrum();
		DateTime tijdVan = new DateTime(instantie.getOpeningVan());
		DateTime tijdTot = new DateTime(instantie.getOpeningTot());

		ScheduleOptions scheduleOptions = ScheduleOptions.defaultOptions();

		scheduleOptions.setSnapToGrid(true);
		scheduleOptions.setDate(navigatieStatus.getCalenderDatum());
		scheduleOptions.setBusinessHours(new BusinessHoursOption(tijdVan.getHourOfDay(), tijdTot.getHourOfDay()));
		scheduleOptions.setWeek(!navigatieStatus.isDagWeergave());

		JsScope tooltip = new JsScope("elementen")
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void execute(JsScopeContext scopeContext)
			{
			}
		};

		scheduleOptions.setTooltip(tooltip);

		scheduleOptions.setHeight(new JsScope("schedule")
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void execute(JsScopeContext scopeContext)
			{

			}
		});

		return scheduleOptions;
	}

	private Map<ICalendarResource, List<? extends IAppointment>> getRefreshMap(Location locatie, Date startTijd, Date eindTijd)
	{
		Map<ICalendarResource, List<? extends IAppointment>> refreshMap = new HashMap<>();

		List<IAppointment> appointments = new ArrayList<>();

		appointments.addAll(appointmentService.getAppointmentsMetFilter(locatie, startTijd, eindTijd, new RoosterItem()));

		appointments.addAll(appointmentService.getAppointmentsMetFilter(locatie, startTijd, eindTijd, new ColonBlokkade()));

		if (appointments.isEmpty())
		{

			appointments.add(new ScheduleItem()
			{

				private static final long serialVersionUID = 1L;

				@Override
				public Long getId()
				{
					return 0L;
				}

				@Override
				protected String getTooltip()
				{
					return null;
				}
			});
		}
		refreshMap.put(locatie, appointments);

		return refreshMap;
	}

	private String createErrorMessage(OpslaanVerwijderenTijdBlokException e)
	{
		return getString(e.getMessage()) + e.getAdditionalMessageInfo();
	}

	public void editBlokkade(AjaxRequestTarget target, ColonBlokkade blokkade)
	{
		IModel<ColonBlokkade> blokkadeModel = ModelUtil.cModel(blokkade);

		try
		{
			final Date origRecEndDateTime = getOrigRecEndDateTime(blokkade);

			dialog.setContent(new EditBlokkadePanel(IDialog.CONTENT_ID, blokkadeModel)
			{

				private static final long serialVersionUID = 1L;

				@Override
				protected void onClose(AjaxRequestTarget target, CalendarRefresher<ColonBlokkade> refresher)
				{
					if (refresher.isVerwijderd())
					{
						updateCalendar(target);
						info(getString("blokkade.verwijderd"));
					}
					else
					{
						updateCalendar(target);
						Kamer selectedKamer = getModelObject().getLocation();
						Map<ICalendarResource, List<? extends IAppointment>> refreshMap = new HashMap<>();
						if (selectedKamer == null)
						{
							for (Kamer kamer : getActieveKamers())
							{
								refreshMap.putAll(getRefreshMap(kamer, start, end));
							}
						}
						else
						{
							refreshMap = getRefreshMap(selectedKamer, start, end);
						}
						target.appendJavaScript(calendar.refresh(refreshMap).getStatement());
						info(getString("blokkade.opgeslagen"));
					}
					dialog.close(target);
					target.add(roosterAantallenPerJaarPanel);
				}

				@Override
				protected boolean onBeforeOpslaan(ColonBlokkade blokkade)
				{
					boolean isValideBlokkade = super.onBeforeOpslaan(blokkade);
					if (isValideBlokkade)
					{
						try
						{
							roosterService.magBlokkadeOpslaanVerwijderen(blokkade, getRecurrenceOption(), getRecurrenceEditEnd(), origRecEndDateTime, true,
								getKamers(blokkade));
						}
						catch (OpslaanVerwijderenTijdBlokException e)
						{
							isValideBlokkade = false;
							error(createErrorMessage(e));
						}
					}
					return isValideBlokkade;
				}

				@Override
				protected boolean onBeforeDelete(ColonBlokkade objectToDelete)
				{
					boolean isValideBlokkade = super.onBeforeDelete(objectToDelete);
					if (isValideBlokkade)
					{
						try
						{
							hibernateService.reload(objectToDelete);
							roosterService.magBlokkadeOpslaanVerwijderen(objectToDelete, getRecurrenceOption(), getRecurrenceEditEnd(), origRecEndDateTime, false,
								getKamers(objectToDelete));
						}
						catch (OpslaanVerwijderenTijdBlokException e)
						{
							isValideBlokkade = false;
							error(createErrorMessage(e));
						}
					}
					return isValideBlokkade;
				}

				private List<Kamer> getKamers(ColonBlokkade blokkade)
				{
					List<Kamer> kamers = new ArrayList<>();
					if (Boolean.TRUE.equals(ModelUtil.nullSafeGet(alleKamers)))
					{
						kamers = getActieveKamers();
					}
					else
					{
						kamers = Arrays.asList(blokkade.getLocation());
					}
					return kamers;
				}
			});

			dialog.open(target);
		}
		catch (ObjectNotFoundException e)
		{
			LOG.error("Kan blokkade edit popup niet openen", e);
		}

	}
}
