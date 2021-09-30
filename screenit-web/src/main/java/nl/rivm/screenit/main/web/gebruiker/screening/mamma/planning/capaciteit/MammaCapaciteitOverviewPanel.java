package nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.capaciteit;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import nl.rivm.screenit.dto.mamma.planning.PlanningCapaciteitBlokDto;
import nl.rivm.screenit.dto.mamma.planning.PlanningMeldingenDto;
import nl.rivm.screenit.dto.mamma.planning.PlanningStandplaatsPeriodeDto;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.fullcalendar.CalendarResponse;
import nl.rivm.screenit.main.web.component.fullcalendar.FullCalendar;
import nl.rivm.screenit.main.web.component.fullcalendar.callback.AbstractShiftedEventParam;
import nl.rivm.screenit.main.web.component.fullcalendar.callback.ClickedEvent;
import nl.rivm.screenit.main.web.component.fullcalendar.callback.DroppedEvent;
import nl.rivm.screenit.main.web.component.fullcalendar.callback.ResizedEvent;
import nl.rivm.screenit.main.web.component.fullcalendar.callback.SelectedRange;
import nl.rivm.screenit.main.web.component.fullcalendar.callback.View;
import nl.rivm.screenit.main.web.component.fullcalendar.config.Config;
import nl.rivm.screenit.main.web.component.fullcalendar.config.DateShortcut;
import nl.rivm.screenit.main.web.component.fullcalendar.selector.EventSourceSelector;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.component.modal.IDialog;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.capaciteit.sources.ScreenITEventSourceFactory;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.mamma.MammaBaseCapaciteitsBlokService;
import nl.rivm.screenit.service.mamma.MammaBaseConceptPlanningsApplicatie;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.mamma.MammaPlanningUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.commons.lang3.StringUtils;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.behavior.AttributeAppender;
import org.apache.wicket.markup.head.CssHeaderItem;
import org.apache.wicket.markup.head.IHeaderResponse;
import org.apache.wicket.markup.head.JavaScriptHeaderItem;
import org.apache.wicket.markup.head.OnDomReadyHeaderItem;
import org.apache.wicket.markup.head.PriorityHeaderItem;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.markup.html.panel.Fragment;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.markup.repeater.RepeatingView;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.joda.time.DateTime;
import org.joda.time.LocalTime;

public class MammaCapaciteitOverviewPanel extends GenericPanel<MammaScreeningsEenheid>
{

	private static final long serialVersionUID = 1L;

	private BootstrapDialog dialog;

	private FullCalendar calendar;

	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private ICurrentDateSupplier dateSupplier;

	@SpringBean
	private MammaBaseCapaciteitsBlokService baseCapaciteitsBlokService;

	@SpringBean
	private MammaBaseConceptPlanningsApplicatie baseConceptPlanningsApplicatie;

	private Date origStartTijd;

	private DateTime huidigeStartVanWeek;

	private DateTime huidigeEindVanWeek;

	public static final LocalTime MINIMALE_TIJD = new LocalTime(7, 30);

	public static final LocalTime MAXIMALE_TIJD = new LocalTime(21, 0);

	private ScreenITEventSourceFactory sourceFactory;

	private RepeatingView tooltips;

	private List<String> addedTooltips = new ArrayList<>();

	public MammaCapaciteitOverviewPanel(String id, IModel<MammaScreeningsEenheid> model, LocalDate date, boolean isGrootOverzicht)
	{
		super(id, model);

		dialog = new BootstrapDialog("dialog");
		add(dialog);
		setOutputMarkupId(true);

		Config config = new Config();
		config.setFirstDay(1);
		config.setSelectable(true);
		config.setSelectHelper(false);
		config.setAspectRatio(2.6f);

		config.setEventTextColor("#000000");

		config.setDatum(date);
		config.setFirstHour(8);

		if (isGrootOverzicht)
		{
			config.setCellHeight(20);
		}
		else
		{
			config.setCellHeight(200);
		}

		sourceFactory = new ScreenITEventSourceFactory(model);
		config.add(sourceFactory.getWeekCapaciteitSource());
		config.add(sourceFactory.getAantalOnderzoekenPerDagSource());
		config.add(sourceFactory.getBlokkadesSource());
		config.add(sourceFactory.getStandplaatsSource(this));
		config.add(sourceFactory.getAantalOnderzoekenPerWeekSource());

		config.getHeader().setLeft("");
		config.getHeader().setCenter("title");
		config.getHeader().setRight("huidigeWeek, uitgenodigdTotEnMet,vrijgegevenTotEnMet, herhalingsWeek");

		var uitgenodigdTmButtonText = String.format("Uitgenodigd tot en met <br> %s", DateUtil.formatShortDate(getModelObject().getUitgenodigdTotEnMet()));
		var toUitgenodigdTmButton = new DateShortcut(uitgenodigdTmButtonText, DateUtil.toLocalDate(getModelObject().getUitgenodigdTotEnMet()));
		config.getDateShortcuts().put("uitgenodigdTotEnMet", toUitgenodigdTmButton);

		var vrijgevenTmButtonText = String.format("Vrijgegeven tot en met <br> %s", DateUtil.formatShortDate(getModelObject().getVrijgegevenTotEnMet()));
		var toVrijgegevenTmButton = new DateShortcut(vrijgevenTmButtonText, DateUtil.toLocalDate(getModelObject().getVrijgegevenTotEnMet()));
		config.getDateShortcuts().put("vrijgegevenTotEnMet", toVrijgegevenTmButton);

		DateShortcut toHerhalingsWeekButton = new DateShortcut("Herhalingsweek", baseConceptPlanningsApplicatie.getScreeningsEenheidMetaData(model.getObject()).herhalingsWeek);
		config.getDateShortcuts().put("herhalingsWeek", toHerhalingsWeekButton);

		DateShortcut huidigeWeek = new DateShortcut("Huidige week", dateSupplier.getLocalDate());
		config.getDateShortcuts().put("huidigeWeek", huidigeWeek);

		config.setAllDaySlot(true);
		config.setAllWeekSlot(true);

		config.setLoading("function(bool) { if (bool) $(\"#loading\").show(); else $(\"#loading\").hide(); }");

		config.setMinTime(MINIMALE_TIJD);
		config.setMaxTime(MAXIMALE_TIJD);

		config.setDefaultView("agendaWeek");

		boolean magAanpassen = ScreenitSession.get().checkPermission(Recht.GEBRUIKER_SCREENING_MAMMA_PLANNING, Actie.AANPASSEN)
			&& ScreenitSession.get().getScreeningOrganisatie() != null;
		config.setDisableDragging(!magAanpassen);
		config.setDisableResizing(true);

		calendar = new FullCalendar("calendar", config)
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onDateRangeSelected(SelectedRange range, CalendarResponse response)
			{
				if (!range.isAllDay() && magAanpassen)
				{
					IModel<PlanningCapaciteitBlokDto> blokModel = new CompoundPropertyModel<>(new PlanningCapaciteitBlokDto());
					PlanningCapaciteitBlokDto blok = blokModel.getObject();
					blok.screeningsEenheidId = getModelObject().getId();
					blok.vanaf = range.getStart().toDate();
					blok.tot = range.getEnd().toDate();
					blok.minderValideAfspraakMogelijk = true;
					openEventPopup(response, blokModel);
				}
			}

			@Override
			protected boolean onEventDropped(DroppedEvent event, CalendarResponse response)
			{
				response.refetchEvents();
				return wijzigCapaciteitsBlok(event);
			}

			@Override
			protected boolean onEventResized(ResizedEvent event, CalendarResponse response)
			{
				return wijzigCapaciteitsBlok(event);
			}

			@Override
			protected void onEventClicked(ClickedEvent event, CalendarResponse response)
			{
				if (magAanpassen && event.getEvent().isEditable())
				{
					IModel<PlanningCapaciteitBlokDto> blokModel = new CompoundPropertyModel<PlanningCapaciteitBlokDto>(sourceFactory.getBlok(event.getEvent().getConceptId()));
					openEventPopup(response, blokModel);
				}
			}

			@Override
			protected void onViewDisplayed(View view, CalendarResponse response)
			{
				huidigeStartVanWeek = view.getStart();
				huidigeEindVanWeek = view.getEnd();
				response.getTarget().add(tooltips.getParent());
				response.getTarget().appendJavaScript("initTooltip()");
				MammaCapaciteitOverviewPanel.this.onCalenderRendered(response.getTarget());
			}
		};

		calendar.setMarkupId("calendar");
		add(calendar);
		add(new EventSourceSelector("selector", calendar));

		final WebMarkupContainer tooltipContainer = new WebMarkupContainer("tooltipContainer");
		add(tooltipContainer);
		tooltipContainer.setOutputMarkupId(true);
		tooltips = new RepeatingView("tooltip");
		tooltipContainer.add(tooltips);
	}

	protected void onCalenderRendered(AjaxRequestTarget target)
	{

	}

	public DateTime getHuidigeStartVanWeek()
	{
		return huidigeStartVanWeek;
	}

	private void openEventPopup(CalendarResponse response, IModel<PlanningCapaciteitBlokDto> blokModel)
	{
		origStartTijd = blokModel.getObject().vanaf;
		dialog.openWith(response.getTarget(), new MammaCapaciteitBlokEditPopup(IDialog.CONTENT_ID, blokModel)
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onVerwijderen(AjaxRequestTarget target, IModel<PlanningCapaciteitBlokDto> model)
			{
				PlanningCapaciteitBlokDto abstractAppointmentToDelete = model.getObject();
				String melding = baseCapaciteitsBlokService.delete(abstractAppointmentToDelete, ScreenitSession.get().getLoggedInInstellingGebruiker());
				if (StringUtils.isBlank(melding))
				{
					blokSuccesvolChanged(response, target);
				}
				else
				{
					getThisPage().errorMelding(melding);
				}
			}

			@Override
			protected void onOpslaan(AjaxRequestTarget target, IModel<PlanningCapaciteitBlokDto> model)
			{
				PlanningCapaciteitBlokDto blokDto = model.getObject();
				onBeforeOpslaan(blokDto);
				if (!getThisPage().hasMeldingen())
				{
					String melding = baseCapaciteitsBlokService.saveOrUpdate(blokDto, ScreenitSession.get().getLoggedInInstellingGebruiker());
					if (StringUtils.isBlank(melding))
					{
						blokSuccesvolChanged(response, target);
					}
					else
					{
						getThisPage().errorMelding(melding);
						getThisPage().showMeldingen();
					}
				}
				else
				{
					getThisPage().showMeldingen();
				}
			}

			private void blokSuccesvolChanged(CalendarResponse response, AjaxRequestTarget target)
			{
				response.refetchEvents();
				resetTooltips();
				dialog.close(target);
				getThisPage().successMelding(getString("message.gegevens.onthouden"));
			}

		});
	}

	private MammaSECapaciteitEditPage getThisPage()
	{
		return (MammaSECapaciteitEditPage) getPage();
	}

	private void resetTooltips()
	{
		tooltips.removeAll();
		addedTooltips.clear();
	}

	private void onBeforeOpslaan(PlanningCapaciteitBlokDto blok)
	{
		Date start = blok.vanaf;
		Date tot = blok.tot;
		LocalTime startTime = new LocalTime(start);
		LocalTime totTime = new LocalTime(tot);

		if (startTime.getMinuteOfHour() % 5 != 0)
		{
			getThisPage().errorMelding(getString("CapaciteitKalender.starttijd.minuten.geen.factor.vijf"));
		}
		if (totTime.getMinuteOfHour() % 5 != 0)
		{
			getThisPage().errorMelding(getString("CapaciteitKalender.eindtijd.minuten.geen.factor.vijf"));
		}

		Long minimumTijdvak = MammaPlanningUtil.minimumTijdvak(blok.blokType.getFactorType().getFactor(ScreenitSession.get().getScreeningOrganisatie()));
		if (startTime.plusMinutes(minimumTijdvak.intValue()).isAfter(totTime))
		{
			getThisPage().errorMelding(String.format(getString("CapaciteitKalender.starttijd.na.eindtijd"), minimumTijdvak));
		}
		if (blok.conceptId != null && !origStartTijd.equals(start) && start.before(dateSupplier.getDateMidnight()))
		{
			getThisPage().errorMelding(getString("CapaciteitKalender.nieuwe.starttijd.in.verleden"));
		}
		else if (blok.conceptId == null && start.before(dateSupplier.getDateMidnight()))
		{
			getThisPage().errorMelding(getString("CapaciteitKalender.starttijd.in.verleden"));
		}
		if (totTime.getHourOfDay() == 0 || startTime.isBefore(MINIMALE_TIJD) || totTime.isAfter(MAXIMALE_TIJD))
		{
			getThisPage().errorMelding(getString("CapaciteitKalender.blok.buiten.minmax"));
		}
	}

	private boolean wijzigCapaciteitsBlok(AbstractShiftedEventParam event)
	{

		if (!(event instanceof DroppedEvent) || event instanceof DroppedEvent && !((DroppedEvent) event).isAllDay())
		{
			PlanningCapaciteitBlokDto blok = sourceFactory.getBlok(event.getEvent().getConceptId());
			origStartTijd = blok.vanaf;

			if (event.getNewEndTime().getDayOfYear() > event.getNewStartTime().getDayOfYear())
			{
				long millisDifference = event.getNewEndTime().getMillis() - event.getNewStartTime().getMillis();
				DateTime newEndDate = event.getNewEndTime().withTimeAtStartOfDay();
				blok.tot = newEndDate.toDate();
				blok.vanaf = newEndDate.minusMillis((int) millisDifference).toDate();
			}
			else
			{
				blok.vanaf = event.getNewStartTime().toDate();
				blok.tot = event.getNewEndTime().toDate();
			}
			onBeforeOpslaan(blok);

			if (!getThisPage().hasMeldingen())
			{
				String melding = baseCapaciteitsBlokService.saveOrUpdate(blok, ScreenitSession.get().getLoggedInInstellingGebruiker());

				if (StringUtils.isBlank(melding))
				{
					resetTooltips();
					getThisPage().successMelding(getString("message.gegevens.onthouden"));
					return true;
				}
				else
				{
					getThisPage().errorMelding(melding);
				}
			}
		}
		return false;
	}

	public void addMeldingTooltip(IModel<PlanningStandplaatsPeriodeDto> standplaatsPeriodeModel)
	{
		String tooltipId = "tooltip-m" + standplaatsPeriodeModel.getObject().conceptId;
		if (!addedTooltips.contains(tooltipId))
		{
			tooltips.add(new MeldingenTooltip(tooltips.newChildId(), standplaatsPeriodeModel));
			addedTooltips.add(tooltipId);
		}
	}

	@Override
	public void renderHead(IHeaderResponse response)
	{
		super.renderHead(response);
		response.render(new PriorityHeaderItem(CssHeaderItem.forUrl("assets/js/libs/qtip/jquery.qtip.min.css")));
		response.render(new PriorityHeaderItem(JavaScriptHeaderItem.forUrl("assets/js/libs/qtip/jquery.qtip.min.js")));
		response.render(new OnDomReadyHeaderItem("initTooltip()"));
	}

	private class MeldingenTooltip extends Fragment
	{

		private static final long serialVersionUID = 1L;

		public MeldingenTooltip(String id, IModel<PlanningStandplaatsPeriodeDto> model)
		{
			super(id, "meldingenFragment", MammaCapaciteitOverviewPanel.this);
			PlanningStandplaatsPeriodeDto standplaatsPeriodeDto = model.getObject();
			add(new AttributeAppender("class", Model.of(" tooltip-m" + standplaatsPeriodeDto.conceptId)));
			add(new ListView<PlanningMeldingenDto.PlanningMeldingDto>("meldingen", standplaatsPeriodeDto.meldingenDto.meldingen)
			{

				private static final long serialVersionUID = 1L;

				@Override
				protected void populateItem(ListItem<PlanningMeldingenDto.PlanningMeldingDto> item)
				{
					item.setDefaultModel(new CompoundPropertyModel<>(item.getModel()));
					Label niveau = new Label("niveau", "");
					niveau.add(new AttributeAppender("class", " " + item.getModelObject().niveau.getCssClass()));
					item.add(niveau);
					item.add(new Label("tekst"));
				}
			});
		}
	}

}
