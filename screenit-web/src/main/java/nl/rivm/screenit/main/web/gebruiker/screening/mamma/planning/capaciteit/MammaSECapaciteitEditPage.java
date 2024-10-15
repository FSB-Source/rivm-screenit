package nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.capaciteit;

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
import java.time.temporal.TemporalAdjusters;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import nl.rivm.screenit.dto.mamma.planning.PlanningMeldingenDto.PlanningMeldingDto;
import nl.rivm.screenit.dto.mamma.planning.PlanningScreeningsEenheidMetaDataDto;
import nl.rivm.screenit.main.service.mamma.MammaScreeningsEenheidService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.component.modal.IDialog;
import nl.rivm.screenit.main.web.filter.SecurityHeadersFilter;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerBasePage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.MammaPlanningBasePage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.dashboard.MammaPlanningDashboardPage;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.enums.MammaMeldingNiveau;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.mamma.MammaBaseConceptPlanningsApplicatie;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormComponentUpdatingBehavior;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.ajax.markup.html.form.AjaxCheckBox;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.ChoiceRenderer;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.request.http.WebResponse;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.validation.validator.DateValidator;
import org.wicketstuff.shiro.ShiroConstraint;
import org.wicketstuff.wiquery.ui.datepicker.DateOption;
import org.wicketstuff.wiquery.ui.datepicker.DatePicker;

@SecurityConstraint(
	actie = Actie.INZIEN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = { Recht.GEBRUIKER_SCREENING_MAMMA_PLANNING },
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.MAMMA })
public class MammaSECapaciteitEditPage extends MammaPlanningBasePage
{
	@SpringBean
	private MammaScreeningsEenheidService screeningsEenheidService;

	@SpringBean
	private MammaBaseConceptPlanningsApplicatie baseConceptPlanningsApplicatie;

	@SpringBean
	private ICurrentDateSupplier dateSupplier;

	private final IModel<MammaScreeningsEenheid> screeningsEenheidModel;

	private final IModel<List<MammaScreeningsEenheid>> screeningsEenhedenModel;

	private final IModel<Boolean> isGrootOverzicht;

	private final IModel<Date> datumModel;

	private BootstrapDialog dialog;

	private MammaCapaciteitOverviewPanel calenderPanel;

	private AjaxLink<Void> weekHerhalenLink;

	private AjaxLink<Void> dagKopierenLink;

	private final Label herhalingsWeek;

	private final List<PlanningMeldingDto> meldingen = new ArrayList<>();

	private final DatePicker<Date> datumField;

	@Override
	protected void setHeaders(WebResponse response)
	{
		super.setHeaders(response);
		SecurityHeadersFilter.allowUnsafeInlineSecurityPolicy(response); 
	}

	public MammaSECapaciteitEditPage(MammaScreeningsEenheid screeningsEenheidInit, Date startWeek)
	{
		ScreeningOrganisatie sessionSO = ScreenitSession.get().getScreeningOrganisatie();

		this.screeningsEenheidModel = ModelUtil.cModel(screeningsEenheidInit);
		this.screeningsEenhedenModel = ModelUtil.listRModel(screeningsEenheidService.getActieveScreeningsEenhedenVoorScreeningOrganisatie(sessionSO));

		addNavigation();

		ScreenitDropdown<MammaScreeningsEenheid> screeningsEenheid = new ScreenitDropdown<>("screeningsEenheid", this.screeningsEenheidModel, screeningsEenhedenModel,
			new ChoiceRenderer<>("naam"));
		add(screeningsEenheid);
		screeningsEenheid.add(new AjaxFormComponentUpdatingBehavior("change")
		{
			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				Date huidigeDatum = calenderPanel.getHuidigeStartVanWeek();
				refreshKalender(target, huidigeDatum);
			}
		});

		if (screeningsEenheidModel.getObject().getVrijgegevenTotEnMet() != null && startWeek == null)
		{
			startWeek = screeningsEenheidModel.getObject().getVrijgegevenTotEnMet();
		}

		herhalingsWeek = new Label("herhalingsWeek", "");
		herhalingsWeek.setOutputMarkupId(true);
		add(herhalingsWeek);

		isGrootOverzicht = new Model<>(true);

		AjaxCheckBox grootOverzicht = new AjaxCheckBox("grootOverzicht", isGrootOverzicht)
		{

			@Override
			protected void onUpdate(AjaxRequestTarget ajaxRequestTarget)
			{
				if (calenderPanel.getHuidigeStartVanWeek() != null)
				{
					Date huidigeDatum = calenderPanel.getHuidigeStartVanWeek();
					refreshKalender(ajaxRequestTarget, huidigeDatum);
				}
			}
		};

		startWeek = startWeek != null ? startWeek : dateSupplier.getDate();
		datumModel = Model.of(startWeek);
		datumField = ComponentHelper.newDatePicker("datum", datumModel, true);
		datumField.setOutputMarkupId(true);
		add(datumField);
		Date plannenTotEnMetDatum = baseConceptPlanningsApplicatie.getPlannenTotEnMetDatum();
		datumField.add(DateValidator.maximum(plannenTotEnMetDatum));
		datumField.setMaxDate(new DateOption(plannenTotEnMetDatum));
		datumField.add(new AjaxFormComponentUpdatingBehavior("change")
		{
			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				refreshKalender(target, datumModel.getObject());
			}
		});
		add(new IndicatingAjaxLink<Void>("prevWeek")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				Date datum = datumModel.getObject();
				if (datum != null)
				{
					Date previousWeek = DateUtil.toUtilDate(DateUtil.toLocalDate(datum).minusWeeks(1));
					datumModel.setObject(previousWeek);
					refreshKalender(target, previousWeek);
				}
			}
		});
		add(new IndicatingAjaxLink<Void>("nextWeek")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				Date datum = datumModel.getObject();
				if (datum != null)
				{
					Date nextWeek = DateUtil.toUtilDate(DateUtil.toLocalDate(datum).plusWeeks(1));
					if (datumField.getMaxDate().getDateParam().compareTo(nextWeek) >= 0)
					{
						datumModel.setObject(nextWeek);
						refreshKalender(target, nextWeek);
					}
				}
			}
		});

		grootOverzicht.setOutputMarkupId(true);
		add(grootOverzicht);

		refreshKalender(null, startWeek);
	}

	private void addNavigation()
	{
		dialog = new BootstrapDialog("dialog");
		add(dialog);

		weekHerhalenLink = new AjaxLink<>("weekHerhalen")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				dialog.openWith(target,
					new MammaWeekHerhalenPopup(IDialog.CONTENT_ID, calenderPanel.getModel(), DateUtil.toLocalDate(calenderPanel.getHuidigeStartVanWeek()))
					{

						@Override
						void herhalingOpgeslagen(AjaxRequestTarget target)
						{
							Date huidigeDatum = calenderPanel.getHuidigeStartVanWeek();
							refreshKalender(target, huidigeDatum);
							dialog.close(target);
						}

					});
			}
		};
		weekHerhalenLink.setVisible(magAanpassen);
		weekHerhalenLink.setOutputMarkupPlaceholderTag(true);
		weekHerhalenLink.setOutputMarkupId(true);
		add(weekHerhalenLink);

		dagKopierenLink = new AjaxLink<>("dagKopieren")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				dialog.openWith(target,
					new MammaDagKopierenPopup(IDialog.CONTENT_ID, calenderPanel.getModel(), calenderPanel.getHuidigeStartVanWeek())
					{

						@Override
						protected void onOpgeslagen(AjaxRequestTarget target)
						{
							Date huidigeDatum = calenderPanel.getHuidigeStartVanWeek();
							refreshKalender(target, huidigeDatum);
							dialog.close(target);
						}

					});
			}
		};
		dagKopierenLink.setVisible(magAanpassen);
		dagKopierenLink.setOutputMarkupPlaceholderTag(true);
		dagKopierenLink.setOutputMarkupId(true);
		add(dagKopierenLink);

		add(new AjaxLink<Void>("terug")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				setResponsePage(MammaPlanningDashboardPage.class);
			}
		});
	}

	private void refreshCalendarNavInfo(AjaxRequestTarget target)
	{
		PlanningScreeningsEenheidMetaDataDto metaData = baseConceptPlanningsApplicatie.getScreeningsEenheidMetaData(screeningsEenheidModel.getObject());
		if (metaData.herhalingsWeek != null)
		{
			herhalingsWeek.setDefaultModelObject(DateUtil.getWeekNr(metaData.herhalingsWeek) + "-" + metaData.herhalingsWeek.getYear());
		}
		else
		{
			herhalingsWeek.setDefaultModelObject("(Nog) niet gezet.");
		}

		Date huidigeStartVanWeek = calenderPanel.getHuidigeStartVanWeek();
		datumModel.setObject(huidigeStartVanWeek);

		weekHerhalenLink.setEnabled(!DateUtil.toLocalDate(huidigeStartVanWeek).isBefore(dateSupplier.getLocalDate().with(TemporalAdjusters.next(DayOfWeek.MONDAY))));

		target.add(herhalingsWeek);
		target.add(datumField);
		target.add(weekHerhalenLink);
	}

	private MammaCapaciteitOverviewPanel refreshKalender(AjaxRequestTarget target, Date date)
	{
		calenderPanel = new MammaCapaciteitOverviewPanel("calendar", screeningsEenheidModel, DateUtil.toLocalDate(date), isGrootOverzicht.getObject())
		{

			@Override
			protected void onCalenderRendered(AjaxRequestTarget target)
			{
				super.onCalenderRendered(target);
				refreshCalendarNavInfo(target);
				showMeldingen();
			}
		};

		addOrReplace(calenderPanel);
		if (target != null)
		{
			target.add(calenderPanel);
		}

		return calenderPanel;
	}

	public void successMelding(String melding)
	{
		addMelding(melding, MammaMeldingNiveau.INFO);
	}

	public void errorMelding(String melding)
	{
		addMelding(melding, MammaMeldingNiveau.PROBLEEM);
	}

	private void addMelding(String melding, MammaMeldingNiveau niveau)
	{
		PlanningMeldingDto meldingDto = new PlanningMeldingDto();
		meldingDto.tekst = melding;
		meldingDto.niveau = niveau;
		meldingen.add(meldingDto);
	}

	public void showMeldingen()
	{
		for (PlanningMeldingDto melding : meldingen)
		{
			switch (melding.niveau)
			{
			case INFO:
				info(melding.tekst);
				break;
			case PROBLEEM:
				error(melding.tekst);
				break;
			case WAARSCHUWING:
				warn(melding.tekst);
				break;
			default:
				break;

			}
		}
		meldingen.clear();
	}

	public boolean hasMeldingen()
	{
		return !meldingen.isEmpty();
	}

	@Override
	protected boolean bevatFormulieren()
	{
		return false;
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(screeningsEenheidModel);
		ModelUtil.nullSafeDetach(screeningsEenhedenModel);
	}

	@Override
	protected Class<? extends GebruikerBasePage> getActiveContextMenuClass()
	{
		return MammaPlanningDashboardPage.class;
	}

}
