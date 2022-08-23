package nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.capaciteit;

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

import java.time.DayOfWeek;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.Date;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.main.dao.mamma.MammaScreeningsEenheidDao;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ConfirmingIndicatingAjaxSubmitLink;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.component.form.WeekNumberDateField;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.service.mamma.MammaBaseConceptPlanningsApplicatie;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormComponentUpdatingBehavior;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.ChoiceRenderer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.validation.validator.DateValidator;
import org.wicketstuff.wiquery.ui.datepicker.DateOption;

public abstract class MammaWeekHerhalenPopup extends GenericPanel<MammaScreeningsEenheid>
{
	@SpringBean
	private MammaBaseConceptPlanningsApplicatie baseConceptPlanningsApplicatie;

	@SpringBean
	private MammaScreeningsEenheidDao screeningsEenheidDao;

	private IModel<MammaScreeningsEenheid> selectedScreeningEenheid;

	private final BootstrapDialog confirmPopup;

	private final Form<MammaScreeningsEenheid> herhalenForm;

	private final LocalDate herhalingsWeek;

	private WeekNumberDateField herhalenVanafDatumDatePicker;

	private WeekNumberDateField herhalenTotEnMetDatumDatePicker;

	public MammaWeekHerhalenPopup(String id, IModel<MammaScreeningsEenheid> screeningEenheid, LocalDate herhalingsWeek)
	{
		super(id, screeningEenheid);
		this.herhalingsWeek = herhalingsWeek;

		setSelectedScreeningEenheid(screeningEenheid.getObject());
		confirmPopup = new BootstrapDialog("confirmPopup");
		confirmPopup.setOutputMarkupPlaceholderTag(true);
		add(confirmPopup);

		Label screeningsEenheidNaam = new Label("screeningsEenheidNaam", screeningEenheid.getObject().getNaam());
		add(screeningsEenheidNaam);

		DateTimeFormatter endWeekPattern = DateTimeFormatter.ofPattern("dd MMM yyyy", Constants.LOCALE_NL);

		LocalDate eindHerhalingsWeek = herhalingsWeek.plusDays(6);
		String startPattern = "dd";
		if (!herhalingsWeek.getMonth().equals(eindHerhalingsWeek.getMonth()))
		{
			startPattern += " MMM";
		}
		if (herhalingsWeek.getYear() != eindHerhalingsWeek.getYear())
		{
			startPattern += " yyyy";
		}
		DateTimeFormatter startWeekPattern = DateTimeFormatter.ofPattern(startPattern, Constants.LOCALE_NL);

		String weekSpan = DateUtil.getWeekNr(herhalingsWeek) + ": " + startWeekPattern.format(herhalingsWeek) + " \u2014 " + endWeekPattern.format(eindHerhalingsWeek);
		Label weekSpanLabel = new Label("weekSpan", weekSpan);
		add(weekSpanLabel);

		herhalenForm = new Form<>("herhalenForm", screeningEenheid);
		herhalenForm.setOutputMarkupId(true);
		add(herhalenForm);

		ScreenitDropdown<MammaScreeningsEenheid> screeningsEenheidDropDown = new ScreenitDropdown<>("screeningsEenheid", new PropertyModel<>(this, "selectedScreeningEenheid"),
			ModelUtil.listRModel(screeningsEenheidDao.getActieveScreeningsEenhedenVoorScreeningOrganisatie(ScreenitSession.get().getScreeningOrganisatie())),
			new ChoiceRenderer<>("naam"));
		screeningsEenheidDropDown.setRequired(true);
		screeningsEenheidDropDown.add(new AjaxFormComponentUpdatingBehavior("change")
		{

			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				addOrUpdateVanTotEnMet(target);
			}
		});
		herhalenForm.add(screeningsEenheidDropDown);

		addOrUpdateVanTotEnMet(null);

		ConfirmingIndicatingAjaxSubmitLink<Void> opslaan = new ConfirmingIndicatingAjaxSubmitLink<Void>("opslaan", herhalenForm, confirmPopup, "overschrijven.popup")
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				LocalDate herhalenTotEnMetWeek = herhalenTotEnMetDatumDatePicker.getModelObject();
				LocalDate herhalenVanafWeek = herhalenVanafDatumDatePicker.getModelObject();

				if (herhalenTotEnMetWeek != null && herhalenVanafWeek != null && herhalenVanafWeek.isAfter(herhalenTotEnMetWeek))
				{
					error(getString("vanafNietNaTotEnMet"));
					return;
				}
				MammaScreeningsEenheid selectedSe = getSelectedScreeningEenheid();
				baseConceptPlanningsApplicatie.herhaalWeek(MammaWeekHerhalenPopup.this.getModelObject(), selectedSe, herhalingsWeek, herhalenVanafWeek, herhalenTotEnMetWeek,
					ScreenitSession.get().getLoggedInInstellingGebruiker());
				MammaScreeningsEenheid origSe = MammaWeekHerhalenPopup.this.getModelObject();
				if (!origSe.equals(selectedSe))
				{
					MammaSECapaciteitEditPage page = new MammaSECapaciteitEditPage(selectedSe, DateUtil.toUtilDate(herhalenVanafWeek));
					page.successMelding(getString("herhalen.afgerond"));
					setResponsePage(page);
				}
				else
				{
					((MammaSECapaciteitEditPage) getPage()).successMelding(getString("herhalen.afgerond"));
					herhalingOpgeslagen(target);
				}
			}

		};

		herhalenForm.add(opslaan);

	}

	private void addOrUpdateVanTotEnMet(AjaxRequestTarget target)
	{
		LocalDate eersteDagNieuweWeek = herhalingsWeek.plusDays(7);
		LocalDate vrijgegevenTotEnMet = DateUtil.toLocalDate(getSelectedScreeningEenheid().getVrijgegevenTotEnMet());
		if (vrijgegevenTotEnMet != null && vrijgegevenTotEnMet.isAfter(eersteDagNieuweWeek))
		{
			eersteDagNieuweWeek = vrijgegevenTotEnMet.plusWeeks(1).with(DayOfWeek.MONDAY);
		}
		Date uitersteHerhalenTotEnMet = DateUtil.toUtilDate(DateUtil.toLocalDate(baseConceptPlanningsApplicatie.getPlannenTotEnMetDatum()).minusWeeks(1));

		herhalenVanafDatumDatePicker = new WeekNumberDateField("herhalenVanafWeek", Model.of(eersteDagNieuweWeek), eersteDagNieuweWeek);
		herhalenVanafDatumDatePicker.setOutputMarkupId(true);
		herhalenVanafDatumDatePicker.setRequired(true);
		herhalenVanafDatumDatePicker.add(DateValidator.minimum(eersteDagNieuweWeek));
		herhalenVanafDatumDatePicker.add(DateValidator.maximum(DateUtil.toLocalDate(uitersteHerhalenTotEnMet)));
		herhalenVanafDatumDatePicker.setMaxDate(new DateOption(uitersteHerhalenTotEnMet));
		herhalenVanafDatumDatePicker.add(new AjaxFormComponentUpdatingBehavior("change")
		{

			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				LocalDate weekVanaf = herhalenVanafDatumDatePicker.getModelObject();
				if (weekVanaf != null)
				{
					herhalenTotEnMetDatumDatePicker.setMinDate(new DateOption(DateUtil.toUtilDate(weekVanaf)));
					target.add(herhalenTotEnMetDatumDatePicker);
				}
			}
		});
		herhalenTotEnMetDatumDatePicker = new WeekNumberDateField("herhalenTotEnMetWeek", Model.of((LocalDate) null), eersteDagNieuweWeek);
		herhalenTotEnMetDatumDatePicker.setOutputMarkupId(true);
		herhalenTotEnMetDatumDatePicker.add(DateValidator.maximum(DateUtil.toLocalDate(uitersteHerhalenTotEnMet)));
		herhalenTotEnMetDatumDatePicker.setMaxDate(new DateOption(uitersteHerhalenTotEnMet));
		herhalenForm.addOrReplace(herhalenTotEnMetDatumDatePicker);
		herhalenForm.addOrReplace(herhalenVanafDatumDatePicker);
		if (target != null)
		{
			target.add(herhalenForm);
		}
	}

	public MammaScreeningsEenheid getSelectedScreeningEenheid()
	{
		return ModelUtil.nullSafeGet(selectedScreeningEenheid);
	}

	public void setSelectedScreeningEenheid(MammaScreeningsEenheid selectedScreeningEenheid)
	{
		this.selectedScreeningEenheid = ModelUtil.sModel(selectedScreeningEenheid);
	}

	abstract void herhalingOpgeslagen(AjaxRequestTarget target);

	@Override
	public void detachModels()
	{
		super.detachModels();
		ModelUtil.nullSafeDetach(selectedScreeningEenheid);
	}

}
