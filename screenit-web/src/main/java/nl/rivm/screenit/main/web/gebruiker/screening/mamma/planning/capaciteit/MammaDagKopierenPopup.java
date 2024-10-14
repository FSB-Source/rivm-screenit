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
import java.time.LocalTime;
import java.util.Date;

import nl.rivm.screenit.main.service.mamma.MammaScreeningsEenheidService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.ConfirmingIndicatingAjaxSubmitLink;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.mamma.MammaBaseConceptPlanningsApplicatie;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import nl.topicuszorg.wicket.input.timefield.TimeField;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.CheckBox;
import org.apache.wicket.markup.html.form.ChoiceRenderer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.validation.validator.DateValidator;
import org.wicketstuff.wiquery.ui.datepicker.DateOption;
import org.wicketstuff.wiquery.ui.datepicker.DatePicker;

public abstract class MammaDagKopierenPopup extends GenericPanel<MammaScreeningsEenheid>
{

	@SpringBean
	private MammaBaseConceptPlanningsApplicatie baseConceptPlanningsApplicatie;

	@SpringBean
	private MammaScreeningsEenheidService screeningsEenheidService;

	@SpringBean
	private ICurrentDateSupplier currentDateSupplier;

	private IModel<MammaScreeningsEenheid> doelScreeningEenheid;

	public MammaDagKopierenPopup(String id, IModel<MammaScreeningsEenheid> screeningEenheid, Date initieleWaarde)
	{
		super(id, screeningEenheid);
		setDoelScreeningEenheid(screeningEenheid.getObject());

		var confirmPopup = new BootstrapDialog("confirmPopup");
		add(confirmPopup);

		var screeningsEenheidNaam = new Label("naam");
		add(screeningsEenheidNaam);

		var form = maakForm();
		add(form);

		var bronDagPicker = maakDatumField("bronDag", initieleWaarde, null, null);
		form.add(bronDagPicker);

		var bronTijdVanPicker = maakTijdPicker("bronTijdVan", LocalTime.of(0, 0));
		form.add(bronTijdVanPicker);

		var bronTijdTotPicker = maakTijdPicker("bronTijdTot", LocalTime.of(23, 59));
		form.add(bronTijdTotPicker);

		var doelSePicker = maakDoelScreeningsEenheidPicker();
		form.add(doelSePicker);

		var doelDagPicker = maakDatumField("doelDag", null, DateUtil.toUtilDate(currentDateSupplier.getLocalDate().plusDays(1)),
			baseConceptPlanningsApplicatie.getPlannenTotEnMetDatum());
		form.add(doelDagPicker);

		var doelOpenen = new CheckBox("doelOpenen", Model.of(true));
		form.add(doelOpenen);

		var opslaanBtn = new ConfirmingIndicatingAjaxSubmitLink<>("opslaan", form, confirmPopup, "kopieren.popup")
		{

			@Override
			protected boolean skipConfirmation()
			{
				return !isEindTijdNaStarttijd(bronTijdVanPicker, bronTijdTotPicker);
			}

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				if (!isEindTijdNaStarttijd(bronTijdVanPicker, bronTijdTotPicker))
				{
					MammaDagKopierenPopup.this.error(String.format(getString("eindtijd.na.starttijd")));
					return;
				}

				var bronSe = MammaDagKopierenPopup.this.getModelObject();
				var bronDag = DateUtil.toLocalDate(bronDagPicker.getModelObject());
				var bronVanTijd = DateUtil.toLocalTime(bronTijdVanPicker.getModelObject());
				var bronTotTijd = DateUtil.toLocalTime(bronTijdTotPicker.getModelObject());

				var doelSe = getDoelScreeningEenheid();
				var doelDag = DateUtil.toLocalDate(doelDagPicker.getModelObject());
				var beginWeekDoelDag = doelDag.with(DayOfWeek.MONDAY);

				var openDoel = doelOpenen.getModelObject();

				baseConceptPlanningsApplicatie.kopieerDag(bronSe, doelSe, bronDag, bronVanTijd, bronTotTijd, doelDag,
					ScreenitSession.get().getLoggedInInstellingGebruiker());

				var melding = String.format(getString("kopieren.afgerond"), bronDag.format(DateUtil.LOCAL_DATE_FORMAT), doelDag.format(DateUtil.LOCAL_DATE_FORMAT));

				if (openDoel && (!bronSe.equals(doelSe) || !beginWeekDoelDag.isEqual(DateUtil.toLocalDate(initieleWaarde))))
				{
					var page = new MammaSECapaciteitEditPage(doelSe, DateUtil.toUtilDate(beginWeekDoelDag));
					page.successMelding(melding);
					setResponsePage(page);
				}
				else
				{
					((MammaSECapaciteitEditPage) getPage()).successMelding(melding);
					onOpgeslagen(target);
				}
			}
		};

		form.add(opslaanBtn);
	}

	private boolean isEindTijdNaStarttijd(TimeField startTijd, TimeField eindTijd)
	{
		var bronVanTijd = DateUtil.toLocalTime(startTijd.getModelObject());
		var bronTotTijd = DateUtil.toLocalTime(eindTijd.getModelObject());

		return bronTotTijd.isAfter(bronVanTijd);
	}

	private Form<IModel<MammaScreeningsEenheid>> maakForm()
	{
		var form = new Form<>("kopierenForm", this::getModel);
		form.setOutputMarkupId(true);
		return form;
	}

	private ScreenitDropdown<MammaScreeningsEenheid> maakDoelScreeningsEenheidPicker()
	{
		var screeningsEenheidDropDown = new ScreenitDropdown<>("doelScreeningsEenheid", new PropertyModel<>(this, "doelScreeningEenheid"),
			ModelUtil.listRModel(screeningsEenheidService.getActieveScreeningsEenhedenVoorScreeningOrganisatie(ScreenitSession.get().getScreeningOrganisatie())),
			new ChoiceRenderer<>("naam"));
		screeningsEenheidDropDown.setRequired(true);
		return screeningsEenheidDropDown;
	}

	private DatePicker<Date> maakDatumField(String id, Date initieleWaarde, Date minimaleDatum, Date maximaleDatum)
	{
		var dateField = ComponentHelper.newDatePicker(id, Model.of(initieleWaarde));
		dateField.setOutputMarkupId(true);
		dateField.setRequired(true);
		if (minimaleDatum != null)
		{
			dateField.add(DateValidator.minimum(minimaleDatum));
			dateField.setMinDate(new DateOption(minimaleDatum));
		}
		if (maximaleDatum != null)
		{
			dateField.add(DateValidator.maximum(maximaleDatum));
			dateField.setMaxDate(new DateOption(maximaleDatum));
		}
		return dateField;
	}

	private TimeField maakTijdPicker(String id, LocalTime initialValue)
	{
		var timeField = new TimeField(id, true);
		timeField.setModel(Model.of(DateUtil.toUtilDate(initialValue.atDate(currentDateSupplier.getLocalDate()))));
		timeField.setOutputMarkupId(true);
		timeField.setEnabled(true);
		timeField.setRequired(true);
		timeField.setHours(initialValue.getHour());
		timeField.setMinutes(initialValue.getMinute());
		return timeField;
	}

	protected abstract void onOpgeslagen(AjaxRequestTarget target);

	public MammaScreeningsEenheid getDoelScreeningEenheid()
	{
		return ModelUtil.nullSafeGet(doelScreeningEenheid);
	}

	public void setDoelScreeningEenheid(MammaScreeningsEenheid doelScreeningEenheid)
	{
		this.doelScreeningEenheid = ModelUtil.sModel(doelScreeningEenheid);
	}

}
