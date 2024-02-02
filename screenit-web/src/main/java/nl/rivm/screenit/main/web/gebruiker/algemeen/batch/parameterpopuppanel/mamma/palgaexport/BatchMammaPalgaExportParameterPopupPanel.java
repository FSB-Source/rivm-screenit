package nl.rivm.screenit.main.web.gebruiker.algemeen.batch.parameterpopuppanel.mamma.palgaexport;

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

import java.util.Date;
import java.util.Map;

import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.validator.MaximalePeriodeLengteValidator;
import nl.rivm.screenit.main.web.gebruiker.algemeen.batch.parameterpopuppanel.AbstractParameterPopupPanel;
import nl.rivm.screenit.model.batch.popupconfig.MammaPalgaExportConfig;
import nl.rivm.screenit.model.batch.popupconfig.MammaPalgaExportGewensteUitslag;
import nl.rivm.screenit.model.batch.popupconfig.MammaPalgaExportPeriodeType;
import nl.rivm.screenit.model.batch.popupconfig.MammaPalgaGrondslag;
import nl.rivm.screenit.model.enums.JobStartParameter;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.topicuszorg.wicket.input.validator.DependantDateValidator;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormChoiceComponentUpdatingBehavior;
import org.apache.wicket.markup.html.form.EnumChoiceRenderer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.Radio;
import org.apache.wicket.markup.html.form.RadioGroup;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.validation.validator.RangeValidator;
import org.wicketstuff.wiquery.ui.datepicker.DatePicker;

public class BatchMammaPalgaExportParameterPopupPanel extends AbstractParameterPopupPanel<MammaPalgaExportConfig>
{

	@SpringBean
	private ICurrentDateSupplier currentDateSupplier;

	public BatchMammaPalgaExportParameterPopupPanel(String id, Form<?> form)
	{
		super(id, new CompoundPropertyModel<>(new MammaPalgaExportConfig()));

		add(maakGrondslagkeuzeRadioChoice());
		add(maakSelectPeriodeTypeRadioChoice(form));
		addGewensteUitslagRadioChoice();
		add(new TextField<>("maxAantalPerFile", Integer.class).setRequired(true).add(new RangeValidator<>(1, 2000000)));
	}

	private RadioGroup<MammaPalgaExportConfig> maakGrondslagkeuzeRadioChoice()
	{
		var radioGroup = new RadioGroup<MammaPalgaExportConfig>("grondslag");
		radioGroup.setRequired(true);

		radioGroup.add(new Radio<>("kwaliteitsborging", new Model<>(MammaPalgaGrondslag.KWALITEITSBORGING)));
		radioGroup.add(new Radio<>("monitor", new Model<>(MammaPalgaGrondslag.LANDELIJKE_MONITOR)));

		var volgnummerField = maakVolgnummerKwaliteitsborgingVeld();
		radioGroup.add(volgnummerField);

		radioGroup.add(new AjaxFormChoiceComponentUpdatingBehavior()
		{
			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				updateGrondslagVelden(target, volgnummerField);
			}
		});

		return radioGroup;
	}

	private TextField<Integer> maakVolgnummerKwaliteitsborgingVeld()
	{
		TextField<Integer> field = ComponentHelper.newTextField("volgnummerKwaliteitsborging", Integer.class, true);
		field.setEnabled(false);
		field.add(new RangeValidator<>(1, 99));
		return field;
	}

	private void updateGrondslagVelden(AjaxRequestTarget target, TextField<Integer> volgnummerField)
	{
		if (getModelObject().getGrondslag() != null)
		{
			volgnummerField.setEnabled(MammaPalgaGrondslag.KWALITEITSBORGING.equals(getModelObject().getGrondslag()));
			target.add(volgnummerField);
		}
	}

	private RadioGroup<MammaPalgaExportConfig> maakSelectPeriodeTypeRadioChoice(Form<?> form)
	{
		RadioGroup<MammaPalgaExportConfig> periodeTypeRadio = new RadioGroup<>("periodeType");
		periodeTypeRadio.setRequired(true);

		periodeTypeRadio.add(new Radio<>("range", new Model<>(MammaPalgaExportPeriodeType.ONDERZOEKS_DATUM_PERIODE)));
		periodeTypeRadio.add(new Radio<>("xmaanden", new Model<>(MammaPalgaExportPeriodeType.ONDERZOEKS_DATUM_AANTAL_MAANDEN_TERUG)));

		DatePicker<Date> vanafOnderzoeksDatum = maakDatumveld("vanafOnderzoeksDatum");
		DatePicker<Date> totEnMetOnderzoeksDatum = maakDatumveld("totEnMetOnderzoeksDatum");
		form.add(new DependantDateValidator(vanafOnderzoeksDatum, totEnMetOnderzoeksDatum, DependantDateValidator.Operator.AFTER));
		form.add(new MaximalePeriodeLengteValidator(vanafOnderzoeksDatum, totEnMetOnderzoeksDatum, 50));
		TextField<Integer> onderzoekAantalMaandenTerug = maakAantalMaandenTerugVeld();
		periodeTypeRadio.add(vanafOnderzoeksDatum);
		periodeTypeRadio.add(totEnMetOnderzoeksDatum);
		periodeTypeRadio.add(onderzoekAantalMaandenTerug);

		periodeTypeRadio.add(new AjaxFormChoiceComponentUpdatingBehavior()
		{
			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				updatePeriodeTypeVelden(target, vanafOnderzoeksDatum, totEnMetOnderzoeksDatum, onderzoekAantalMaandenTerug);
			}
		});
		return periodeTypeRadio;
	}

	private DatePicker<Date> maakDatumveld(String id)
	{
		DatePicker<Date> datePicker = ComponentHelper.newDatePicker(id);
		datePicker.setOutputMarkupId(true);
		datePicker.setEnabled(false);
		datePicker.setRequired(true);
		datePicker.add(RangeValidator.maximum(currentDateSupplier.getDate()));
		return datePicker;
	}

	private TextField<Integer> maakAantalMaandenTerugVeld()
	{
		TextField<Integer> field = ComponentHelper.newTextField("onderzoekAantalMaandenTerug", Integer.class, true);
		field.setEnabled(false);
		field.add(new RangeValidator<>(0, 50));
		return field;
	}

	private void updatePeriodeTypeVelden(AjaxRequestTarget target, DatePicker<Date> vanafOnderzoeksDatum, DatePicker<Date> totEnMetOnderzoeksDatum,
		TextField<Integer> onderzoekAantalMaandenTerug)
	{
		if (getModelObject().getPeriodeType() != null)
		{
			if (MammaPalgaExportPeriodeType.ONDERZOEKS_DATUM_PERIODE.equals(getModelObject().getPeriodeType()))
			{
				vanafOnderzoeksDatum.setEnabled(true);
				totEnMetOnderzoeksDatum.setEnabled(true);
				onderzoekAantalMaandenTerug.setEnabled(false);
				target.add(vanafOnderzoeksDatum, totEnMetOnderzoeksDatum, onderzoekAantalMaandenTerug);
			}
			else
			{
				vanafOnderzoeksDatum.setEnabled(false);
				totEnMetOnderzoeksDatum.setEnabled(false);
				onderzoekAantalMaandenTerug.setEnabled(true);
				target.add(vanafOnderzoeksDatum, totEnMetOnderzoeksDatum, onderzoekAantalMaandenTerug);
			}
		}
	}

	private void addGewensteUitslagRadioChoice()
	{
		var radioChoice = ComponentHelper.addRadioChoice(this, "gewensteUitslag", MammaPalgaExportGewensteUitslag.class);
		radioChoice.setChoiceRenderer(new EnumChoiceRenderer<>(this)
		{
			@Override
			public Object getDisplayValue(MammaPalgaExportGewensteUitslag object)
			{
				return super.getDisplayValue(object) + " (" + object.getCodeInFilePrefix() + ")";
			}
		});
	}

	@Override
	public void vulJobParameters(Map<String, Object> jobParameters)
	{
		jobParameters.put(JobStartParameter.MAMMA_PALGA_EXPORT.name(), getSerializedDefaultModelObject());
	}
}
