package nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.dashboard;

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

import java.util.Date;
import java.util.List;
import java.util.Optional;

import nl.rivm.screenit.dto.mamma.planning.PlanningConceptMeldingenDto;
import nl.rivm.screenit.dto.mamma.planning.PlanningStandplaatsPeriodeDto;
import nl.rivm.screenit.main.service.mamma.MammaScreeningsEenheidService;
import nl.rivm.screenit.main.service.mamma.MammaStandplaatsPeriodeService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.enums.MammaMeldingNiveau;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.mamma.MammaBaseConceptPlanningsApplicatie;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.mamma.MammaPlanningUtil;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxButton;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.util.ListModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.validation.IValidatable;
import org.apache.wicket.validation.ValidationError;
import org.apache.wicket.validation.validator.DateValidator;
import org.joda.time.DateTime;
import org.wicketstuff.wiquery.ui.datepicker.DatePicker;

public abstract class MammaCapaciteitUitnodigenPanel extends GenericPanel<MammaScreeningsEenheid>
{

	@SpringBean
	private MammaScreeningsEenheidService screeningsEenheidService;

	@SpringBean
	private ICurrentDateSupplier dateSupplier;

	@SpringBean
	private MammaStandplaatsPeriodeService standplaatsPeriodeService;

	@SpringBean
	private MammaBaseConceptPlanningsApplicatie baseConceptPlanningsApplicatie;

	public MammaCapaciteitUitnodigenPanel(String id, IModel<MammaScreeningsEenheid> screeningsEenheidModel)
	{
		super(id, screeningsEenheidModel);
		ScreeningOrganisatie ingelogdNamensRegio = ScreenitSession.get().getScreeningOrganisatie();
		boolean magAanpassen = ingelogdNamensRegio != null && ScreenitSession.get().checkPermission(Recht.GEBRUIKER_SCREENING_MAMMA_PLANNING, Actie.AANPASSEN);

		MammaScreeningsEenheid screeningsEenheid = screeningsEenheidModel.getObject();
		IModel<List<PlanningStandplaatsPeriodeDto>> standplaatsPeriodes = new ListModel<>(
			standplaatsPeriodeService.getStandplaatsPeriodesSorted(screeningsEenheidModel.getObject()));

		ScreenitForm<MammaScreeningsEenheid> form = new ScreenitForm<>("form");
		add(form);

		form.add(new Label("naam"));
		form.add(new Label("uitgenodigdTotEnMet"));

		DatePicker<Date> uitnodigenTotEnMetKalender = ComponentHelper.newYearDatePicker("uitnodigenTotEnMet");
		uitnodigenTotEnMetKalender.setDisabled(!magAanpassen);
		form.add(uitnodigenTotEnMetKalender);
		if (screeningsEenheid.getUitgenodigdTotEnMet() != null)
		{
			uitnodigenTotEnMetKalender.add(DateValidator.minimum(screeningsEenheid.getUitgenodigdTotEnMet()));
		}
		DateTime vandaag = dateSupplier.getDateTime();
		uitnodigenTotEnMetKalender.add(new VrijgegevenTotEnMetUitnodigenTotEnMetDatumValidator(standplaatsPeriodes));
		uitnodigenTotEnMetKalender.add(DateValidator.maximum(vandaag.plusMonths(2).toDate()));

		DatePicker<Date> vrijgegevenTotEnMetKalender = ComponentHelper.newYearDatePicker("vrijgegevenTotEnMet");
		vrijgegevenTotEnMetKalender.setDisabled(!magAanpassen);
		form.add(vrijgegevenTotEnMetKalender);
		vrijgegevenTotEnMetKalender.add(new VrijgegevenTotEnMetUitnodigenTotEnMetDatumValidator(standplaatsPeriodes));
		vrijgegevenTotEnMetKalender.add(DateValidator.range(vandaag.minusDays(1).toDate(), vandaag.plusMonths(6).toDate()));

		IndicatingAjaxButton opslaanknop = new IndicatingAjaxButton("opslaan")
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				PlanningConceptMeldingenDto meldingen = baseConceptPlanningsApplicatie.saveConcept(ScreenitSession.get().getLoggedInInstellingGebruiker(), true);
				if (meldingen.seMeldingen.size() > 0)
				{
					error(getString("wijzigingDatums.conceptNietOpgeslagen"));
				}
				else
				{
					boolean isScreeningsEenheidGewijzigd = screeningsEenheidService.saveOrUpdateSE(MammaCapaciteitUitnodigenPanel.this.getModelObject(),
						ScreenitSession.get().getLoggedInInstellingGebruiker());

					close(target);

					if (!standplaatsPeriodes.getObject().isEmpty())
					{
						Date uitnodigenTotEnMet = MammaCapaciteitUitnodigenPanel.this.getModelObject().getUitnodigenTotEnMet();

						boolean toonAchtervangMelding = uitnodigenTotEnMet != null
							&& standplaatsPeriodes.getObject().stream().anyMatch(standplaatsPeriode -> !uitnodigenTotEnMet.before(DateUtil.toUtilDate(standplaatsPeriode.totEnMet))
								&& !standplaatsPeriode.gesplitst
								&& standplaatsPeriode.meldingenDto.meldingen.size() > 1 
								&& (standplaatsPeriode.meldingenDto.meldingen.get(0).niveau == MammaMeldingNiveau.PROBLEEM
									|| standplaatsPeriode.meldingenDto.meldingen.get(1).niveau == MammaMeldingNiveau.PROBLEEM));

						if (toonAchtervangMelding)
						{
							warn(getString("uitnodigen.tot.en.met.onvoldoende.capaciteit"));
						}
					}
					else if (isScreeningsEenheidGewijzigd)
					{
						success(getString("message.gegevensopgeslagen"));
					}
				}
			}

		};
		opslaanknop.setEnabled(magAanpassen);
		form.add(opslaanknop);

		form.add(new AjaxLink<Void>("close")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				close(target);
			}
		});
	}

	protected abstract void close(AjaxRequestTarget target);

	private class VrijgegevenTotEnMetUitnodigenTotEnMetDatumValidator extends DateValidator
	{
		private IModel<List<PlanningStandplaatsPeriodeDto>> standplaatsPeriodes;

		VrijgegevenTotEnMetUitnodigenTotEnMetDatumValidator(IModel<List<PlanningStandplaatsPeriodeDto>> standplaatsPeriodes)
		{
			this.standplaatsPeriodes = standplaatsPeriodes;
		}

		@Override
		public void validate(IValidatable<Date> iValidatable)
		{
			Date teValiderenDatum = iValidatable.getValue();
			ValidationError error = new ValidationError();

			Optional<PlanningStandplaatsPeriodeDto> eersteStandplaatsPeriodeMetPrognose = standplaatsPeriodes.getObject().stream()
				.filter(standplaatsPeriode -> standplaatsPeriode.prognose)
				.findFirst();

			if (eersteStandplaatsPeriodeMetPrognose.isPresent())
			{
				if (!MammaPlanningUtil.datumIsMeerDanVijfWerkdagenVoorDatum(DateUtil.toLocalDate(teValiderenDatum), eersteStandplaatsPeriodeMetPrognose.get().totEnMet))
				{
					error.addKey("VijfDagenVoorEinddatumValidator.datumBinnenVijfWerkdagenPrognose");
					iValidatable.error(error);
				}
			}
			else
			{
				if (!standplaatsPeriodes.getObject().isEmpty())
				{
					PlanningStandplaatsPeriodeDto laatsteStandplaatsPeriode = standplaatsPeriodes.getObject().get(standplaatsPeriodes.getObject().size() - 1);
					if (teValiderenDatum.after(DateUtil.toUtilDate(laatsteStandplaatsPeriode.totEnMet)))
					{
						error.addKey("VijfDagenVoorEinddatumValidator.datumNaLaatsteStandplaatsperiode");
						iValidatable.error(error);
					}
				}
				else
				{
					error.addKey("VijfDagenVoorEinddatumValidator.geenStandplaatsPeriodes");
					iValidatable.error(error);
				}
			}
		}
	}
}
