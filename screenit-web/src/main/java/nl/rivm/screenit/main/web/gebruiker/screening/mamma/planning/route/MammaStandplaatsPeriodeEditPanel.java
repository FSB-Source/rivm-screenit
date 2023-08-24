package nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.route;

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

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.EnumSet;
import java.util.List;
import java.util.stream.Collectors;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.dto.mamma.planning.PlanningStandplaatsPeriodeDto;
import nl.rivm.screenit.main.service.mamma.MammaAfspraakService;
import nl.rivm.screenit.main.service.mamma.MammaRouteService;
import nl.rivm.screenit.main.service.mamma.MammaStandplaatsPeriodeService;
import nl.rivm.screenit.main.util.StandplaatsPeriodeUtil;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.ConfirmingIndicatingAjaxSubmitLink;
import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitListMultipleChoice;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.MammaStandplaats;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode;
import nl.rivm.screenit.model.mamma.enums.MammaCapaciteitBlokType;
import nl.rivm.screenit.model.verwerkingverslag.mamma.MammaStandplaatsRondeRapportageStatus;
import nl.rivm.screenit.model.verwerkingverslag.mamma.MammaStandplaatsRondeUitnodigenRapportage;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.InstellingService;
import nl.rivm.screenit.service.mamma.MammaBaseCapaciteitsBlokService;
import nl.rivm.screenit.service.mamma.MammaBaseStandplaatsService;
import nl.rivm.screenit.service.mamma.impl.MammaCapaciteit;
import nl.rivm.screenit.util.BigDecimalUtil;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.mamma.MammaPlanningUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import nl.topicuszorg.wicket.input.AttributeRemover;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.OnChangeAjaxBehavior;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.behavior.AttributeAppender;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxButton;
import org.apache.wicket.markup.head.IHeaderResponse;
import org.apache.wicket.markup.head.JavaScriptHeaderItem;
import org.apache.wicket.markup.head.OnDomReadyHeaderItem;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.ChoiceRenderer;
import org.apache.wicket.markup.html.form.HiddenField;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.markup.html.panel.EmptyPanel;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.validation.validator.RangeValidator;
import org.wicketstuff.wiquery.core.javascript.JsStatement;
import org.wicketstuff.wiquery.ui.datepicker.DatePicker;

import com.google.common.collect.Range;

public abstract class MammaStandplaatsPeriodeEditPanel extends GenericPanel<PlanningStandplaatsPeriodeDto>
{
	@SpringBean
	private MammaStandplaatsPeriodeService standplaatsPeriodeService;

	@SpringBean
	private MammaAfspraakService afspraakService;

	@SpringBean
	private MammaRouteService routeService;

	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private MammaBaseStandplaatsService baseStandplaatsService;

	@SpringBean(name = "applicationUrl")
	private String applicationUrl;

	@SpringBean
	private MammaBaseCapaciteitsBlokService baseCapaciteitsBlokService;

	@SpringBean
	private InstellingService instellingService;

	@SpringBean
	private ICurrentDateSupplier dateSupplier;

	private LocalDate initieleVanaf;

	private LocalDate initieleTotEnMet;

	private LocalDate vrijgegevenTotEnMet;

	private LocalDate uitnodigenTotEnMet;

	private IModel<Date> handmatigeDatum = new Model<>();

	private PlanningStandplaatsPeriodeDto volgendeStandplaatsPeriode;

	private ScreenitDropdown<Long> achtervangStandplaatsPeriodeDropdown;

	private ScreenitDropdown<Long> minderValideUitwijkStandplaatsDropdown;

	private WebMarkupContainer achtervangStandplaatsContainer;

	private WebMarkupContainer minderValideUitwijkStandplaatsContainer;

	@Override
	public void renderHead(IHeaderResponse response)
	{
		super.renderHead(response);
		response.render(JavaScriptHeaderItem.forUrl("assets/js/voorspellingsgrafiek/util.js"));
		response.render(JavaScriptHeaderItem.forUrl("assets/js/voorspellingsgrafiek/data.js"));
		response.render(JavaScriptHeaderItem.forUrl("assets/js/voorspellingsgrafiek/rij.js"));
		response.render(JavaScriptHeaderItem.forUrl("assets/js/voorspellingsgrafiek/matrix.js"));
		response.render(JavaScriptHeaderItem.forUrl("assets/js/voorspellingsgrafiek/voorspellingsgrafiek.js"));

		JsStatement jsStatement = new JsStatement();
		jsStatement.append("deVoorspellingsgrafiekenDeelnamekans = new VoorspellingsgrafiekenDeelnamekans();");
		response.render(OnDomReadyHeaderItem.forScript(jsStatement.render()));
	}

	protected MammaStandplaatsPeriodeEditPanel(String id, IModel<PlanningStandplaatsPeriodeDto> model, PlanningStandplaatsPeriodeDto volgendeStandplaatsPeriode,
		IModel<MammaScreeningsEenheid> screeningsEenheidModel, boolean magBeginDatumWijzigen, boolean magEindDatumWijzigen)
	{
		super(id, model);

		final PlanningStandplaatsPeriodeDto standplaatsPeriodeDto = model.getObject();

		ScreeningOrganisatie ingelogdNamensRegio = ScreenitSession.get().getScreeningOrganisatie();
		boolean magAanpassen = ingelogdNamensRegio != null && ScreenitSession.get().checkPermission(Recht.GEBRUIKER_SCREENING_MAMMA_PLANNING, Actie.AANPASSEN);

		List<Long> actieveStandplaatsen = getStandplaatsen(ingelogdNamensRegio);

		Long initieleMinderValideUitwijkStandplaatsId = getModelObject().minderValideUitwijkStandplaatsId;

		ScreenitForm<MammaStandplaatsPeriode> form = new ScreenitForm<>("form");
		add(form);

		final WebMarkupContainer accordionContainer = new WebMarkupContainer("accordionContainer");

		WebMarkupContainer standVanZakenContainer = new WebMarkupContainer("standVanZakenContainer");
		WebMarkupContainer standVanZakenLink = new WebMarkupContainer("standVanZakenLink");
		addLinkBehaviorAccordion(accordionContainer, standVanZakenContainer, standVanZakenLink);

		WebMarkupContainer configurationContainer = new WebMarkupContainer("configurationContainer");
		WebMarkupContainer configurationLink = new WebMarkupContainer("configurationLink");
		addLinkBehaviorAccordion(accordionContainer, configurationContainer, configurationLink);

		WebMarkupContainer afspraakDrempelContainer = new WebMarkupContainer("afspraakDrempelContainer");
		WebMarkupContainer afspraakDrempelLink = new WebMarkupContainer("afspraakDrempelLink");
		addLinkBehaviorAccordion(accordionContainer, afspraakDrempelContainer, afspraakDrempelLink);

		MammaStandplaats standplaats = hibernateService.get(MammaStandplaats.class, standplaatsPeriodeDto.standplaatsId);

		this.volgendeStandplaatsPeriode = volgendeStandplaatsPeriode;

		String naam = StandplaatsPeriodeUtil.getStandplaatsPeriodeNaam(getModelObject(), standplaats);

		form.add(new Label("standplaatsRonde.standplaats.naam", naam));

		afspraakDrempelContainer.add(new Label("standplaatsRonde.standplaats.regio.afspraakDrempelBk", standplaats.getRegio().getAfspraakDrempelBk()));

		String totEnMetPrognose = "";

		if (Boolean.TRUE.equals(getModelObject().prognose))
		{
			totEnMetPrognose += "prognose: " + DateTimeFormatter.ofPattern("dd-MM-yyyy").format(getModelObject().totEnMet);
			configurationContainer.add(new Label("totEnMetPrognose", totEnMetPrognose));
		}
		else
		{
			configurationContainer.add(new EmptyPanel("totEnMetPrognose"));
		}

		TextField<Integer> afspraakDrempel = new TextField<>("afspraakDrempel");
		afspraakDrempelContainer.add(afspraakDrempel);
		afspraakDrempel.setEnabled(magAanpassen);
		afspraakDrempel.setRequired(false);
		afspraakDrempel.setType(Integer.class);
		afspraakDrempel.add(RangeValidator.range(0, 100));

		DatePicker<Date> einddatum;
		if (Boolean.TRUE.equals(standplaatsPeriodeDto.prognose))
		{
			einddatum = ComponentHelper.newYearDatePicker("totEnMet", handmatigeDatum);
		}
		else
		{
			einddatum = ComponentHelper.newYearDatePicker("totEnMet");
		}
		einddatum.setOutputMarkupId(true);
		einddatum.setDisabled(!magEindDatumWijzigen || !magAanpassen);

		form.setEnabled(true);

		configurationContainer.add(einddatum);

		DatePicker<Date> vanafDatum = ComponentHelper.newYearDatePicker("vanaf");
		if (magBeginDatumWijzigen)
		{
			configurationContainer.add(new EmptyPanel("vanafLabel"));
		}
		else
		{
			vanafDatum.setVisible(false);
			configurationContainer.add(new Label("vanafLabel", getModelObject().vanaf));
		}
		vanafDatum.setOutputMarkupId(true);
		vanafDatum.setDisabled(!magAanpassen);
		configurationContainer.add(vanafDatum);

		achtervangStandplaatsPeriodeDropdown = new ScreenitDropdown<>("achtervangStandplaatsId",
			actieveStandplaatsen, new ChoiceRenderer<>()
		{
			@Override
			public Object getDisplayValue(Long object)
			{
				return hibernateService.load(MammaStandplaats.class, object).getNaam();
			}
		});
		achtervangStandplaatsPeriodeDropdown.add(new OnChangeAjaxBehavior()
		{
			@Override
			protected void onUpdate(AjaxRequestTarget ajaxRequestTarget)
			{

			}
		});

		achtervangStandplaatsPeriodeDropdown.setOutputMarkupId(true);
		achtervangStandplaatsPeriodeDropdown.setEnabled(magAanpassen);

		achtervangStandplaatsContainer = new WebMarkupContainer("achtervangStandplaatsContainer");
		achtervangStandplaatsContainer.setOutputMarkupId(true);
		achtervangStandplaatsContainer.add(achtervangStandplaatsPeriodeDropdown);
		configurationContainer.add(achtervangStandplaatsContainer);

		einddatum.add(new OnChangeAjaxBehavior()
		{
			@Override
			protected void onUpdate(AjaxRequestTarget ajaxRequestTarget)
			{
				achtervangStandplaatsPeriodeDropdown.setEnabled(einddatum.getModelObject() != null);
				if (einddatum.getModelObject() == null)
				{
					getModelObject().achtervangStandplaatsId = null;
				}
				ajaxRequestTarget.add(achtervangStandplaatsContainer);
			}
		});

		if (getModelObject().prognose)
		{
			achtervangStandplaatsPeriodeDropdown.setEnabled(false);
		}

		minderValideUitwijkStandplaatsDropdown = new ScreenitDropdown<>("minderValideUitwijkStandplaatsId",
			actieveStandplaatsen, new ChoiceRenderer<>()
		{
			@Override
			public Object getDisplayValue(Long object)
			{
				return hibernateService.load(MammaStandplaats.class, object).getNaam();
			}
		});

		minderValideUitwijkStandplaatsDropdown.add(new OnChangeAjaxBehavior()
		{
			@Override
			protected void onUpdate(AjaxRequestTarget ajaxRequestTarget)
			{

			}
		});
		minderValideUitwijkStandplaatsDropdown.setNullValid(true);
		minderValideUitwijkStandplaatsDropdown.setOutputMarkupId(true);
		minderValideUitwijkStandplaatsDropdown.setEnabled(magAanpassen);

		minderValideUitwijkStandplaatsContainer = new WebMarkupContainer("minderValideUitwijkStandplaatsContainer");
		minderValideUitwijkStandplaatsContainer.setOutputMarkupId(true);
		minderValideUitwijkStandplaatsContainer.add(minderValideUitwijkStandplaatsDropdown);
		configurationContainer.add(minderValideUitwijkStandplaatsContainer);

		DatePicker<Date> minderValideUitnodigenVanaf = ComponentHelper.newYearDatePicker("minderValideUitnodigenVanaf");
		minderValideUitnodigenVanaf.setDisabled(!magAanpassen);
		configurationContainer.add(minderValideUitnodigenVanaf);

		addAfspraakcapaciteitBeschikbaarVoor(configurationContainer, standplaats);

		initieleVanaf = getModelObject().vanaf;
		initieleTotEnMet = getModelObject().totEnMet;

		MammaScreeningsEenheid screeningsEenheid = screeningsEenheidModel.getObject();
		vrijgegevenTotEnMet = DateUtil.toLocalDate(screeningsEenheid.getVrijgegevenTotEnMet());
		uitnodigenTotEnMet = DateUtil.toLocalDate(screeningsEenheid.getUitnodigenTotEnMet());

		WebMarkupContainer geenCapaciteitOverzicht = new WebMarkupContainer("geenCapaciteitOverzicht");
		standVanZakenContainer.add(geenCapaciteitOverzicht);
		WebMarkupContainer capaciteitOverzichtContainer = new WebMarkupContainer("capaciteitOverzichtContainer");
		standVanZakenContainer.add(capaciteitOverzichtContainer);

		Long standplaatsPeriodeId = standplaatsPeriodeDto.id;
		MammaStandplaatsPeriode standplaatsPeriode = null;
		if (standplaatsPeriodeId != null)
		{
			standplaatsPeriode = hibernateService.load(MammaStandplaatsPeriode.class, standplaatsPeriodeId);
		}
		if (standplaatsPeriode != null && vrijgegevenTotEnMet != null && !vrijgegevenTotEnMet.isBefore(standplaatsPeriodeDto.vanaf))
		{
			geenCapaciteitOverzicht.setVisible(false);

			LocalDate vandaag = dateSupplier.getLocalDate();

			LocalDate vanaf = Collections.max(Arrays.asList(vandaag, standplaatsPeriodeDto.vanaf));
			LocalDate totEnMet = Collections.min(Arrays.asList(vrijgegevenTotEnMet, standplaatsPeriodeDto.totEnMet));

			MammaCapaciteit capaciteit = baseCapaciteitsBlokService.getCapaciteit(baseCapaciteitsBlokService
				.getNietGeblokkeerdeCapaciteitsBlokDtos(standplaatsPeriode, DateUtil.toUtilDate(vanaf.atStartOfDay()),
					DateUtil.toUtilDate(totEnMet.atTime(Constants.BK_EINDTIJD_DAG)),
					EnumSet.of(MammaCapaciteitBlokType.REGULIER, MammaCapaciteitBlokType.TEHUIS)));

			BigDecimal beschikbareCapaciteitRegulier = capaciteit.getCapaciteit(MammaCapaciteitBlokType.REGULIER).beschikbareCapaciteit;
			BigDecimal beschikbareCapaciteitTehuis = capaciteit.getCapaciteit(MammaCapaciteitBlokType.TEHUIS).beschikbareCapaciteit;
			BigDecimal vrijeCapaciteitRegulier = capaciteit.getCapaciteit(MammaCapaciteitBlokType.REGULIER).vrijeCapaciteit;
			BigDecimal vrijeCapaciteitTehuis = capaciteit.getCapaciteit(MammaCapaciteitBlokType.TEHUIS).vrijeCapaciteit;

			BigDecimal factorDubbeleTijd = standplaats.getRegio().getFactorDubbeleTijdBk();

			capaciteitOverzichtContainer.add(new Label("beschikbaarRegulier",
				beschikbareCapaciteitRegulier.setScale(0, RoundingMode.HALF_UP).toString()));
			capaciteitOverzichtContainer.add(new Label("beschikbaarTehuis",
				beschikbareCapaciteitTehuis.divide(factorDubbeleTijd, 0, RoundingMode.HALF_UP)
					+ " (" + BigDecimalUtil.decimalToString(beschikbareCapaciteitTehuis, 1) + " regulier)"));
			capaciteitOverzichtContainer.add(new Label("vrijRegulier",
				vrijeCapaciteitRegulier.setScale(1, RoundingMode.HALF_UP).toString()));
			capaciteitOverzichtContainer.add(new Label("vrijTehuis",
				vrijeCapaciteitTehuis.divide(factorDubbeleTijd, 1, RoundingMode.HALF_UP)
					+ " (" + vrijeCapaciteitTehuis.setScale(1, RoundingMode.HALF_UP) + " regulier)"));
		}
		else
		{
			capaciteitOverzichtContainer.setVisible(false);
		}

		WebMarkupContainer nietUitgenodigd = new WebMarkupContainer("nietUitgenodigd");
		standVanZakenContainer.add(nietUitgenodigd);
		WebMarkupContainer uitnodigenOverzichtContainer = new WebMarkupContainer("uitnodigenOverzichtContainer");
		standVanZakenContainer.add(uitnodigenOverzichtContainer);

		MammaStandplaatsRondeUitnodigenRapportage standplaatsRondeUitnodigenRapportage = null;
		if (standplaatsPeriode != null)
		{
			standplaatsRondeUitnodigenRapportage = standplaatsPeriodeService.getStandplaatsRondeUitnodigenRapportage(standplaatsPeriode.getStandplaatsRonde());
		}

		if (standplaatsRondeUitnodigenRapportage != null && standplaatsRondeUitnodigenRapportage.getStatus() != MammaStandplaatsRondeRapportageStatus.ALLEEN_UITSTEL_UITNODIGINGEN)
		{
			nietUitgenodigd.setVisible(false);

			uitnodigenOverzichtContainer.setDefaultModel(ModelUtil.csModel(standplaatsRondeUitnodigenRapportage));
			uitnodigenOverzichtContainer.add(new Label("totaalTotaal"));
			uitnodigenOverzichtContainer.add(new Label("totaalVervolgRonde"));
			uitnodigenOverzichtContainer.add(new Label("totaalEersteRonde"));
			uitnodigenOverzichtContainer.add(new Label("totaalDubbeleTijd"));
			uitnodigenOverzichtContainer.add(new Label("totaalMinderValide"));
			uitnodigenOverzichtContainer.add(new Label("totaalTehuis"));
			uitnodigenOverzichtContainer.add(new Label("totaalSuspect"));
			uitnodigenOverzichtContainer.add(new Label("uitTeNodigenTotaal"));
			uitnodigenOverzichtContainer.add(new Label("uitTeNodigenVervolgRonde"));
			uitnodigenOverzichtContainer.add(new Label("uitTeNodigenEersteRonde"));
			uitnodigenOverzichtContainer.add(new Label("uitTeNodigenDubbeleTijd"));
			uitnodigenOverzichtContainer.add(new Label("uitTeNodigenMinderValide"));
			uitnodigenOverzichtContainer.add(new Label("uitTeNodigenTehuis"));
			uitnodigenOverzichtContainer.add(new Label("uitTeNodigenSuspect"));
		}
		else
		{
			uitnodigenOverzichtContainer.setVisible(false);
		}

		IndicatingAjaxButton splitsenKnop = new IndicatingAjaxButton("splitsen")
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				if (!magSplitsen())
				{
					standVanZakenContainer.add(new AttributeRemover("class", Model.of(" in"), " "));
					configurationContainer.add(new AttributeRemover("class", Model.of(" in"), " "));
					afspraakDrempelContainer.add(new AttributeRemover("class", Model.of(" in"), " "));
					configurationContainer.add(new AttributeAppender("class", " in"));
					target.add(configurationContainer);

					return;
				}

				standplaatsPeriodeService.splitsStandplaatsPeriode(MammaStandplaatsPeriodeEditPanel.this.getModelObject(), ScreenitSession.get().getLoggedInInstellingGebruiker());

				info(getString("message.gegevens.onthouden"));
				standplaatsPeriodeGewijzigd(target);
			}
		};
		splitsenKnop.setEnabled(magAanpassen);
		form.add(splitsenKnop);

		BootstrapDialog dialog = new BootstrapDialog("dialog");
		add(dialog);

		ConfirmingIndicatingAjaxSubmitLink<Void> opslaanKnop = new ConfirmingIndicatingAjaxSubmitLink<>("opslaan", dialog,
			"Standplaatsperiode.opslaan.minder.valide.uitwijk.waarschuwing")
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				boolean magOnthouden = magOnthouden();
				if (magOnthouden)
				{
					boolean isStandplaatsGewijzigd = opslaan();
					if (isStandplaatsGewijzigd)
					{
						info(getString("message.gegevens.onthouden"));
						standplaatsPeriodeGewijzigd(target);
					}
				}
				else
				{
					standVanZakenContainer.add(new AttributeRemover("class", Model.of(" in"), " "));
					configurationContainer.add(new AttributeRemover("class", Model.of(" in"), " "));
					afspraakDrempelContainer.add(new AttributeRemover("class", Model.of(" in"), " "));
					configurationContainer.add(new AttributeAppender("class", " in"));
					target.add(configurationContainer);
				}
			}

			@Override
			protected boolean skipConfirmation()
			{
				return standplaatsPeriodeDto.minderValideUitwijkStandplaatsId == null
					|| standplaatsPeriodeDto.minderValideUitwijkStandplaatsId.equals(initieleMinderValideUitwijkStandplaatsId);
			}

		};
		opslaanKnop.setEnabled(magAanpassen);
		form.add(opslaanKnop);
		form.setDefaultButton(opslaanKnop);

		form.add(new AjaxLink<Void>("close")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				close(target);
			}
		});

		afspraakDrempelContainer.add(new HiddenField<>("applicationUrl", Model.of(applicationUrl)));
		afspraakDrempelContainer.add(new HiddenField<>("subUrl", Model.of("/api/getAfspraakDrempelOverzichtStandplaats?standplaatsId=" + standplaats.getId() + "&")));

		accordionContainer.add(afspraakDrempelLink);
		accordionContainer.add(configurationLink);
		accordionContainer.add(standVanZakenLink);
		accordionContainer.add(afspraakDrempelContainer);
		accordionContainer.add(configurationContainer);
		accordionContainer.add(standVanZakenContainer);

		form.add(accordionContainer);
	}

	private List<Long> getStandplaatsen(ScreeningOrganisatie ingelogdNamensRegio)
	{
		List<Long> standplaatsenIds = new ArrayList<>();
		if (ingelogdNamensRegio != null)
		{
			List<MammaStandplaats> standplaatsen = routeService.getStandplaatsenMetRoute(ingelogdNamensRegio);
			for (Long id : getModelObject().afspraakcapaciteitBeschikbaarVoorIds)
			{
				standplaatsen.addAll(routeService.getStandplaatsenMetRoute(hibernateService.load(ScreeningOrganisatie.class, id)));
			}
			standplaatsenIds = standplaatsen.stream()
				.filter(standplaats -> !standplaats.getId().equals(getModelObject().standplaatsId)).sorted(Comparator.comparing(MammaStandplaats::getNaam))
				.map(MammaStandplaats::getId)
				.collect(Collectors.toList());
		}
		return standplaatsenIds;
	}

	private void addAfspraakcapaciteitBeschikbaarVoor(WebMarkupContainer configurationContainer, MammaStandplaats standplaats)
	{
		List<Long> alleScreeningorganisatieIds = instellingService.getAllActiefScreeningOrganisaties().stream().map(ScreeningOrganisatie::getId).collect(Collectors.toList());
		alleScreeningorganisatieIds.remove(standplaats.getRegio().getId());
		configurationContainer.add(new ScreenitListMultipleChoice<>("afspraakcapaciteitBeschikbaarVoorIds", alleScreeningorganisatieIds, new ChoiceRenderer<>()
		{
			@Override
			public Object getDisplayValue(Long object)
			{
				return hibernateService.load(ScreeningOrganisatie.class, object).getNaam();
			}
		}).add(new OnChangeAjaxBehavior()
		{
			@Override
			protected void onUpdate(AjaxRequestTarget ajaxRequestTarget)
			{
				List<Long> standplaatsenIds = getStandplaatsen(ScreenitSession.get().getScreeningOrganisatie());
				if (!standplaatsenIds.contains(getModelObject().achtervangStandplaatsId))
				{
					getModelObject().achtervangStandplaatsId = null;
				}
				if (!standplaatsenIds.contains(getModelObject().minderValideUitwijkStandplaatsId))
				{
					getModelObject().minderValideUitwijkStandplaatsId = null;
				}
				achtervangStandplaatsPeriodeDropdown.setChoices(standplaatsenIds);
				minderValideUitwijkStandplaatsDropdown.setChoices(standplaatsenIds);

				ajaxRequestTarget.add(achtervangStandplaatsContainer);
				ajaxRequestTarget.add(minderValideUitwijkStandplaatsContainer);
			}
		}));
	}

	private boolean magOnthouden()
	{
		PlanningStandplaatsPeriodeDto standplaatsPeriodeDto = getModelObject();

		if (handmatigeDatum.getObject() != null)
		{
			standplaatsPeriodeDto.totEnMet = DateUtil.toLocalDate(handmatigeDatum.getObject());
			standplaatsPeriodeDto.prognose = false;
		}
		else if (standplaatsPeriodeDto.totEnMet == null
			&& !Boolean.TRUE.equals(standplaatsPeriodeDto.prognose))
		{
			Date datumEersteAfspraakVolgendePeriode = afspraakService.getDatumEersteGeplandeAfspraak(volgendeStandplaatsPeriode.id);
			if (initieleTotEnMet != null && volgendeStandplaatsPeriode != null && datumEersteAfspraakVolgendePeriode != null)
			{
				error(getString("Standplaatsperiode.einddatum.leeg.afspraken.in.volgende"));
				return false;
			}

			if (initieleTotEnMet != null
				&& (vrijgegevenTotEnMet != null
				&& !MammaPlanningUtil.datumIsMeerDanVijfWerkdagenVoorDatum(vrijgegevenTotEnMet, initieleTotEnMet)
				|| uitnodigenTotEnMet != null
				&& !MammaPlanningUtil.datumIsMeerDanVijfWerkdagenVoorDatum(uitnodigenTotEnMet, initieleTotEnMet)))
			{
				standplaatsPeriodeDto.totEnMet = initieleTotEnMet;
				error(
					getString("Standplaatsperiode.einddatum.verwijderen.binnenVijfDagen"));
				return false;
			}

			if (standplaatsPeriodeDto.gesplitst)
			{
				error("Einddatum mag niet leeg zijn wanneer de periode is gesplitst.");
				return false;
			}

			standplaatsPeriodeDto.prognose = true;
		}
		if (standplaatsPeriodeDto.vanaf == null)
		{
			error(getString("Standplaatsperiode.lege.begindatum"));
			return false;
		}
		if (!standplaatsPeriodeDto.prognose && standplaatsPeriodeDto.totEnMet != null)
		{
			if (standplaatsPeriodeDto.totEnMet.isBefore(standplaatsPeriodeDto.vanaf))
			{
				error(getString("Standplaatsperiode.einddatum.veranderen.eindVoorBegin"));
				return false;
			}
			if (volgendeStandplaatsPeriode == null)
			{
				if (vrijgegevenTotEnMet != null
					&& standplaatsPeriodeDto.totEnMet.isBefore(vrijgegevenTotEnMet))
				{
					error(getString("Standplaatsperiode.einddatum.voor.vrijgegeven.tot.en.met"));
					return false;
				}

				if (uitnodigenTotEnMet != null
					&& standplaatsPeriodeDto.totEnMet.isBefore(uitnodigenTotEnMet))
				{
					error(getString("Standplaatsperiode.einddatum.voor.uitnodigen.tot.en.met"));
					return false;
				}
			}
			Date datumLaatsteAfspraak = afspraakService.getDatumLaatsteGeplandeAfspraak(standplaatsPeriodeDto.id);
			if (datumLaatsteAfspraak != null && DateUtil.toLocalDate(datumLaatsteAfspraak).isAfter(standplaatsPeriodeDto.totEnMet))
			{
				boolean isLopendeStandplaatsPeriode = Range.closedOpen(standplaatsPeriodeDto.vanaf, standplaatsPeriodeDto.totEnMet)
					.contains(dateSupplier.getLocalDate());
				if (!isLopendeStandplaatsPeriode)
				{
					String datumLaatsteAfspraakText = DateUtil.formatShortDate(datumLaatsteAfspraak);
					error(String.format(getString("Standplaatsperiode.einddatum.veranderen.overschrijdtAfspraak.na"), datumLaatsteAfspraakText));
					return false;
				}
				else if (standplaatsPeriodeDto.totEnMet.isAfter(dateSupplier.getLocalDate()))
				{
					info(getString("Standplaatsperiode.einddatum.veranderen.afspraken.worden.verzet"));
				}
			}

			if (volgendeStandplaatsPeriode != null)
			{
				if (!volgendeStandplaatsPeriode.prognose && !standplaatsPeriodeDto.totEnMet.isBefore(volgendeStandplaatsPeriode.totEnMet))
				{
					error(getString("Standplaatsperiode.einddatum.voor.volgende.einddatum"));
					return false;
				}

				Date datumEersteAfspraakVolgendePeriode = afspraakService.getDatumEersteGeplandeAfspraak(volgendeStandplaatsPeriode.id);
				if (datumEersteAfspraakVolgendePeriode != null && !standplaatsPeriodeDto.totEnMet.isBefore(DateUtil.toLocalDate(datumEersteAfspraakVolgendePeriode)))
				{
					String datumEersteAfspraakVolgendePeriodeText = DateUtil.formatShortDate(datumEersteAfspraakVolgendePeriode);
					error(String.format(getString("Standplaatsperiode.einddatum.veranderen.overschrijdtAfspraak.voor"), datumEersteAfspraakVolgendePeriodeText));
					return false;
				}
			}
		}
		else if (volgendeStandplaatsPeriode != null && !volgendeStandplaatsPeriode.prognose && standplaatsPeriodeDto.totEnMet == null)
		{
			error(getString("Standplaatsperiode.einddatum.niet.wanneer.volgende.periode.ingevoerd"));
			return false;
		}

		if (!standplaatsPeriodeDto.prognose && standplaatsPeriodeDto.totEnMet != null || standplaatsPeriodeDto.prognose && handmatigeDatum.getObject() != null)
		{
			if (standplaatsPeriodeDto.achtervangStandplaatsId == null)
			{
				error(getString("Standplaatsperiode.achtervang.verplicht"));
				return false;
			}
		}

		return true;

	}

	private boolean magSplitsen()
	{
		PlanningStandplaatsPeriodeDto standplaatsPeriodeDto = getModelObject();

		LocalDate totEnMet = standplaatsPeriodeDto.totEnMet;
		boolean prognose = standplaatsPeriodeDto.prognose;

		if (totEnMet == null || prognose && handmatigeDatum.getObject() == null)
		{
			error(getString("Standplaatsperiode.lege.einddatum.gesplitst"));
			return false;
		}

		if (!initieleVanaf.equals(standplaatsPeriodeDto.vanaf)
			|| !initieleTotEnMet.equals(totEnMet) || prognose)
		{
			error(getString("Standplaatsperiode.onthoud.wijzigingen"));
			return false;
		}

		if (standplaatsPeriodeDto.gesplitst)
		{
			error(getString("Standplaatsperiode.is.al.gesplitst"));
			return false;
		}
		return true;
	}

	private boolean opslaan()
	{
		return standplaatsPeriodeService.saveOrUpdateStandplaatsPeriode(getModelObject(),
			ScreenitSession.get().getLoggedInInstellingGebruiker());
	}

	private void addLinkBehaviorAccordion(WebMarkupContainer accordionContainer, WebMarkupContainer teOpenenContainer, WebMarkupContainer link)
	{

		link.add(new AttributeAppender("data-parent", "#" + accordionContainer.getMarkupId()));

		link.add(new AttributeAppender("href", "#" + teOpenenContainer.getMarkupId()));
	}

	protected abstract void close(AjaxRequestTarget target);

	protected abstract void standplaatsPeriodeGewijzigd(AjaxRequestTarget target);

	@Override
	protected void onDetach()
	{
		super.onDetach();
		handmatigeDatum.detach();
	}
}
