package nl.rivm.screenit.main.web.gebruiker.screening.mamma.afspraken;

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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.EnumSet;
import java.util.List;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.dto.mamma.afspraken.MammaCapaciteitBlokDto;
import nl.rivm.screenit.main.service.mamma.MammaAfspraakService;
import nl.rivm.screenit.main.util.StandplaatsPeriodeUtil;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.ConfirmingIndicatingAjaxSubmitLink;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode;
import nl.rivm.screenit.model.mamma.enums.MammaCapaciteitBlokType;
import nl.rivm.screenit.model.mamma.enums.MammaFactorType;
import nl.rivm.screenit.model.mamma.enums.MammaVerzettenReden;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.mamma.MammaBaseAfspraakService;
import nl.rivm.screenit.service.mamma.MammaBaseCapaciteitsBlokService;
import nl.rivm.screenit.service.mamma.MammaBaseDossierService;
import nl.rivm.screenit.service.mamma.MammaBaseKansberekeningService;
import nl.rivm.screenit.service.mamma.impl.MammaCapaciteit;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import nl.topicuszorg.wicket.input.validator.DependantDateValidator;
import nl.topicuszorg.wicket.input.validator.DependantDateValidator.Operator;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormComponentUpdatingBehavior;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.ChoiceRenderer;
import org.apache.wicket.markup.html.form.EnumChoiceRenderer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.FormComponent;
import org.apache.wicket.markup.html.form.RadioChoice;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.validation.validator.DateValidator;
import org.wicketstuff.datetime.markup.html.basic.DateLabel;

public abstract class MammaBulkVerzettenPopup extends GenericPanel<MammaBulkVerzettenFilter>
{

	private static final long serialVersionUID = 1L;

	private BootstrapDialog confirmPopup;

	@SpringBean
	private MammaBaseAfspraakService baseAfspraakService;

	@SpringBean
	private MammaBaseCapaciteitsBlokService baseCapaciteitsBlokService;

	@SpringBean
	private MammaAfspraakService afspraakService;

	@SpringBean
	private ICurrentDateSupplier dateSupplier;

	@SpringBean
	private SimplePreferenceService preferenceService;

	@SpringBean
	private MammaBaseKansberekeningService baseKansberekeningService;

	@SpringBean
	private MammaBaseDossierService baseDossierService;

	private ScreenitDropdown<MammaStandplaatsPeriode> standplaatsPeriodeDropdown;

	private IModel<List<MammaAfspraak>> afsprakenModel;

	private IModel<MammaStandplaatsPeriode> standplaatsPeriodeModel;

	private MammaCapaciteit capaciteit;

	private BigDecimal benodigdeCapaciteitVoorAfspraken;

	private IModel<MammaStandplaatsPeriode> gekozenStandplaatsPeriodeModel;

	private FormComponent<Date> vanaf;

	private FormComponent<Date> tot;

	private WebMarkupContainer vrijeCapaciteitContainer;

	private DateValidator dateValidatorMinimum;

	private DateValidator dateValidatorMaximum;

	private Form<MammaBulkVerzettenFilter> form;

	private Date afspraakVerzettenZonderClientContactVanaf;

	public MammaBulkVerzettenPopup(String id, List<MammaAfspraak> afspraken, IModel<MammaScreeningsEenheid> screeningsEenheidModel, IModel<Date> datum,
		IModel<MammaStandplaatsPeriode> standplaatsPeriodeModel, List<MammaStandplaatsPeriode> standplaatsPeriodesVoorBulkVerzetten)
	{
		super(id);
		this.afsprakenModel = ModelUtil.listRModel(afspraken);
		this.standplaatsPeriodeModel = standplaatsPeriodeModel;

		MammaScreeningsEenheid screeningsEenheid = screeningsEenheidModel.getObject();
		MammaStandplaatsPeriode standplaatsPeriode = standplaatsPeriodeModel.getObject();

		gekozenStandplaatsPeriodeModel = standplaatsPeriodesVoorBulkVerzetten.contains(standplaatsPeriode) ? ModelUtil.cModel(standplaatsPeriode)
			: ModelUtil.cModel(standplaatsPeriodesVoorBulkVerzetten.get(0));

		afspraakVerzettenZonderClientContactVanaf = DateUtil.plusWerkdagen(dateSupplier.getDateMidnight(),
			preferenceService.getInteger(PreferenceKey.MAMMA_AFSPRAAK_VERZETTEN_ZONDER_CLIENT_CONTACT_VANAF_AANTAL_WERKDAGEN.name()));

		Date minDag = Collections.max(Arrays.asList(gekozenStandplaatsPeriodeModel.getObject().getVanaf(), afspraakVerzettenZonderClientContactVanaf));
		Date maxDag = Collections.min(
			Arrays.asList(gekozenStandplaatsPeriodeModel.getObject().getTotEnMet(), gekozenStandplaatsPeriodeModel.getObject().getScreeningsEenheid().getVrijgegevenTotEnMet()));

		benodigdeCapaciteitVoorAfspraken = afspraken.stream().map(afspraak ->
		{
			BigDecimal voorlopigeOpkomstkans = baseKansberekeningService
				.getVoorlopigeOpkomstkans(afspraak.getUitnodiging(), gekozenStandplaatsPeriodeModel.getObject(), MammaVerzettenReden.ONVOORZIENE_OMSTANDIGHEDEN);
			MammaFactorType factorType = baseDossierService.getFactorType(afspraak.getUitnodiging().getScreeningRonde().getDossier());
			BigDecimal factor = factorType.getFactor((ScreeningOrganisatie) screeningsEenheid.getBeoordelingsEenheid().getParent().getRegio());
			return voorlopigeOpkomstkans.multiply(factor);
		}).reduce(BigDecimal.ZERO, BigDecimal::add);

		MammaBulkVerzettenFilter filter = new MammaBulkVerzettenFilter(minDag, maxDag, gekozenStandplaatsPeriodeModel);

		setModel(new CompoundPropertyModel<>(filter));
		confirmPopup = new BootstrapDialog("confirmPopup");
		confirmPopup.setOutputMarkupPlaceholderTag(true);
		add(confirmPopup);

		String huidigeStandplaatsPeriodeNaam = StandplaatsPeriodeUtil.getStandplaatsPeriodeNaam(standplaatsPeriode);

		add(new Label("standplaatsPeriode.screeningsEenheid.naam"));
		add(new Label("standplaatsPeriode.standplaatsRonde.standplaats.naam", huidigeStandplaatsPeriodeNaam));
		add(DateLabel.forDatePattern("currentDay", datum, "dd-MM-yyyy"));
		add(new Label("aantalAfspraken", afspraken.size()));
		add(new Label("benodigdeCapaciteitTotaal", benodigdeCapaciteitVoorAfspraken.setScale(1, RoundingMode.HALF_UP).toString()));

		vrijeCapaciteitContainer = new WebMarkupContainer("vrijeCapaciteitContainer");
		vrijeCapaciteitContainer.setOutputMarkupId(true);
		addOrReplaceVrijeCapaciteit(filter, vrijeCapaciteitContainer);
		form = new Form<>("form", getModel());
		form.add(vrijeCapaciteitContainer);

		standplaatsPeriodeDropdown = new ScreenitDropdown<>("standplaatsPeriodeNaar", gekozenStandplaatsPeriodeModel, ModelUtil.listRModel(standplaatsPeriodesVoorBulkVerzetten),
			new ChoiceRenderer<>()
			{
				@Override
				public Object getDisplayValue(MammaStandplaatsPeriode standplaatsPeriode)
				{
					return StandplaatsPeriodeUtil.getStandplaatsPeriodeNaam(standplaatsPeriode) + ", " + standplaatsPeriode.getScreeningsEenheid().getNaam();
				}
			});

		standplaatsPeriodeDropdown.add(new AjaxFormComponentUpdatingBehavior("change")
		{
			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				tot.clearInput();
				vanaf.clearInput();
				standplaatsPeriodeSelected(target);
			}
		});

		form.add(standplaatsPeriodeDropdown);

		dateValidatorMinimum = DateValidator.minimum(minDag);
		vanaf = ComponentHelper.newDatePicker("vanaf").add(dateValidatorMinimum).setRequired(true);
		vanaf.add(new AjaxFormComponentUpdatingBehavior("change")
		{
			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				MammaBulkVerzettenFilter verzettenFilter = MammaBulkVerzettenPopup.this.getModelObject();
				addOrReplaceVrijeCapaciteit(verzettenFilter, vrijeCapaciteitContainer);
				target.add(vrijeCapaciteitContainer);

			}
		});
		form.add(vanaf);

		dateValidatorMaximum = DateValidator.maximum(maxDag);
		tot = ComponentHelper.newDatePicker("totEnMet").add(dateValidatorMaximum)
			.setRequired(true);
		tot.add(new AjaxFormComponentUpdatingBehavior("change")
		{
			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				MammaBulkVerzettenFilter verzettenFilter = MammaBulkVerzettenPopup.this.getModelObject();
				addOrReplaceVrijeCapaciteit(verzettenFilter, vrijeCapaciteitContainer);
				target.add(vrijeCapaciteitContainer);

			}
		});
		form.add(tot);

		form.add(new DependantDateValidator(vanaf, tot, Operator.AFTER));

		RadioChoice<MammaVerzettenReden> reden = new RadioChoice<MammaVerzettenReden>("reden", new ArrayList<>(MammaVerzettenReden.BRIEF_VERPLICHT),
			new EnumChoiceRenderer<>(this));
		reden.setPrefix("<label class=\"radio\">");
		reden.setSuffix("</label>");
		reden.setRequired(true);
		form.add(reden);

		add(form);

		ConfirmingIndicatingAjaxSubmitLink<Void> verzetten = new ConfirmingIndicatingAjaxSubmitLink<Void>("verzetten", form, confirmPopup, "bulk.verzetten")
		{
			private boolean bulkVerzettenMogelijk;

			@Override
			protected boolean skipConfirmation()
			{
				bulkVerzettenMogelijk = true;
				if (benodigdeCapaciteitVoorAfspraken.compareTo(capaciteit.getCapaciteit(MammaCapaciteitBlokType.values()).vrijeCapaciteit) > 0)
				{
					error(getString("bulk.verzetten.te.weinig.cap"));
					bulkVerzettenMogelijk = false;
				}
				return !bulkVerzettenMogelijk;
			}

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				MammaBulkVerzettenFilter verzettenFilter = MammaBulkVerzettenPopup.this.getModelObject();
				if (bulkVerzettenMogelijk)
				{
					afspraakService.bulkVerzetten(verzettenFilter, afsprakenModel.getObject(), ScreenitSession.get().getLoggedInAccount(), DateUtil.toLocalDate(datum.getObject()));
					verzettenAfgerond(target);
				}
			}

		};
		form.add(verzetten);
	}

	private void addOrReplaceVrijeCapaciteit(MammaBulkVerzettenFilter filter, WebMarkupContainer benodigdeCapaciteitContainer)
	{
		MammaStandplaatsPeriode standplaatsPeriode = gekozenStandplaatsPeriodeModel.getObject();

		Date vanaf = DateUtil.toUtilDate(filter.getVanafLocalDate());
		Date totEnMet = DateUtil.toUtilDate(filter.getTotEnMetLocalDate().plusDays(1));

		Collection<MammaCapaciteitBlokDto> capaciteitsBlokken = baseCapaciteitsBlokService.getNietGeblokkeerdeCapaciteitsBlokDtos(standplaatsPeriode, vanaf, totEnMet,
			EnumSet.of(MammaCapaciteitBlokType.REGULIER));
		capaciteit = baseCapaciteitsBlokService.getCapaciteit(capaciteitsBlokken);

		benodigdeCapaciteitContainer.addOrReplace(
			new Label("vrijeCapaciteitTotaal", capaciteit.getCapaciteit(MammaCapaciteitBlokType.values()).vrijeCapaciteit.setScale(1, RoundingMode.HALF_UP).toString()));
	}

	private void standplaatsPeriodeSelected(AjaxRequestTarget target)
	{
		Date minDag = Collections.max(Arrays.asList(gekozenStandplaatsPeriodeModel.getObject().getVanaf(), afspraakVerzettenZonderClientContactVanaf));
		Date maxDag = gekozenStandplaatsPeriodeModel.getObject().getScreeningsEenheid().getVrijgegevenTotEnMet() != null
			? Collections
				.min(Arrays.asList(gekozenStandplaatsPeriodeModel.getObject().getTotEnMet(),
					gekozenStandplaatsPeriodeModel.getObject().getScreeningsEenheid().getVrijgegevenTotEnMet()))
			: gekozenStandplaatsPeriodeModel.getObject().getTotEnMet();

		vanaf.remove(dateValidatorMinimum);
		tot.remove(dateValidatorMaximum);

		dateValidatorMinimum = DateValidator.minimum(minDag);
		dateValidatorMaximum = DateValidator.maximum(maxDag);

		vanaf.add(dateValidatorMinimum);
		tot.add(dateValidatorMaximum);

		MammaBulkVerzettenFilter filter = getModelObject();
		filter.setVanaf(minDag);
		filter.setTotEnMet(maxDag);
		filter.setStandplaatsPeriode(gekozenStandplaatsPeriodeModel.getObject());

		addOrReplaceVrijeCapaciteit(filter, vrijeCapaciteitContainer);

		target.add(vrijeCapaciteitContainer);
		target.add(form);
	}

	protected abstract void verzettenAfgerond(AjaxRequestTarget target);

	@Override
	protected void detachModel()
	{
		super.detachModel();
		ModelUtil.nullSafeDetach(afsprakenModel);
		ModelUtil.nullSafeDetach(standplaatsPeriodeModel);
		ModelUtil.nullSafeDetach(gekozenStandplaatsPeriodeModel);
	}
}
