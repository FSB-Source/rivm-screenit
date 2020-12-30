package nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.bmhklaboratorium.facturatie;

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

import java.math.BigDecimal;
import java.text.MessageFormat;
import java.text.NumberFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.List;

import nl.rivm.screenit.dto.cervix.facturatie.CervixVerrichtingenZoekObject;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.component.table.ClientColumn;
import nl.rivm.screenit.main.web.component.table.EnumPropertyColumn;
import nl.rivm.screenit.main.web.component.table.ExportToXslLink;
import nl.rivm.screenit.main.web.component.table.GeboortedatumColumn;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.OrganisatieBeheer;
import nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.OrganisatiePaspoortPanel;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.BMHKLaboratorium;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.cervix.CervixMonster;
import nl.rivm.screenit.model.cervix.CervixUitstrijkje;
import nl.rivm.screenit.model.cervix.enums.CervixTariefType;
import nl.rivm.screenit.model.cervix.facturatie.CervixBoekRegel;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.enums.ToegangLevel;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.InstellingService;
import nl.rivm.screenit.service.cervix.CervixVerrichtingService;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.cervix.CervixMonsterUtil;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;
import nl.topicuszorg.wicket.hibernate.SimpleHibernateModel;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import nl.topicuszorg.wicket.input.validator.BSNValidator;
import nl.topicuszorg.wicket.input.validator.DependantDateValidator.Operator;
import nl.topicuszorg.wicket.search.column.DateTimePropertyColumn;

import org.apache.commons.lang.StringUtils;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.markup.html.repeater.data.grid.ICellPopulator;
import org.apache.wicket.extensions.markup.html.repeater.data.table.AbstractColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.CheckBox;
import org.apache.wicket.markup.html.form.ChoiceRenderer;
import org.apache.wicket.markup.html.form.DropDownChoice;
import org.apache.wicket.markup.html.form.EnumChoiceRenderer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.FormComponent;
import org.apache.wicket.markup.html.form.validation.AbstractFormValidator;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.model.util.ListModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.shiro.ShiroConstraint;
import org.wicketstuff.wiquery.ui.datepicker.DatePicker;

@SecurityConstraint(
	actie = Actie.INZIEN,
	constraint = ShiroConstraint.HasPermission,
	recht = {
		Recht.GEBRUIKER_BMHK_LABORATORIA_OVERZICHT_VERRICHTINGEN },
	checkScope = true,
	level = ToegangLevel.INSTELLING,
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.CERVIX })
public class CervixBmhkLaboratoriumOverzichtVerrichtingenPage extends OrganisatieBeheer
{
	private IModel<CervixVerrichtingenZoekObject> formCriteria = new CompoundPropertyModel<>(new CervixVerrichtingenZoekObject());

	private CervixLabVerrichtingenDataProvider verrichtingenDataProvider;

	@SpringBean
	private CervixVerrichtingService verrichtingService;

	@SpringBean
	private InstellingService instellingService;

	@SpringBean
	private ICurrentDateSupplier currentDateSupplier;

	private IModel<ScreeningOrganisatie> screeningOrganisatieModel = new SimpleHibernateModel<>();

	private WebMarkupContainer verrichtingenTableContainer;

	private ScreenitDataTable<CervixBoekRegel, String> bmhkLaboratoriumVerrichtingenTabel;

	private IModel<CervixLaboratoriumVerrichtingTotalenViewObject> verrichtingTotalenModel = new CompoundPropertyModel<>(new CervixLaboratoriumVerrichtingTotalenViewObject());

	private WebMarkupContainer totalenContainer;

	private int MAXIMALE_AANTAL_MAANDEN_TUSSEN_VANAF_TOTENMET = 3;

	public CervixBmhkLaboratoriumOverzichtVerrichtingenPage()
	{
		Date vandaag = currentDateSupplier.getDateMidnight();
		Calendar calendar = Calendar.getInstance();
		calendar.add(Calendar.MONTH, -1);

		formCriteria.getObject().setVerrichtingsDatumVanaf(calendar.getTime());
		formCriteria.getObject().setVerrichtingsDatumTotenmet(vandaag);

		Form<CervixVerrichtingenZoekObject> form = new Form<>("verrichtingZoekenForm", formCriteria);
		form.setOutputMarkupId(true);
		add(form);

		add(new OrganisatiePaspoortPanel("paspoort", ModelUtil.sModel(super.getCurrentSelectedOrganisatie())));

		bepaalSoDropdown(form);

		ComponentHelper.addTextField(form, "monsterId", false, 55, false);
		FormComponent<String> bsnField = ComponentHelper.addTextField(form, "bsn", false, 9, false);
		bsnField.add(new BSNValidator());

		ComponentHelper.addTextField(form, "betalingskenmerk", false, 255, false);

		CheckBox checkBox = ComponentHelper.newCheckBox("alleenZonderBetalingskenmerk");
		checkBox.setOutputMarkupId(true);
		form.add(checkBox);

		List<CervixTariefType> tariefTypes = new ArrayList<>(Arrays.asList(CervixTariefType.getAlleLabTariefTypes()));
		final ScreenitDropdown<CervixTariefType> tariefTypesDropdown = ComponentHelper.newDropDownChoice("verrichtingsType",
			new ListModel<>(tariefTypes), new EnumChoiceRenderer<>(), false);
		tariefTypesDropdown.setNullValid(true);
		tariefTypesDropdown.setOutputMarkupId(true);
		form.add(tariefTypesDropdown);

		DatePicker<Date> vanafDatumDatePicker = ComponentHelper.newDatePicker("verrichtingsDatumVanaf", new PropertyModel<Date>(form.getModel(), "verrichtingsDatumVanaf"));
		vanafDatumDatePicker.setRequired(true);
		form.add(vanafDatumDatePicker);
		DatePicker<Date> totenmetDatumDatePicker = ComponentHelper.newDatePicker("verrichtingsDatumTotenmet",
			new PropertyModel<>(form.getModel(), "verrichtingsDatumTotenmet"));
		totenmetDatumDatePicker.setRequired(true);
		form.add(totenmetDatumDatePicker);

		form.add(new AbstractFormValidator()
		{

			@Override
			@SuppressWarnings("unchecked")
			public FormComponent<Date>[] getDependentFormComponents()
			{
				return new FormComponent[] { vanafDatumDatePicker, totenmetDatumDatePicker };
			}

			@Override
			protected String resourceKey()
			{
				return "DependantDateValidator." + Operator.AFTER.toString().toLowerCase();
			}

			@Override
			public void validate(Form<?> form)
			{
				Date referenceDate = vanafDatumDatePicker.getConvertedInput();
				Date componentDate = totenmetDatumDatePicker.getConvertedInput();

				if (referenceDate != null && componentDate != null)
				{
					Integer compare = referenceDate.compareTo(componentDate);

					if (compare > 0)
					{
						error(vanafDatumDatePicker);
					}
					else
					{
						if (DateUtil.getMonthsBetweenDates(referenceDate,
							componentDate) > MAXIMALE_AANTAL_MAANDEN_TUSSEN_VANAF_TOTENMET)
						{
							vanafDatumDatePicker.error(MessageFormat.format(getString("teveel.verschil.vanaf.totenmet"), MAXIMALE_AANTAL_MAANDEN_TUSSEN_VANAF_TOTENMET));
						}
					}
				}
			}
		});

		DatePicker<Date> geboorteDatumDatePicker = ComponentHelper.newDatePicker("geboorteDatum",
			new PropertyModel<>(form.getModel(), "geboorteDatum"));
		form.add(geboorteDatumDatePicker);

		form.add(new AbstractFormValidator()
		{

			@Override
			@SuppressWarnings("unchecked")
			public FormComponent<Date>[] getDependentFormComponents()
			{
				return new FormComponent[] { geboorteDatumDatePicker, bsnField };
			}

			@Override
			protected String resourceKey()
			{
				return "bsn.geboortedatum.niet.ingevuld";
			}

			@Override
			public void validate(Form<?> form)
			{
				String bsn = bsnField.getConvertedInput();
				Date geboorteDatum = geboorteDatumDatePicker.getConvertedInput();

				if (geboorteDatum == null && StringUtils.isNotBlank(bsn)
					|| geboorteDatum != null && StringUtils.isBlank(bsn))
				{
					error(bsnField);
				}
			}
		});

		form.add(new IndicatingAjaxSubmitLink("filteren")
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				setVerrichtingTotalenModel();
				target.add(verrichtingenTableContainer, totalenContainer);
			}
		});

		verrichtingenDataProvider = new CervixLabVerrichtingenDataProvider(formCriteria, screeningOrganisatieModel,
			ModelUtil.sModel((BMHKLaboratorium) getCurrentSelectedOrganisatie()));

		add(getVerrichtingenTableContainer());

		add(getTotalenContainer());

		verrichtingenTableContainer.add(new ExportToXslLink<>("exporteren", "overzicht-betalingen", "Exporteren", bmhkLaboratoriumVerrichtingenTabel));
	}

	private void bepaalSoDropdown(Form form)
	{
		List<ScreeningOrganisatie> screeningOrganisaties = instellingService.getAllActiefScreeningOrganisaties();

		boolean toonSOdropdown = true;
		ToegangLevel toegangLevel = ScreenitSession.get().getToegangsLevel(Actie.INZIEN, Recht.GEBRUIKER_BMHK_LABORATORIA_OVERZICHT_VERRICHTINGEN);
		if (ScreenitSession.get().getScreeningOrganisatie() != null && ToegangLevel.REGIO.equals(toegangLevel))
		{
			screeningOrganisatieModel.setObject(ScreenitSession.get().getScreeningOrganisatie());
			toonSOdropdown = false;
		}

		DropDownChoice<ScreeningOrganisatie> screeningOrganisatieDropDownChoice = ComponentHelper.newDropDownChoice("screeningOrganisatie",
			ModelUtil.listRModel(screeningOrganisaties), new ChoiceRenderer<>("naam"), false);
		screeningOrganisatieDropDownChoice.setOutputMarkupId(true);
		screeningOrganisatieDropDownChoice.setNullValid(true);
		screeningOrganisatieDropDownChoice.setModel(screeningOrganisatieModel);
		screeningOrganisatieDropDownChoice.setVisible(toonSOdropdown);
		form.add(screeningOrganisatieDropDownChoice);
		String soLabel = "-";
		if (ScreenitSession.get().getScreeningOrganisatie() != null)
		{
			soLabel = ScreenitSession.get().getScreeningOrganisatie().getNaam();
		}
		Label label = new Label("soLabel", soLabel);
		label.setVisible(!toonSOdropdown);
		form.add(label);
	}

	private WebMarkupContainer getTotalenContainer()
	{
		totalenContainer = new WebMarkupContainer("totalenContainer");
		totalenContainer.setOutputMarkupId(true);
		totalenContainer.setDefaultModel(verrichtingTotalenModel);
		setVerrichtingTotalenModel();

		totalenContainer.add(new Label("hpvAnalyseUitstrijkjeTariefTotaal"));
		totalenContainer.add(new Label("hpvAnalyseZasTariefTotaal"));
		totalenContainer.add(new Label("cytologieNaHpvUitstrijkjeTariefTotaal"));
		totalenContainer.add(new Label("cytologieNaHpvZasTariefTotaal"));
		totalenContainer.add(new Label("cytologieVervolguitstrijkjeTariefTotaal"));

		totalenContainer.add(new Label("totaalBedrag"));

		return totalenContainer;
	}

	private void setVerrichtingTotalenModel()
	{
		CervixLaboratoriumVerrichtingTotalenViewObject verrichtingTotalenViewObject = verrichtingTotalenModel.getObject();

		BigDecimal hpvAnalyseUitstrijkjeBedrag = getTotaalBedrag(CervixTariefType.LAB_HPV_ANALYSE_UITSTRIJKJE);
		BigDecimal hpvAnalyseZasBedrag = getTotaalBedrag(CervixTariefType.LAB_HPV_ANALYSE_ZAS);
		BigDecimal cytologieNaHpvUitstrijkje = getTotaalBedrag(CervixTariefType.LAB_CYTOLOGIE_NA_HPV_UITSTRIJKJE);
		BigDecimal cytologieNaHpvZas = getTotaalBedrag(CervixTariefType.LAB_CYTOLOGIE_NA_HPV_ZAS);
		BigDecimal cytologieVervolguitstrijkjeTariefTotaal = getTotaalBedrag(CervixTariefType.LAB_CYTOLOGIE_VERVOLGUITSTRIJKJE);

		verrichtingTotalenViewObject.setHpvAnalyseUitstrijkjeTariefTotaal(NumberFormat.getCurrencyInstance().format(hpvAnalyseUitstrijkjeBedrag));
		verrichtingTotalenViewObject.setHpvAnalyseZasTariefTotaal(NumberFormat.getCurrencyInstance().format(hpvAnalyseZasBedrag));
		verrichtingTotalenViewObject.setCytologieNaHpvUitstrijkjeTariefTotaal(NumberFormat.getCurrencyInstance().format(cytologieNaHpvUitstrijkje));
		verrichtingTotalenViewObject.setCytologieNaHpvZasTariefTotaal(NumberFormat.getCurrencyInstance().format(cytologieNaHpvZas));
		verrichtingTotalenViewObject.setCytologieVervolguitstrijkjeTariefTotaal(NumberFormat.getCurrencyInstance().format(cytologieVervolguitstrijkjeTariefTotaal));

		BigDecimal totaalBedrag = hpvAnalyseUitstrijkjeBedrag.add(hpvAnalyseZasBedrag).add(cytologieNaHpvUitstrijkje).add(cytologieNaHpvZas)
			.add(cytologieVervolguitstrijkjeTariefTotaal);
		verrichtingTotalenViewObject.setTotaalBedrag(NumberFormat.getCurrencyInstance().format(totaalBedrag));

	}

	private BigDecimal getTotaalBedrag(CervixTariefType tariefType)
	{
		BigDecimal bedrag = verrichtingService.getLaboratoriumTotaalBedrag(formCriteria.getObject(), screeningOrganisatieModel.getObject(),
			verrichtingenDataProvider.getGeselecteerdeOrganisatie(), tariefType);
		if (bedrag == null)
		{
			bedrag = BigDecimal.ZERO;
		}
		return bedrag;
	}

	private WebMarkupContainer getVerrichtingenTableContainer()
	{
		verrichtingenTableContainer = new WebMarkupContainer("verrichtingenTableContainer");
		verrichtingenTableContainer.setOutputMarkupId(true);

		SimpleDateFormat dateFormatter = new SimpleDateFormat("dd-MM-yyyy");

		List<IColumn<CervixBoekRegel, String>> columns = new ArrayList<>();
		columns.add(new PropertyColumn<>(Model.of("Screeningsorganisatie"), "regio.naam", "verrichting.regio.naam"));
		columns.add(new ClientColumn<>("persoon.achternaam", "verrichting.client"));
		columns.add(new GeboortedatumColumn<>("persoon.geboortedatum", "verrichting.client.persoon"));
		columns.add(new PropertyColumn<>(Model.of("BSN"), "persoon.bsn", "verrichting.client.persoon.bsn"));
		columns.add(new PropertyColumn<>(Model.of("Monster-id"), "monster.monsterId", "verrichting.monster.monsterId"));
		columns.add(new EnumPropertyColumn<CervixBoekRegel, String, CervixTariefType>(Model.of("Type verrichting"), "verrichting.type", "verrichting.type"));
		columns.add(new DateTimePropertyColumn<>(Model.of("Verrichtingsdatum"), "verrichting.verrichtingsDatum", "verrichting.verrichtingsDatum", dateFormatter));
		columns.add(new DateTimePropertyColumn<>(Model.of("Ontvangst monster"), "verrichting.monster.ontvangstdatum", "monster.ontvangstdatum", dateFormatter));
		columns
			.add(new DateTimePropertyColumn<CervixBoekRegel, String>(Model.of("Ontvangst formulier"), "verrichting.monster.labformulier.scanDatum", "labformulier.scanDatum",
				dateFormatter)
			{

				@Override
				public IModel<Object> getDataModel(IModel<CervixBoekRegel> embeddedModel)
				{
					SimpleDateFormat dateFormat = new SimpleDateFormat("dd-MM-yyyy");
					CervixMonster monster = embeddedModel.getObject().getVerrichting().getMonster();
					if (CervixMonsterUtil.isZAS(monster))
					{
						return new Model("");
					}
					else
					{
						CervixUitstrijkje uitstrijkje = CervixMonsterUtil.getUitstrijkje(monster);
						if (uitstrijkje.getLabformulier() != null && uitstrijkje.getLabformulier().getScanDatum() != null)
						{
							String scanDatum = DateUtil.LOCAL_DATE_FORMAT.format(DateUtil.toLocalDate(uitstrijkje.getLabformulier().getScanDatum()));
							return (IModel) Model.of(scanDatum);
						}
						else
						{
							return (IModel) Model.of("");
						}
					}
				}
			});
		columns.add(new AbstractColumn<CervixBoekRegel, String>(Model.of("Bedrag"))
		{
			@Override
			public void populateItem(Item<ICellPopulator<CervixBoekRegel>> cellItem, String componentId, IModel<CervixBoekRegel> rowModel)
			{
				IModel<BigDecimal> labelModel = new PropertyModel<>(rowModel, "tarief." + rowModel.getObject().getVerrichting().getType().getBedragProperty());

				BigDecimal bedrag = labelModel.getObject();
				if (rowModel.getObject().getDebet())
				{
					bedrag = bedrag.negate();
				}
				cellItem.add(new Label(componentId, NumberFormat.getCurrencyInstance().format(bedrag)));
			}
		});
		columns.add(new DateTimePropertyColumn<>(Model.of("Betalingsdatum"), "specificatie.betaalopdrachtRegel.betaalopdracht.statusDatum", "betaalopdracht.statusDatum",
			dateFormatter));
		columns.add(new PropertyColumn<>(Model.of("Betalingskenmerk"), "betaalopdracht.betalingskenmerk", "specificatie.betaalopdrachtRegel.betaalopdracht.betalingskenmerk"));

		bmhkLaboratoriumVerrichtingenTabel = new ScreenitDataTable<CervixBoekRegel, String>("bmhkLaboratoriumVerrichtingenTabel", columns,
			verrichtingenDataProvider, Model.of("boekingsregels"))
		{
			@Override
			protected boolean isRowClickable(IModel<CervixBoekRegel> rowModel)
			{
				return false;
			}
		};
		bmhkLaboratoriumVerrichtingenTabel.setOutputMarkupId(true);
		verrichtingenTableContainer.add(bmhkLaboratoriumVerrichtingenTabel);
		return verrichtingenTableContainer;
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
		ModelUtil.nullSafeDetach(formCriteria);
		ModelUtil.nullSafeDetach(screeningOrganisatieModel);
		ModelUtil.nullSafeDetach(verrichtingTotalenModel);
	}
}
