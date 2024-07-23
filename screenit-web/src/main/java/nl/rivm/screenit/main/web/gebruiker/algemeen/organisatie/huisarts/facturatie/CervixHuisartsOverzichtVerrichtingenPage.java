package nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.huisarts.facturatie;

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

import java.math.BigDecimal;
import java.text.NumberFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import nl.rivm.screenit.dto.cervix.facturatie.CervixVerrichtingenZoekObject;
import nl.rivm.screenit.main.service.cervix.CervixVerrichtingService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.main.web.component.price.BigDecimalPriceLabel;
import nl.rivm.screenit.main.web.component.table.ClientColumn;
import nl.rivm.screenit.main.web.component.table.ExportToCsvLink;
import nl.rivm.screenit.main.web.component.table.GeboortedatumColumn;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.CervixHuisartsPaspoortPanel;
import nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.OrganisatieBeheer;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.GbaPersoon_;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.cervix.CervixHuisarts;
import nl.rivm.screenit.model.cervix.CervixHuisartsLocatie;
import nl.rivm.screenit.model.cervix.CervixHuisartsLocatie_;
import nl.rivm.screenit.model.cervix.CervixLabformulier_;
import nl.rivm.screenit.model.cervix.CervixMonster_;
import nl.rivm.screenit.model.cervix.enums.CervixTariefType;
import nl.rivm.screenit.model.cervix.facturatie.CervixBetaalopdracht_;
import nl.rivm.screenit.model.cervix.facturatie.CervixBoekRegel;
import nl.rivm.screenit.model.cervix.facturatie.CervixBoekRegel_;
import nl.rivm.screenit.model.cervix.facturatie.CervixHuisartsTarief;
import nl.rivm.screenit.model.cervix.facturatie.CervixVerrichting_;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.enums.ToegangLevel;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.InstellingService;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.cervix.CervixMonsterUtil;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;
import nl.topicuszorg.organisatie.model.Organisatie_;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;
import nl.topicuszorg.wicket.hibernate.SimpleHibernateModel;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import nl.topicuszorg.wicket.input.validator.BSNValidator;
import nl.topicuszorg.wicket.input.validator.DependantDateValidator;
import nl.topicuszorg.wicket.search.column.DateTimePropertyColumn;

import org.apache.commons.lang.StringUtils;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.markup.html.repeater.data.grid.ICellPopulator;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.export.AbstractExportableColumn;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.ChoiceRenderer;
import org.apache.wicket.markup.html.form.DropDownChoice;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.FormComponent;
import org.apache.wicket.markup.html.form.validation.AbstractFormValidator;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.shiro.ShiroConstraint;
import org.wicketstuff.wiquery.ui.datepicker.DatePicker;

import static nl.rivm.screenit.main.service.cervix.impl.AbstractCervixBoekregelsDataProviderServiceImpl.BETAALOPDRACHT_PROPERTY;
import static nl.rivm.screenit.main.service.cervix.impl.AbstractCervixBoekregelsDataProviderServiceImpl.HUISARTS_LOCATIE_PROPERTY;
import static nl.rivm.screenit.main.service.cervix.impl.AbstractCervixBoekregelsDataProviderServiceImpl.LABFORMULIER_PROPERTY;
import static nl.rivm.screenit.main.service.cervix.impl.AbstractCervixBoekregelsDataProviderServiceImpl.MONSTER_PROPERTY;
import static nl.rivm.screenit.main.service.cervix.impl.AbstractCervixBoekregelsDataProviderServiceImpl.PERSOON_PROPERTY;
import static nl.rivm.screenit.main.service.cervix.impl.AbstractCervixBoekregelsDataProviderServiceImpl.REGIO_PROPERTY;

@SecurityConstraint(actie = Actie.INZIEN, constraint = ShiroConstraint.HasPermission, recht = {
	Recht.GEBRUIKER_BMHK_HUISARTS_OVERZICHT_VERRICHTINGEN }, checkScope = true, level = ToegangLevel.INSTELLING, bevolkingsonderzoekScopes = { Bevolkingsonderzoek.CERVIX })
public class CervixHuisartsOverzichtVerrichtingenPage extends OrganisatieBeheer
{
	private final SimpleDateFormat dateFormatter = new SimpleDateFormat("dd-MM-yyyy");

	private IModel<CervixVerrichtingenZoekObject> formCriteria = new CompoundPropertyModel<>(new CervixVerrichtingenZoekObject());

	private CervixHuisartsVerrichtingenDataProvider verrichtingenDataProvider;

	@SpringBean
	private CervixVerrichtingService verrichtingService;

	@SpringBean
	private InstellingService instellingService;

	@SpringBean
	private ICurrentDateSupplier currentDateSupplier;

	private WebMarkupContainer verrichtingenTableContainer;

	private ScreenitDataTable<CervixBoekRegel, String> huisartsVerrichtingenTabel;

	private WebMarkupContainer totalenContainer;

	private IModel<CervixHuisartsLocatie> huisartsLocatieModel;

	private IModel<ScreeningOrganisatie> screeningOrganisatieModel = new SimpleHibernateModel<>();

	private IModel<CervixHuisartsVerrichtingTotalenViewObject> verrichtingTotalenModel = new CompoundPropertyModel<>(new CervixHuisartsVerrichtingTotalenViewObject());

	public CervixHuisartsOverzichtVerrichtingenPage()
	{
		ScreenitForm<CervixVerrichtingenZoekObject> form = new ScreenitForm<>("verrichtingZoekenForm", formCriteria);
		form.setOutputMarkupId(true);
		add(form);

		add(new CervixHuisartsPaspoortPanel("paspoort", ModelUtil.sModel((CervixHuisarts) getCurrentSelectedOrganisatie())));

		var vandaag = currentDateSupplier.getLocalDate();

		formCriteria.getObject().setVerrichtingsDatumVanaf(DateUtil.toUtilDate(vandaag.minusMonths(1)));
		formCriteria.getObject().setVerrichtingsDatumTotenmet(DateUtil.toUtilDate(vandaag));
		formCriteria.getObject().setVerrichtingsType(CervixTariefType.HUISARTS_UITSTRIJKJE);

		bepaalSoDropdown(form);

		var huisarts = (CervixHuisarts) getCurrentSelectedOrganisatie();

		huisartsLocatieModel = new SimpleHibernateModel<>();
		DropDownChoice<CervixHuisartsLocatie> huisartsLocatieDropDownChoice = ComponentHelper.newDropDownChoice("huisartsLocatie",
			ModelUtil.listRModel(huisarts.getHuisartsLocaties()), new ChoiceRenderer<>("naam"),
			false);
		huisartsLocatieDropDownChoice.setNullValid(true);
		huisartsLocatieDropDownChoice.setOutputMarkupId(true);
		huisartsLocatieDropDownChoice.setModel(huisartsLocatieModel);
		form.add(huisartsLocatieDropDownChoice);

		ComponentHelper.addTextField(form, "monsterId", false, 55, false);
		FormComponent<String> bsnField = ComponentHelper.addTextField(form, "bsn", false, 9, false);
		bsnField.add(new BSNValidator());

		ComponentHelper.addTextField(form, "betalingskenmerk", false, 255, false);

		var checkBox = ComponentHelper.newCheckBox("alleenZonderBetalingskenmerk");
		checkBox.setOutputMarkupId(true);
		form.add(checkBox);

		form.add(new IndicatingAjaxSubmitLink("filteren")
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				var criteria = formCriteria.getObject();
				if ((criteria.getGeboorteDatum() == null && StringUtils.isNotBlank(criteria.getBsn()))
					|| (criteria.getGeboorteDatum() != null && StringUtils.isBlank(criteria.getBsn())))
				{
					error("Zowel bsn als geboortedatum dienen ingevuld te zijn!");
				}
				else
				{
					setVerrichtingTotalenModel();
					target.add(huisartsVerrichtingenTabel, totalenContainer);
				}
			}
		});

		DatePicker<Date> vanafDatumDatePicker = ComponentHelper.newDatePicker("verrichtingsDatumVanaf", new PropertyModel<>(form.getModel(), "verrichtingsDatumVanaf"));
		form.add(vanafDatumDatePicker);
		DatePicker<Date> totenmetDatumDatePicker = ComponentHelper.newDatePicker("verrichtingsDatumTotenmet",
			new PropertyModel<>(form.getModel(), "verrichtingsDatumTotenmet"));
		form.add(totenmetDatumDatePicker);

		form.add(new DependantDateValidator(vanafDatumDatePicker, totenmetDatumDatePicker, DependantDateValidator.Operator.AFTER));

		DatePicker<Date> geboorteDatumDatePicker = ComponentHelper.newDatePicker("geboorteDatum",
			new PropertyModel<>(form.getModel(), "geboorteDatum"));
		form.add(geboorteDatumDatePicker);

		DatePicker<Date> datumUitstrijkjeDatePicker = ComponentHelper.newDatePicker("datumUitstrijkje",
			new PropertyModel<>(form.getModel(), "datumUitstrijkje"));
		form.add(datumUitstrijkjeDatePicker);

		form.add(new AbstractFormValidator()
		{
			@Override
			public FormComponent<?>[] getDependentFormComponents()
			{
				return new FormComponent[] { bsnField, geboorteDatumDatePicker };
			}

			@Override
			public void validate(Form<?> form)
			{
				var bsn = bsnField.getConvertedInput();
				var geboorteDatum = geboorteDatumDatePicker.getConvertedInput();
				if ((geboorteDatum == null && StringUtils.isNotBlank(bsn))
					|| (geboorteDatum != null && StringUtils.isBlank(bsn)))
				{
					form.error("Zowel bsn als geboortedatum dienen ingevuld te zijn!");
				}
			}
		});

		verrichtingenDataProvider = new CervixHuisartsVerrichtingenDataProvider(formCriteria, screeningOrganisatieModel,
			ModelUtil.sModel((CervixHuisarts) getCurrentSelectedOrganisatie()),
			huisartsLocatieModel);

		add(getVerrichtingenTableContainer());

		add(getTotalenContainer());

		verrichtingenTableContainer.add(new ExportToCsvLink<>("exporteren", "overzicht-betalingen", "Exporteren", huisartsVerrichtingenTabel.getDataProvider(), getColumns()));
	}

	private void bepaalSoDropdown(Form<?> form)
	{
		var toonSOdropdown = true;
		var toegangLevel = ScreenitSession.get().getToegangsLevel(Actie.INZIEN, Recht.GEBRUIKER_BMHK_HUISARTS_OVERZICHT_VERRICHTINGEN);
		if (ScreenitSession.get().getScreeningOrganisatie() != null && ToegangLevel.REGIO.equals(toegangLevel))
		{
			screeningOrganisatieModel.setObject(ScreenitSession.get().getScreeningOrganisatie());
			toonSOdropdown = false;
		}

		List<ScreeningOrganisatie> screeningOrganisaties = instellingService.getAllActiefScreeningOrganisaties();
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
		var label = new Label("soLabel", soLabel);
		label.setVisible(!toonSOdropdown);
		form.add(label);
	}

	private WebMarkupContainer getTotalenContainer()
	{
		totalenContainer = new WebMarkupContainer("totalenContainer");
		totalenContainer.setOutputMarkupId(true);
		totalenContainer.setDefaultModel(verrichtingTotalenModel);
		setVerrichtingTotalenModel();

		var aantalUitstrijkjes = new Label("aantalUitstrijkjes");
		totalenContainer.add(aantalUitstrijkjes);
		totalenContainer.add(new Label("totaalBedrag"));
		return totalenContainer;
	}

	private void setVerrichtingTotalenModel()
	{
		var verrichtingTotalenModelObject = verrichtingTotalenModel.getObject();
		formCriteria.getObject().setAlleenVerrichtingen(true);
		verrichtingTotalenModelObject.setAantalUitstrijkjes(verrichtingenDataProvider.size() + "");
		formCriteria.getObject().setAlleenVerrichtingen(false);
		verrichtingTotalenModelObject.setTotaalBedrag(NumberFormat.getCurrencyInstance().format(getTotaalBedrag()));

		verrichtingTotalenModel.setObject(verrichtingTotalenModelObject);
	}

	private BigDecimal getTotaalBedrag()
	{
		var bedrag = verrichtingService.getHuisartsTotaalBedrag(formCriteria.getObject(), screeningOrganisatieModel.getObject(),
			verrichtingenDataProvider.getGeselecteerdeHuisarts(), huisartsLocatieModel.getObject());
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

		huisartsVerrichtingenTabel = new ScreenitDataTable<>("huisartsVerrichtingenTabel", getColumns(),
			verrichtingenDataProvider, Model.of("boekingsregels"))
		{
			@Override
			protected boolean isRowClickable(IModel<CervixBoekRegel> rowModel)
			{
				return false;
			}
		};
		huisartsVerrichtingenTabel.setOutputMarkupId(true);
		verrichtingenTableContainer.add(huisartsVerrichtingenTabel);
		return verrichtingenTableContainer;
	}

	private List<IColumn<CervixBoekRegel, String>> getColumns()
	{
		List<IColumn<CervixBoekRegel, String>> columns = new ArrayList<>();
		columns.add(new PropertyColumn<>(Model.of("Screeningsorganisatie"), REGIO_PROPERTY + "." + Organisatie_.NAAM, REGIO_PROPERTY + "." + Organisatie_.NAAM));
		columns.add(new PropertyColumn<>(Model.of("Locatie"), HUISARTS_LOCATIE_PROPERTY + "." + CervixHuisartsLocatie_.naam,
			HUISARTS_LOCATIE_PROPERTY + "." + CervixHuisartsLocatie_.NAAM));
		columns.add(new ClientColumn<>(PERSOON_PROPERTY + "." + GbaPersoon_.ACHTERNAAM, CervixBoekRegel_.VERRICHTING + "." + CervixVerrichting_.CLIENT));
		columns.add(new GeboortedatumColumn<>(PERSOON_PROPERTY + "." + GbaPersoon_.GEBOORTEDATUM, PERSOON_PROPERTY));
		columns.add(new PropertyColumn<>(Model.of("BSN"), PERSOON_PROPERTY + "." + GbaPersoon_.BSN, PERSOON_PROPERTY + "." + GbaPersoon_.BSN));
		columns.add(new PropertyColumn<>(Model.of("Monster-id"), MONSTER_PROPERTY + "." + CervixMonster_.MONSTER_ID, MONSTER_PROPERTY + "." + CervixMonster_.MONSTER_ID));
		columns.add(new DateTimePropertyColumn<>(Model.of("Verrichtingsdatum"), CervixBoekRegel_.VERRICHTING + "." + CervixVerrichting_.VERRICHTINGS_DATUM,
			CervixBoekRegel_.VERRICHTING + "." + CervixVerrichting_.VERRICHTINGS_DATUM, dateFormatter));
		columns.add(new PropertyColumn<>(Model.of("Datum uitstrijkje"), LABFORMULIER_PROPERTY + "." + CervixLabformulier_.DATUM_UITSTRIJKJE,
			LABFORMULIER_PROPERTY + "." + CervixLabformulier_.DATUM_UITSTRIJKJE)
		{

			@Override
			public IModel<String> getDataModel(IModel<CervixBoekRegel> embeddedModel)
			{
				var dateFormat = new SimpleDateFormat("dd-MM-yyyy");
				var monster = embeddedModel.getObject().getVerrichting().getMonster();
				if (!CervixMonsterUtil.isZAS(monster))
				{
					var uitstrijkje = CervixMonsterUtil.getUitstrijkje(monster);
					if (uitstrijkje.getLabformulier() != null && uitstrijkje.getLabformulier().getDatumUitstrijkje() != null)
					{
						return Model.of(dateFormat.format(uitstrijkje.getLabformulier().getDatumUitstrijkje()));
					}
				}
				return Model.of("");
			}
		});
		columns.add(new PropertyColumn<>(Model.of("Ontvangst formulier"), "labformulier.scanDatum", "verrichting.monster.labformulier.scanDatum")
		{

			@Override
			public IModel<String> getDataModel(IModel<CervixBoekRegel> embeddedModel)
			{
				var dateFormat = new SimpleDateFormat("dd-MM-yyyy");

				var monster = embeddedModel.getObject().getVerrichting().getMonster();
				if (!CervixMonsterUtil.isZAS(monster))
				{
					var uitstrijkje = CervixMonsterUtil.getUitstrijkje(monster);
					if (uitstrijkje.getLabformulier() != null && uitstrijkje.getLabformulier().getScanDatum() != null)
					{
						return Model.of(dateFormat.format(uitstrijkje.getLabformulier().getScanDatum()));
					}
				}
				return Model.of("");
			}
		});
		columns.add(new AbstractExportableColumn<>(Model.of("Bedrag"))
		{
			@Override
			public void populateItem(Item<ICellPopulator<CervixBoekRegel>> cellItem, String componentId, IModel<CervixBoekRegel> rowModel)
			{
				cellItem.add(new BigDecimalPriceLabel(componentId, getBoekregel(rowModel)));
			}

			private BigDecimal getBoekregel(IModel<CervixBoekRegel> rowModel)
			{
				var boekRegel = rowModel.getObject();
				var tarief = (CervixHuisartsTarief) HibernateHelper.deproxy(boekRegel.getTarief());
				var bedrag = tarief.getTarief();
				if (boekRegel.getDebet())
				{
					bedrag = bedrag.negate();
				}
				return bedrag;
			}

			@Override
			public IModel<String> getDataModel(IModel<CervixBoekRegel> rowModel)
			{
				return Model.of(NumberFormat.getCurrencyInstance().format(getBoekregel(rowModel)));
			}
		});

		columns.add(new DateTimePropertyColumn<>(Model.of("Betalingsdatum"), BETAALOPDRACHT_PROPERTY + "." + CervixBetaalopdracht_.STATUS_DATUM,
			BETAALOPDRACHT_PROPERTY + "." + CervixBetaalopdracht_.STATUS_DATUM,
			dateFormatter));
		columns.add(new PropertyColumn<>(Model.of("Betalingskenmerk"), BETAALOPDRACHT_PROPERTY + "." + CervixBetaalopdracht_.BETALINGSKENMERK,
			BETAALOPDRACHT_PROPERTY + "." + CervixBetaalopdracht_.BETALINGSKENMERK));
		return columns;
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
		ModelUtil.nullSafeDetach(huisartsLocatieModel);
		ModelUtil.nullSafeDetach(screeningOrganisatieModel);
		ModelUtil.nullSafeDetach(verrichtingTotalenModel);
	}
}
