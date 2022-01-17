package nl.rivm.screenit.main.web.gebruiker.screening.mamma.afspraken;

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

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.text.SimpleDateFormat;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.dao.mamma.MammaBaseBlokkadeDao;
import nl.rivm.screenit.dao.mamma.MammaBaseStandplaatsPeriodeDao;
import nl.rivm.screenit.main.dao.mamma.MammaScreeningsEenheidDao;
import nl.rivm.screenit.main.service.mamma.MammaStandplaatsPeriodeService;
import nl.rivm.screenit.main.util.StandplaatsPeriodeUtil;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.component.modal.DefaultDialogCloseCallback;
import nl.rivm.screenit.main.web.component.modal.IDialog;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerBasePage;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaBlokkade;
import nl.rivm.screenit.model.mamma.MammaCapaciteitBlok;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.MammaStandplaats;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode;
import nl.rivm.screenit.model.mamma.enums.MammaAfspraakStatus;
import nl.rivm.screenit.model.mamma.enums.MammaCapaciteitBlokType;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.mamma.MammaBaseAfspraakService;
import nl.rivm.screenit.service.mamma.MammaBaseCapaciteitsBlokService;
import nl.rivm.screenit.service.mamma.MammaBaseConceptPlanningsApplicatie;
import nl.rivm.screenit.util.BigDecimalUtil;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import nl.topicuszorg.wicket.search.column.HibernateCheckBoxListContainer;

import org.apache.commons.lang.StringUtils;
import org.apache.wicket.Component;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormComponentUpdatingBehavior;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.behavior.AttributeAppender;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.ChoiceRenderer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.repeater.RepeatingView;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.datetime.markup.html.basic.DateLabel;
import org.wicketstuff.shiro.ShiroConstraint;
import org.wicketstuff.wiquery.ui.datepicker.DatePicker;

@SecurityConstraint(
	actie = Actie.INZIEN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = { Recht.GEBRUIKER_SCREENING_MAMMA_AFSPRAKEN_BEHEER },
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.MAMMA })
public class MammaAfsprakenDagOverzichtPage extends MammaAfsprakenBasePage
{
	private static final long serialVersionUID = 1L;

	@SpringBean
	private MammaScreeningsEenheidDao screeningsEenheidDao;

	@SpringBean
	private MammaBaseAfspraakService baseAfspraakService;

	@SpringBean
	private MammaBaseCapaciteitsBlokService baseCapaciteitsBlokService;

	@SpringBean
	private MammaStandplaatsPeriodeService standplaatsPeriodeService;

	@SpringBean
	private MammaBaseBlokkadeDao baseBlokkadeDao;

	@SpringBean
	private MammaBaseStandplaatsPeriodeDao baseStandplaatsPeriodeDao;

	@SpringBean
	private MammaBaseConceptPlanningsApplicatie baseConceptPlanningsApplicatie;

	@SpringBean
	private SimplePreferenceService preferenceService;

	@SpringBean
	private ICurrentDateSupplier dateSupplier;

	private IModel<MammaScreeningsEenheid> screeningsEenheidModel;

	private IModel<List<MammaScreeningsEenheid>> screeningsEenhedenModel;

	private IModel<Date> datumModel;

	private HibernateCheckBoxListContainer<MammaAfspraak> selectedAfspraken = new HibernateCheckBoxListContainer<>();

	private BootstrapDialog dialog;

	private Form form;

	private WebMarkupContainer capaciteitContainer;

	private boolean magVerzetten;

	private boolean magBulkVerzetten;

	private Component volledigDagLabel;

	private WebMarkupContainer metaInfo;

	public MammaAfsprakenDagOverzichtPage(IModel<MammaScreeningsEenheid> screeningsEenheidModel, Date startDatum)
	{
		magVerzetten = ScreenitSession.get().checkPermission(Recht.GEBRUIKER_CLIENT_MAMMA_AFSPRAAK_WIJZIGEN, Actie.AANPASSEN) && ingelogdNamensRegio;
		magBulkVerzetten = ScreenitSession.get().checkPermission(Recht.GEBRUIKER_MAMMA_AFSPRAAK_BULK_VERZETTEN, Actie.AANPASSEN) && ingelogdNamensRegio;

		ScreeningOrganisatie sessionSO = ScreenitSession.get().getScreeningOrganisatie();
		this.screeningsEenheidModel = ModelUtil.cRModel(screeningsEenheidModel.getObject());

		setDefaultModel(this.screeningsEenheidModel);

		this.screeningsEenhedenModel = ModelUtil.listRModel(screeningsEenheidDao.getActieveScreeningsEenhedenVoorScreeningOrganisatie(sessionSO));

		addNavigation();

		ScreenitDropdown<MammaScreeningsEenheid> screeningsEenheidDropdown = new ScreenitDropdown<>("screeningsEenheidDropdown", this.screeningsEenheidModel,
			screeningsEenhedenModel, new ChoiceRenderer<>("naam"));
		add(screeningsEenheidDropdown);
		screeningsEenheidDropdown.add(new AjaxFormComponentUpdatingBehavior("change")
		{
			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				refreshKalender(target);
			}
		});

		metaInfo = new WebMarkupContainer("metaInfo");
		add(metaInfo.setOutputMarkupId(true));

		datumModel = Model.of(startDatum);
		DatePicker<Date> datumField = ComponentHelper.newDatePicker("datum", datumModel, true);
		datumField.setOutputMarkupId(true);
		add(datumField);
		datumField.add(new AjaxFormComponentUpdatingBehavior("change")
		{
			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				if (datumModel.getObject() == null)
				{
					datumModel.setObject(startDatum);
				}
				target.add(datumField);
				refreshKalender(target);
			}
		});
		add(new IndicatingAjaxLink<Void>("prevDay")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				Date datum = datumModel.getObject();
				datumModel.setObject(DateUtil.toUtilDate(DateUtil.toLocalDate(datum).minusDays(1)));
				target.add(datumField);
				refreshKalender(target);
			}
		});
		add(new IndicatingAjaxLink<Void>("nextDay")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				Date datum = datumModel.getObject();
				datumModel.setObject(DateUtil.toUtilDate(DateUtil.toLocalDate(datum).plusDays(1)));
				target.add(datumField);
				refreshKalender(target);
			}
		});
		refreshKalender(null);
	}

	private void addNavigation()
	{
		dialog = new BootstrapDialog("dialog");
		dialog.setCloseCallback(new DefaultDialogCloseCallback());
		add(dialog);

		form = new ScreenitForm<Void>("form");
		form.setOutputMarkupId(true);
		add(form);

		capaciteitContainer = new WebMarkupContainer("capaciteitContainer");
		capaciteitContainer.setOutputMarkupId(true);
		form.add(capaciteitContainer);

		add(new AjaxLink<Void>("terug")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				setResponsePage(MammaAfsprakenSEZoekenPage.class);
			}
		});
	}

	private void refreshKalender(AjaxRequestTarget target)
	{
		selectedAfspraken.setCheckForAll(false);
		RepeatingView blokken = new RepeatingView("calendar");

		Date datum = datumModel.getObject();
		volledigDagLabel = DateLabel.forDatePattern("dag", Model.of(datum), "EEEE dd MMMM yyyy").setOutputMarkupId(true);
		addOrReplace(volledigDagLabel);
		addMetaInfo();

		LocalDate currentDay = DateUtil.toLocalDate(datum);
		LocalDate minimaleDagVoorBulkVerzetten = dateSupplier.getLocalDate()
			.minusWeeks(preferenceService.getInteger(PreferenceKey.MAMMA_BULK_VERZETTEN_IN_VERLEDEN_AANTAL_WEKEN.name(), 2));

		boolean magNuVerzetten = magVerzetten && !currentDay.isBefore(minimaleDagVoorBulkVerzetten);
		boolean magNuBulkVerzetten = magBulkVerzetten && !currentDay.isBefore(minimaleDagVoorBulkVerzetten);

		MammaScreeningsEenheid screeningsEenheid = screeningsEenheidModel.getObject();

		Date vanaf = DateUtil.toUtilDate(currentDay);
		Date totEnMet = DateUtil.toUtilDate(currentDay.plusDays(1));

		long aantalAfspraken = baseAfspraakService.countAfspraken(screeningsEenheid, vanaf, totEnMet, MammaAfspraakStatus.NIET_GEANNULEERD.toArray(new MammaAfspraakStatus[0]));
		capaciteitContainer.addOrReplace(new Label("aantalAfspraken", Model.of(aantalAfspraken)));

		List<MammaAfspraak> afspraken = baseAfspraakService.getAfspraken(screeningsEenheid, vanaf, totEnMet, MammaAfspraakStatus.GEPLAND);
		List<MammaCapaciteitBlok> capaciteitsBlokken = baseCapaciteitsBlokService.getCapaciteitsBlokken(screeningsEenheid, vanaf, totEnMet, true,
			Arrays.asList(MammaCapaciteitBlokType.REGULIER, MammaCapaciteitBlokType.TEHUIS));

		blokken.add(new MammaAfsprakenZonderBlokPanel(blokken.newChildId(), screeningsEenheidModel, currentDay, selectedAfspraken, dialog, magNuVerzetten, magNuBulkVerzetten,
			afspraken.stream().filter(afspraak -> afspraak.getCapaciteitBlok() == null).collect(Collectors.toList())));

		BigDecimal vrijeCapaciteit = BigDecimal.ZERO;
		BigDecimal beschikbareCapaciteit = BigDecimal.ZERO;
		for (MammaCapaciteitBlok blok : capaciteitsBlokken)
		{
			blokken.add(new MammaBlokMetAfsprakenPanel(blokken.newChildId(), ModelUtil.cRModel(blok), selectedAfspraken, currentDay, dialog, magNuVerzetten, magNuBulkVerzetten));
			vrijeCapaciteit = vrijeCapaciteit.add(blok.getVrijeCapaciteit());
			beschikbareCapaciteit = beschikbareCapaciteit.add(blok.getBeschikbareCapaciteit());
		}
		capaciteitContainer.addOrReplace(new Label("vrijeCapaciteit", vrijeCapaciteit.setScale(1, RoundingMode.HALF_UP).toString()));
		capaciteitContainer.addOrReplace(new Label("beschikbareCapaciteit", BigDecimalUtil.decimalToString(beschikbareCapaciteit)));
		form.addOrReplace(blokken);

		WebMarkupContainer selectAllCheckboxContainer = new WebMarkupContainer("selectAllCheckboxContainer");
		selectAllCheckboxContainer.setVisible(magNuBulkVerzetten);
		selectAllCheckboxContainer.setOutputMarkupId(true);
		selectAllCheckboxContainer.setOutputMarkupPlaceholderTag(true);
		form.addOrReplace(selectAllCheckboxContainer);

		IndicatingAjaxSubmitLink bulkVerzetten = new IndicatingAjaxSubmitLink("bulkVerzetten", form)
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				super.onSubmit(target);

				List<MammaAfspraak> afspraken = selectedAfspraken.getList();
				List<MammaStandplaatsPeriode> standplaatsPeriodesVoorBulkVerzetten = standplaatsPeriodeService
					.getStandplaatsPeriodesVoorBulkVerzetten(ScreenitSession.get().getScreeningOrganisatie());

				if (afspraken.isEmpty())
				{
					error(getString("error.geen.afspraken.voor.bulk.verzetten"));
				}
				else if (standplaatsPeriodesVoorBulkVerzetten.isEmpty())
				{
					error(getString("error.geen.standplaats.periodes.verzetten"));
				}
				else
				{
					dialog.openWith(target, new MammaBulkVerzettenPopup(IDialog.CONTENT_ID, afspraken, screeningsEenheidModel, datumModel,
						ModelUtil.sModel(baseStandplaatsPeriodeDao.getStandplaatsPeriode(screeningsEenheidModel.getObject(), datum)), standplaatsPeriodesVoorBulkVerzetten)
					{
						@Override
						protected void verzettenAfgerond(AjaxRequestTarget target)
						{
							success(getString("afspraken.verzet"));
							dialog.close(target);
							refreshKalender(target);
						}
					});
				}
			}
		};
		bulkVerzetten.setVisible(magNuBulkVerzetten);
		bulkVerzetten.setOutputMarkupPlaceholderTag(true);
		bulkVerzetten.setOutputMarkupId(true);
		addOrReplace(bulkVerzetten);

		if (target != null)
		{
			target.add(form);
			target.add(volledigDagLabel);
			target.add(metaInfo);
			target.add(bulkVerzetten);
		}
	}

	private void addMetaInfo()
	{
		MammaScreeningsEenheid screeningsEenheid = screeningsEenheidModel.getObject();
		Date datum = datumModel.getObject();

		MammaStandplaatsPeriode standplaatsPeriode = baseStandplaatsPeriodeDao.getStandplaatsPeriode(screeningsEenheid, datum);
		String standplaatsPeriodeTekst = "Geen";
		SimpleDateFormat format = new SimpleDateFormat("dd-MM-yyyy");
		MammaStandplaats standplaats = null;
		if (standplaatsPeriode != null)
		{
			standplaats = standplaatsPeriode.getStandplaatsRonde().getStandplaats();
			standplaatsPeriodeTekst = StandplaatsPeriodeUtil.getStandplaatsPeriodeNaam(standplaatsPeriode) + " (" + format.format(standplaatsPeriode.getVanaf()) + " - "
				+ format.format(standplaatsPeriode.getTotEnMet())
				+ ")";
		}
		metaInfo.addOrReplace(new Label("standplaatsPeriodeTekst", standplaatsPeriodeTekst));

		List<String> blokkadesStringList = new ArrayList<>();
		for (MammaBlokkade blokkade : baseBlokkadeDao.getActieveBlokkadesVoorSE(standplaats, screeningsEenheid, datum))
		{
			String blokkadeString = "";
			switch (blokkade.getType())
			{
			case SCREENINGS_EENHEID:
				blokkadeString = "SE";
				break;
			case SCREENINGS_ORGANISATIE:
				blokkadeString = "SO";
				break;
			case STANDPLAATS:
				blokkadeString = "Standplaats";
				break;
			}
			blokkadeString += " (" + format.format(blokkade.getVanaf()) + " - " + format.format(blokkade.getTotEnMet()) + ")";
			blokkadesStringList.add(blokkadeString);
		}
		Label blokkadesLabel = new Label("blokkades", StringUtils.join(blokkadesStringList, ", "));
		add(blokkadesLabel);
		if (!blokkadesStringList.isEmpty())
		{
			blokkadesLabel.add(new AttributeAppender("class", Model.of("blokkades")));
		}
		else
		{
			blokkadesLabel.setDefaultModelObject("Geen");
		}
		metaInfo.addOrReplace(blokkadesLabel);
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
		return MammaAfsprakenSEZoekenPage.class;
	}

}
