package nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.review;

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

import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.main.model.mamma.beoordeling.MammaConclusieReviewZoekObject;
import nl.rivm.screenit.main.service.MedewerkerService;
import nl.rivm.screenit.main.service.mamma.MammaConclusieReviewService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.IndicatingAjaxFormChoiceComponentUpdatingBehavior;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.component.table.ClientColumn;
import nl.rivm.screenit.main.web.component.table.EnumPropertyColumn;
import nl.rivm.screenit.main.web.component.table.GeboortedatumColumn;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.main.web.component.table.ScreenitDateTimePropertyColumn;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.AbstractMammaBePage;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.MammaConclusieReviewFilterOptie;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.MammaConclusieReview;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import nl.topicuszorg.wicket.input.behavior.IndicatingAjaxFormComponentUpdatingBehavior;
import nl.topicuszorg.wicket.search.column.DateTimePropertyColumn;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormComponentUpdatingBehavior;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.form.CheckBox;
import org.apache.wicket.markup.html.form.ChoiceRenderer;
import org.apache.wicket.markup.html.form.EnumChoiceRenderer;
import org.apache.wicket.markup.html.form.RadioChoice;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.jetbrains.annotations.NotNull;
import org.wicketstuff.shiro.ShiroConstraint;
import org.wicketstuff.wiquery.ui.datepicker.DatePicker;

@SecurityConstraint(
	actie = Actie.AANPASSEN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = { Recht.GEBRUIKER_SCREENING_MAMMA_REVIEW_WERKLIJST },
	organisatieTypeScopes = { OrganisatieType.BEOORDELINGSEENHEID },
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.MAMMA })
public class MammaReviewWerklijstPage extends AbstractMammaBePage
{

	@SpringBean
	private MammaConclusieReviewService conclusieReviewService;

	@SpringBean
	private MedewerkerService medewerkerService;

	private IModel<MammaConclusieReviewZoekObject> zoekObjectModel;

	private final WebMarkupContainer filterContainer;

	private final WebMarkupContainer tabelContainer;

	private WebMarkupContainer coordinerendRadioloogContainer;

	private WebMarkupContainer coordinerendRadioloogFilterContainer;

	private final boolean gebruikerIsCoordinerendRadioloog;

	private RadioChoice<MammaConclusieReviewFilterOptie> conclusieOptieFilter;

	private DatePicker<Date> filterDatumVanaf;

	public MammaReviewWerklijstPage()
	{
		gebruikerIsCoordinerendRadioloog = ScreenitSession.get().checkPermission(Recht.GEBRUIKER_BEOORDELINGSEENHEID_COORDINEREND_RADIOLOOG, Actie.INZIEN);

		tabelContainer = new WebMarkupContainer("refreshContainer");
		tabelContainer.setOutputMarkupId(Boolean.TRUE);
		add(tabelContainer);

		maakZoekObject();

		filterContainer = new WebMarkupContainer("filterContainer", zoekObjectModel);
		filterContainer.setOutputMarkupId(true);
		tabelContainer.add(filterContainer);

		maakCoordinerendRadioloogContainer();
		maakConclusieFilter();
		maakGezienCheckboxFilter();
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();

		maakResultatenTabel();
	}

	private void maakCoordinerendRadioloogContainer()
	{
		coordinerendRadioloogContainer = new WebMarkupContainer("coordinerendRadioloogContainer", zoekObjectModel);
		coordinerendRadioloogContainer.setVisible(gebruikerIsCoordinerendRadioloog);

		if (gebruikerIsCoordinerendRadioloog)
		{
			maakRadioloogKeuzeDropdown();
			maakCoordinerendRadioloogFilterContainer();
		}

		tabelContainer.add(coordinerendRadioloogContainer);
	}

	private void maakCoordinerendRadioloogFilterContainer()
	{
		coordinerendRadioloogFilterContainer = new WebMarkupContainer("coordinerendRadioloogFilterContainer");
		coordinerendRadioloogFilterContainer.setOutputMarkupId(true);
		coordinerendRadioloogFilterContainer.setVisible(false);
		coordinerendRadioloogContainer.add(coordinerendRadioloogFilterContainer);
		maakEindconclusieDatumFilter();
		maakGezienCoordinerendRadioloogCheckboxFilter();
	}

	private void maakResultatenTabel()
	{
		var onderzoekDataProvider = new MammaReviewDataProvider(zoekObjectModel);

		List<IColumn<MammaConclusieReview, String>> columns = new ArrayList<>();
		columns.add(new DateTimePropertyColumn<>(Model.of("Onderzoeksdatum"), "screeningRonde.laatsteOnderzoek.creatieDatum", Constants.getDateTimeSecondsFormat()));
		columns.add(new ClientColumn<>("persoon.achternaam", "screeningRonde.dossier.client"));
		columns.add(new GeboortedatumColumn<>("persoon.geboortedatum", "screeningRonde.dossier.client.persoon"));
		columns.add(new PropertyColumn<>(Model.of("BSN"), "persoon.bsn", "screeningRonde.dossier.client.persoon.bsn"));
		columns.add(new EnumPropertyColumn<>(Model.of("Conclusie"), "screeningRonde.followUpConclusieStatus", "screeningRonde.followUpConclusieStatus"));
		columns.add(new ScreenitDateTimePropertyColumn<>(Model.of("Gereviewd op"), "reviewMoment", Constants.getDateTimeSecondsFormat()));

		if (coordinerendRadioloogKijkBijAndereRadioloog())
		{
			columns.add(new PropertyColumn<>(Model.of("Gereviewd door CR"), "reviewMoment")
			{
				@Override
				public IModel<?> getDataModel(IModel<MammaConclusieReview> conclusieReviewModel)
				{
					return reviewMomentCoordinerendRadioloog(conclusieReviewModel);
				}
			});
		}

		var dataTable = new ScreenitDataTable<>("resultaten", columns, onderzoekDataProvider, 10, Model.of("onderzoek(en)"))
		{
			@Override
			public void onClick(AjaxRequestTarget target, IModel<MammaConclusieReview> model)
			{
				wijzigIDS7Role(conclusieReviewService.getMammobridgeRoleBijConclusieReviewFilter(zoekObjectModel.getObject().getFilterOptie()));

				List<Long> beoordelingIds = conclusieReviewService.zoekBeoordelingIdsMetConclusie(zoekObjectModel.getObject(), "laatsteOnderzoek.creatieDatum", true);

				if (ScreenitSession.get().checkPermission(Recht.GEBRUIKER_SCREENING_MAMMA_REVIEW_WERKLIJST, Actie.AANPASSEN) && isHeeftImsKoppelingRecht())
				{
					setResponsePage(new MammaConclusieReviewenPage(model.getObject().getScreeningRonde().getLaatsteOnderzoek().getLaatsteBeoordeling().getId(),
						beoordelingIds, MammaReviewWerklijstPage.class,
						ModelUtil.sModel(zoekObjectModel.getObject().getRadioloog())));
				}
			}

		};
		tabelContainer.addOrReplace(dataTable);
	}

	@NotNull
	private Model<String> reviewMomentCoordinerendRadioloog(IModel<MammaConclusieReview> conclusieReviewModel)
	{
		var screeningRonde = conclusieReviewModel.getObject().getScreeningRonde();
		var coordinerendRadioloog = ScreenitSession.get().getLoggedInInstellingGebruiker();
		var reviewMomentCoordinerendRadioloog = conclusieReviewService.getReviewAfgerondDoorCoordinerendRadioloog(coordinerendRadioloog, screeningRonde)
			.map(MammaConclusieReview::getReviewMoment)
			.map(c -> c.format(DateTimeFormatter.ofPattern(Constants.DEFAULT_DATE_TIME_SECONDS_FORMAT)))
			.orElse("");
		return Model.of(reviewMomentCoordinerendRadioloog);
	}

	private void maakConclusieFilter()
	{
		conclusieOptieFilter = new RadioChoice<>("filterOptie",
			Arrays.asList(MammaConclusieReviewFilterOptie.values()),
			new EnumChoiceRenderer<>(this)
			{
				@Override
				public Object getDisplayValue(MammaConclusieReviewFilterOptie filterOptie)
				{
					return getWeergaveFilterOptieMetAantallen(filterOptie);
				}

				private Object getWeergaveFilterOptieMetAantallen(MammaConclusieReviewFilterOptie filterOptie)
				{
					var displayValue = super.getDisplayValue(filterOptie);
					var nieuwZoekObjectVoorFilterKnoppen = new MammaConclusieReviewZoekObject();

					var zoekObject = zoekObjectModel.getObject();
					nieuwZoekObjectVoorFilterKnoppen.setGezienTonen(zoekObject.getGezienTonen());
					nieuwZoekObjectVoorFilterKnoppen.setGezienCoordinerendRadioloogTonen(zoekObject.isGezienCoordinerendRadioloogTonen());
					nieuwZoekObjectVoorFilterKnoppen.setZoekenVanafEindconclusieDatum(zoekObject.getZoekenVanafEindconclusieDatum());
					nieuwZoekObjectVoorFilterKnoppen.setFilterOptie(filterOptie);
					nieuwZoekObjectVoorFilterKnoppen.setRadioloog(zoekObject.getRadioloog());
					nieuwZoekObjectVoorFilterKnoppen.setIngelogdeGebruiker(zoekObject.getIngelogdeGebruiker());

					var aantalResultaten = conclusieReviewService.countConclusieReviewsVanRadioloog(nieuwZoekObjectVoorFilterKnoppen);
					displayValue = displayValue.toString() + " (" + aantalResultaten + ")";
					return displayValue;
				}
			});
		conclusieOptieFilter.setPrefix("<div class=\"span2\">\n" +
			"<div class=\"control-group\"><label class=\"radio\">");
		conclusieOptieFilter.setSuffix("</label></div>\n" +
			"</div>");

		conclusieOptieFilter.add(new IndicatingAjaxFormChoiceComponentUpdatingBehavior(conclusieOptieFilter)
		{
			@Override
			protected void onComponentUpdate(AjaxRequestTarget target)
			{
				target.add(tabelContainer);
			}
		});
		filterContainer.add(conclusieOptieFilter);
	}

	private void maakGezienCheckboxFilter()
	{
		var gezienTonen = new CheckBox("gezienTonen");

		gezienTonen.add(new IndicatingAjaxFormComponentUpdatingBehavior("click", gezienTonen)
		{
			@Override
			protected void onComponentUpdate(AjaxRequestTarget target)
			{
				refreshCoordinerendRadioloogFilter(target);
				target.add(tabelContainer);
				target.add(conclusieOptieFilter);
			}
		});
		filterContainer.add(gezienTonen);
	}

	private void maakGezienCoordinerendRadioloogCheckboxFilter()
	{
		var gezienCoordinerendRadiolooogTonen = new CheckBox("gezienCoordinerendRadioloogTonen");

		gezienCoordinerendRadiolooogTonen.add(new IndicatingAjaxFormComponentUpdatingBehavior("click", gezienCoordinerendRadiolooogTonen)
		{
			@Override
			protected void onComponentUpdate(AjaxRequestTarget target)
			{
				refreshReviewPagina(target);
			}
		});
		coordinerendRadioloogFilterContainer.add(gezienCoordinerendRadiolooogTonen);

	}

	private void maakZoekObject()
	{
		zoekObjectModel = new CompoundPropertyModel<>(new MammaConclusieReviewZoekObject());
		MammaConclusieReviewZoekObject zoekObject = zoekObjectModel.getObject();
		zoekObject.setGezienTonen(false);
		zoekObject.setRadioloog(ScreenitSession.get().getLoggedInInstellingGebruiker());
		zoekObject.setIngelogdeGebruiker(ScreenitSession.get().getLoggedInInstellingGebruiker());
	}

	private void maakRadioloogKeuzeDropdown()
	{
		var instellingGebruikerZoekObject = new InstellingGebruiker();
		instellingGebruikerZoekObject.setOrganisatie(ScreenitSession.get().getLoggedInInstellingGebruiker().getOrganisatie());

		var radiologenDropdown = new ScreenitDropdown<>("radioloog",
			ModelUtil.listModel(
				medewerkerService.getActieveRadiologen(instellingGebruikerZoekObject, new ArrayList<>(), "medewerker.gebruikersnaam", true)),
			new ChoiceRenderer<>("medewerker.gebruikersnaam"));

		radiologenDropdown.add(new AjaxFormComponentUpdatingBehavior("change")
		{
			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				refreshReviewPagina(target);
				refreshCoordinerendRadioloogFilter(target);
			}
		});

		coordinerendRadioloogContainer.add(radiologenDropdown);
	}

	private Date bepaalVooringevuldReviewZoekDatum(boolean coordinerendRadioloogKijkBijAndereRadioloog)
	{
		if (coordinerendRadioloogKijkBijAndereRadioloog)
		{
			return conclusieReviewService.bepaalInitieleConclusieReviewSorteerDatumCoordinerendRadioloog(zoekObjectModel.getObject());
		}
		return null;
	}

	private void maakEindconclusieDatumFilter()
	{
		filterDatumVanaf = ComponentHelper.newDatePicker("zoekenVanafEindconclusieDatum", true);
		filterDatumVanaf.setOutputMarkupId(true);
		filterDatumVanaf.add(new AjaxFormComponentUpdatingBehavior("change")
		{
			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				zoekObjectModel.getObject().setZoekenVanafEindconclusieDatum(filterDatumVanaf.getModelObject());

				refreshReviewPagina(target);
			}
		});
		coordinerendRadioloogFilterContainer.add(filterDatumVanaf);
	}

	private void refreshReviewPagina(AjaxRequestTarget target)
	{
		maakResultatenTabel();
		target.add(tabelContainer);
		target.add(filterContainer);
	}

	private void refreshCoordinerendRadioloogFilter(AjaxRequestTarget target)
	{
		if (gebruikerIsCoordinerendRadioloog)
		{
			var coordinerendRadioloogKijktBijAndereRadioloog = coordinerendRadioloogKijkBijAndereRadioloog();

			zoekObjectModel.getObject().setZoekenVanafEindconclusieDatum(bepaalVooringevuldReviewZoekDatum(coordinerendRadioloogKijktBijAndereRadioloog));

			coordinerendRadioloogFilterContainer.setVisible(coordinerendRadioloogKijktBijAndereRadioloog);

			target.add(coordinerendRadioloogFilterContainer);
		}
	}

	private boolean coordinerendRadioloogKijkBijAndereRadioloog()
	{
		return zoekObjectModel.getObject().getCoordinerendRadioloogKijktBijAndereRadioloog();
	}

	@Override
	public void detachModels()
	{
		super.detachModels();
		ModelUtil.nullSafeDetach(zoekObjectModel);
	}
}
