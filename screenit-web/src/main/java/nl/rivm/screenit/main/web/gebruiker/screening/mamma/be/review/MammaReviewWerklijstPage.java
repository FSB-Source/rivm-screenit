package nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.review;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.main.model.mamma.beoordeling.MammaConclusieReviewZoekObject;
import nl.rivm.screenit.main.service.mamma.MammaConclusieReviewService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.table.ClientColumn;
import nl.rivm.screenit.main.web.component.table.EnumPropertyColumn;
import nl.rivm.screenit.main.web.component.table.GeboortedatumColumn;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.AbstractMammaBePage;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.MammaConclusieReviewFilterOptie;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import nl.topicuszorg.wicket.search.column.DateTimePropertyColumn;

import org.apache.wicket.Component;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormChoiceComponentUpdatingBehavior;
import org.apache.wicket.ajax.form.AjaxFormComponentUpdatingBehavior;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.form.CheckBox;
import org.apache.wicket.markup.html.form.EnumChoiceRenderer;
import org.apache.wicket.markup.html.form.RadioChoice;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.shiro.ShiroConstraint;

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

	private IModel<MammaConclusieReviewZoekObject> zoekObjectModel;

	private final WebMarkupContainer filterContainer;

	private final WebMarkupContainer tabelContainer;

	private RadioChoice<MammaConclusieReviewFilterOptie> conclusieOptieFilter;

	public MammaReviewWerklijstPage()
	{
		tabelContainer = new WebMarkupContainer("refreshContainer");
		tabelContainer.setOutputMarkupId(Boolean.TRUE);
		add(tabelContainer);

		createZoekObject();

		filterContainer = new WebMarkupContainer("filterContainer", zoekObjectModel);
		tabelContainer.add(filterContainer);

		createConclusieFilter();
		createGezienCheckboxFilter();
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();

		MammaReviewDataProvider onderzoekDataProvider = new MammaReviewDataProvider(zoekObjectModel);

		List<IColumn<MammaScreeningRonde, String>> columns = new ArrayList<>();
		columns.add(new DateTimePropertyColumn<>(Model.of("Onderzoeksdatum"), "laatsteOnderzoek.creatieDatum", Constants.getDateTimeSecondsFormat()));
		columns.add(new ClientColumn<>("persoon.achternaam", "dossier.client"));
		columns.add(new GeboortedatumColumn<>("persoon.geboortedatum", "dossier.client.persoon"));
		columns.add(new PropertyColumn<>(Model.of("BSN"), "persoon.bsn", "dossier.client.persoon.bsn"));
		columns.add(new EnumPropertyColumn<>(Model.of("Conclusie"), "followUpConclusieStatus", "followUpConclusieStatus"));
		columns.add(new PropertyColumn<>(Model.of("Gereviewd op"), "conclusieReviews")
		{
			@Override
			public IModel<String> getDataModel(IModel<MammaScreeningRonde> screeningRondeModel)
			{
				LocalDateTime reviewMoment = conclusieReviewService.getReviewMoment(screeningRondeModel.getObject(), ScreenitSession.get().getLoggedInInstellingGebruiker());
				return reviewMoment != null ? Model.of(reviewMoment.format(DateUtil.LOCAL_DATE_TIME_FORMAT_SECONDS)) : null;
			}
		});

		tabelContainer.add(new ScreenitDataTable<>("resultaten", columns, onderzoekDataProvider, 10, Model.of("onderzoek(en)"))
		{
			@Override
			public void onClick(AjaxRequestTarget target, IModel<MammaScreeningRonde> model)
			{
				wijzigIDS7Role(conclusieReviewService.getMammobridgeRoleBijConclusieReviewFilter(zoekObjectModel.getObject().getFilterOptie()));

				List<Long> beoordelingIds = conclusieReviewService.zoekBeoordelingIdsMetConclusie(zoekObjectModel.getObject(), "laatsteOnderzoek.creatieDatum", true);

				if (ScreenitSession.get().checkPermission(Recht.GEBRUIKER_SCREENING_MAMMA_REVIEW_WERKLIJST, Actie.AANPASSEN) && isHeeftImsKoppelingRecht())
				{
					setResponsePage(new MammaConclusieReviewenPage(model.getObject().getLaatsteOnderzoek().getLaatsteBeoordeling().getId(),
						beoordelingIds, MammaReviewWerklijstPage.class));
				}
			}

		});
	}

	private void createConclusieFilter()
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
					Object displayValue = super.getDisplayValue(filterOptie);
					MammaConclusieReviewZoekObject zoekObject = new MammaConclusieReviewZoekObject();
					zoekObject.setGezienTonen(zoekObjectModel.getObject().getGezienTonen());
					zoekObject.setFilterOptie(filterOptie);
					zoekObject.setInstellingGebruiker(ScreenitSession.get().getLoggedInInstellingGebruiker());
					long aantalResultaten = conclusieReviewService.countScreeningRondesMetConclusie(zoekObject);
					displayValue = displayValue.toString() + " (" + aantalResultaten + ")";
					return displayValue;
				}
			});
		conclusieOptieFilter.setPrefix("<div class=\"span2\">\n" +
			"<div class=\"control-group\"><label class=\"radio\">");
		conclusieOptieFilter.setSuffix("</label></div>\n" +
			"</div>");

		conclusieOptieFilter.add(new AjaxFormChoiceComponentUpdatingBehavior()
		{
			@Override
			protected void onUpdate(AjaxRequestTarget ajaxRequestTarget)
			{
				ajaxRequestTarget.add(tabelContainer);
			}
		});
		filterContainer.add(conclusieOptieFilter);
	}

	private void createGezienCheckboxFilter()
	{
		Component gezienTonen = new CheckBox("gezienTonen");

		gezienTonen.add(new AjaxFormComponentUpdatingBehavior("click")
		{

			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				target.add(tabelContainer);
				target.add(conclusieOptieFilter);
			}
		});
		filterContainer.add(gezienTonen);
	}

	private void createZoekObject()
	{
		zoekObjectModel = new CompoundPropertyModel<>(new MammaConclusieReviewZoekObject());
		MammaConclusieReviewZoekObject zoekObject = zoekObjectModel.getObject();
		zoekObject.setGezienTonen(false);
		zoekObject.setInstellingGebruiker(ScreenitSession.get().getLoggedInInstellingGebruiker());
	}

	@Override
	public void detachModels()
	{
		super.detachModels();
		ModelUtil.nullSafeDetach(zoekObjectModel);
	}
}
