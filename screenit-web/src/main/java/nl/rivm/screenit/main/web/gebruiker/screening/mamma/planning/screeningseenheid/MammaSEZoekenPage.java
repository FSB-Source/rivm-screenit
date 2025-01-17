package nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.screeningseenheid;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.util.ArrayList;
import java.util.List;

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.component.table.ActiefPropertyColumn;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.MammaPlanningBasePage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.MammaScreeningsEenheidFilter;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.Instelling_;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid_;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.InstellingService;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.form.ChoiceRenderer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.shiro.ShiroConstraint;

import static nl.rivm.screenit.util.StringUtil.propertyChain;

@SecurityConstraint(
	actie = Actie.INZIEN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = { Recht.GEBRUIKER_SCREENING_MAMMA_SE_BEHEER },
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.MAMMA })
public class MammaSEZoekenPage extends MammaPlanningBasePage
{
	private final Form<MammaScreeningsEenheidFilter> zoekForm;

	@SpringBean
	private InstellingService instellingService;

	@SpringBean
	private ICurrentDateSupplier dateSupplier;

	public MammaSEZoekenPage()
	{
		IModel<MammaScreeningsEenheidFilter> criteriaModel;
		ScreeningOrganisatie ingelogdNamensRegio = ScreenitSession.get().getScreeningOrganisatie();

		if (ScreenitSession.get().isZoekObjectGezetForComponent(MammaSEZoekenPage.class))
		{
			criteriaModel = (IModel<MammaScreeningsEenheidFilter>) ScreenitSession.get().getZoekObject(MammaSEZoekenPage.class);
		}
		else
		{
			MammaScreeningsEenheidFilter zoekObject = new MammaScreeningsEenheidFilter();
			zoekObject.setRegio(ingelogdNamensRegio);
			zoekObject.setActief(true);
			criteriaModel = new CompoundPropertyModel<>(zoekObject);
		}
		MammaSEDataProvider seDataProvider = new MammaSEDataProvider(criteriaModel);

		final WebMarkupContainer refreshContainer = new WebMarkupContainer("refreshContainer");
		refreshContainer.setOutputMarkupId(Boolean.TRUE);
		add(refreshContainer);

		List<IColumn<MammaScreeningsEenheid, String>> columns = new ArrayList<>();
		columns.add(new PropertyColumn<>(Model.of("Code"), MammaScreeningsEenheid_.CODE, "code"));
		columns.add(new PropertyColumn<>(Model.of("Naam"), MammaScreeningsEenheid_.NAAM, "naam"));
		columns.add(
			new PropertyColumn<>(Model.of("Beoordelingseenheid"), propertyChain(MammaScreeningsEenheid_.BEOORDELINGS_EENHEID, Instelling_.NAAM), "beoordelingsEenheid.naam"));
		columns.add(new PropertyColumn<>(Model.of("Centrale eenheid"), propertyChain(MammaScreeningsEenheid_.BEOORDELINGS_EENHEID, Instelling_.PARENT, Instelling_.NAAM),
			"beoordelingsEenheid.parent.naam"));
		if (ingelogdNamensRegio == null)
		{
			columns.add(new PropertyColumn<>(Model.of("Screeningsorganisatie"),
				propertyChain(MammaScreeningsEenheid_.BEOORDELINGS_EENHEID, Instelling_.PARENT, Instelling_.REGIO, Instelling_.NAAM), "beoordelingsEenheid.parent.regio.naam"));
		}

		columns.add(new ActiefPropertyColumn<>(Model.of(""), "actief", refreshContainer, criteriaModel));

		refreshContainer.add(new ScreenitDataTable<>("resultaten", columns, seDataProvider, 20, Model.of("screeningseenheden"))
		{
			@Override
			public void onClick(AjaxRequestTarget target, IModel<MammaScreeningsEenheid> model)
			{
				MammaScreeningsEenheid screeningsEenheid = model.getObject();
				setResponsePage(new MammaSEEditPage(ModelUtil.ccModel(screeningsEenheid)));
			}
		});

		AjaxLink<Void> toevoegen = new IndicatingAjaxLink<>("seToevoegen")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				MammaScreeningsEenheid screeningsEenheid = new MammaScreeningsEenheid();
				IModel<MammaScreeningsEenheid> model = ModelUtil.ccModel(screeningsEenheid);
				screeningsEenheid = model.getObject();
				screeningsEenheid.setIsMobiel(true);
				screeningsEenheid.setHeeftLift(true);
				screeningsEenheid.setActief(true);
				screeningsEenheid.setTomosyntheseMogelijk(false);
				screeningsEenheid.setHerhalingsWeek(DateUtil.toUtilDate(dateSupplier.getLocalDate().with(DayOfWeek.MONDAY)));
				setResponsePage(new MammaSEEditPage(model));
			}
		};

		toevoegen.setVisible(ingelogdNamensRegio != null && ScreenitSession.get().checkPermission(Recht.GEBRUIKER_SCREENING_MAMMA_PLANNING, Actie.TOEVOEGEN));
		add(toevoegen);
		setDefaultModel(new CompoundPropertyModel<>(criteriaModel));
		zoekForm = new Form<>("zoekForm", (IModel<MammaScreeningsEenheidFilter>) getDefaultModel());
		add(zoekForm);

		zoekForm.add(new TextField<>("screeningsEenheid.code"));
		zoekForm.add(new TextField<>("screeningsEenheid.naam"));
		ScreenitDropdown<ScreeningOrganisatie> regioComponent = new ScreenitDropdown<>("regio",
			ModelUtil.listRModel(instellingService.getActieveInstellingen(ScreeningOrganisatie.class), false),
			new ChoiceRenderer<>("naam"));
		regioComponent.setVisible(ingelogdNamensRegio == null);
		regioComponent.setNullValid(true);
		zoekForm.add(regioComponent);
		IndicatingAjaxSubmitLink zoekenButton = new IndicatingAjaxSubmitLink("zoeken", zoekForm)
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				super.onSubmit(target);
				ScreenitSession.get().setZoekObject(MammaSEZoekenPage.class, zoekForm.getModel());
				target.add(refreshContainer);
			}
		};
		zoekForm.setDefaultButton(zoekenButton);
		zoekForm.add(zoekenButton);
	}

	@Override
	protected boolean bevatFormulieren()
	{
		return Boolean.FALSE;
	}
}
