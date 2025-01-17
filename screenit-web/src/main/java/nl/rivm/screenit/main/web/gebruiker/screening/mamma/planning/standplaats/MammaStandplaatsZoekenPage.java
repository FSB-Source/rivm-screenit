package nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.standplaats;

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

import java.util.ArrayList;
import java.util.List;

import nl.rivm.screenit.main.service.mamma.MammaScreeningsEenheidService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.component.table.ActiefPropertyColumn;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.MammaPlanningBasePage;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.MammaStandplaats;
import nl.rivm.screenit.model.mamma.MammaStandplaatsLocatie;
import nl.rivm.screenit.service.InstellingService;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.commons.lang3.StringUtils;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.extensions.markup.html.repeater.data.grid.ICellPopulator;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.ChoiceRenderer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.INZIEN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = { Recht.GEBRUIKER_SCREENING_MAMMA_PLANNING },
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.MAMMA })
public class MammaStandplaatsZoekenPage extends MammaPlanningBasePage
{

	private static final long serialVersionUID = 1L;

	private Form<MammaStandplaats> zoekForm;

	@SpringBean
	private InstellingService instellingService;

	@SpringBean
	private MammaScreeningsEenheidService screeningsEenheidService;

	public MammaStandplaatsZoekenPage()
	{

		ScreeningOrganisatie ingelogdNamensRegio = ScreenitSession.get().getScreeningOrganisatie();
		IModel<MammaStandplaats> criteriaModel;
		if (ScreenitSession.get().isZoekObjectGezetForComponent(MammaStandplaatsZoekenPage.class))
		{
			criteriaModel = (IModel<MammaStandplaats>) ScreenitSession.get().getZoekObject(MammaStandplaatsZoekenPage.class);
		}
		else
		{
			MammaStandplaats zoekObject = new MammaStandplaats();
			criteriaModel = ModelUtil.cModel(zoekObject);
			zoekObject = criteriaModel.getObject();
			zoekObject.setLocatie(new MammaStandplaatsLocatie());

			zoekObject.setRegio(ingelogdNamensRegio);
			zoekObject.setActief(true);
		}

		MammaStandplaatsDataProvider standplaatsDataProvider = new MammaStandplaatsDataProvider("naam", criteriaModel);

		final WebMarkupContainer refreshContainer = new WebMarkupContainer("refreshContainer");
		refreshContainer.setOutputMarkupId(Boolean.TRUE);
		add(refreshContainer);

		List<IColumn<MammaStandplaats, String>> columns = new ArrayList<>();
		columns.add(new PropertyColumn<MammaStandplaats, String>(Model.of("SE"), "")
		{
			@Override
			public void populateItem(Item<ICellPopulator<MammaStandplaats>> item, String componentId, IModel<MammaStandplaats> rowModel)
			{
				item.add(new Label(componentId, screeningsEenheidService.getGekoppeldeScreeningsEenhedenTekst(rowModel.getObject())));
			}
		});
		columns.add(new PropertyColumn<MammaStandplaats, String>(Model.of("Naam"), "naam", "naam"));
		if (ingelogdNamensRegio == null)
		{
			columns.add(new PropertyColumn<MammaStandplaats, String>(Model.of("Screeningsorganisatie"), "regio.naam", "regio.naam"));
		}
		columns.add(new PropertyColumn<MammaStandplaats, String>(Model.of("Locatie adres"), "locatie.straat", "locatie.straat")
		{
			@Override
			public IModel<String> getDataModel(IModel<MammaStandplaats> rowModel)
			{
				String locatieAdres = "";
				if (rowModel.getObject().getLocatie() != null)
				{
					locatieAdres = rowModel.getObject().getLocatie().getAdres();
				}
				return Model.of(locatieAdres);
			}
		});
		columns.add(new PropertyColumn<MammaStandplaats, String>(Model.of("Locatie plaats"), "locatie.plaats", "locatie")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public IModel<String> getDataModel(IModel<MammaStandplaats> rowModel)
			{
				String postcode = rowModel.getObject().getLocatie().getPostcode();
				if (StringUtils.isBlank(postcode))
				{
					postcode = "";
				}
				String plaats = rowModel.getObject().getLocatie().getPlaats();
				if (StringUtils.isBlank(plaats))
				{
					plaats = "";
				}
				return Model.of(StringUtils.trim(postcode + "  " + plaats));
			}
		});

		columns.add(new ActiefPropertyColumn<MammaStandplaats, MammaStandplaats>(Model.of(""), "actief", refreshContainer, criteriaModel));

		refreshContainer.add(new ScreenitDataTable<MammaStandplaats, String>("resultaten", columns, standplaatsDataProvider, 10, Model.of("standplaats(en)"))
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target, IModel<MammaStandplaats> model)
			{
				MammaStandplaats standplaats = model.getObject();
				setResponsePage(new MammaStandplaatsEditPage(ModelUtil.cModel(standplaats)));
			}

		});

		AjaxLink<Void> toevoegen = new IndicatingAjaxLink<Void>("standplaatsToevoegen")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				MammaStandplaats standplaats = new MammaStandplaats();
				standplaats.setActief(true);
				standplaats.setRegio(ScreenitSession.get().getScreeningOrganisatie());
				setResponsePage(new MammaStandplaatsEditPage(ModelUtil.cModel(standplaats)));
			}
		};

		toevoegen.setVisible(ingelogdNamensRegio != null && ScreenitSession.get().checkPermission(Recht.GEBRUIKER_SCREENING_MAMMA_PLANNING, Actie.TOEVOEGEN));
		add(toevoegen);
		setDefaultModel(new CompoundPropertyModel<>(criteriaModel));
		zoekForm = new Form<MammaStandplaats>("zoekForm", (IModel<MammaStandplaats>) getDefaultModel());
		add(zoekForm);

		zoekForm.add(new TextField<>("naam"));
		zoekForm.add(new TextField<>("locatie.plaats"));
		ScreenitDropdown<ScreeningOrganisatie> regioComponent = new ScreenitDropdown<>("regio",
			ModelUtil.listRModel(instellingService.getActieveInstellingen(ScreeningOrganisatie.class), false), new ChoiceRenderer<ScreeningOrganisatie>("naam"));
		regioComponent.setVisible(ingelogdNamensRegio == null);
		regioComponent.setNullValid(true);
		zoekForm.add(regioComponent);
		IndicatingAjaxSubmitLink zoekenButton = new IndicatingAjaxSubmitLink("zoeken", zoekForm)
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				super.onSubmit(target);
				ScreenitSession.get().setZoekObject(MammaStandplaatsZoekenPage.class, zoekForm.getModel());
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
