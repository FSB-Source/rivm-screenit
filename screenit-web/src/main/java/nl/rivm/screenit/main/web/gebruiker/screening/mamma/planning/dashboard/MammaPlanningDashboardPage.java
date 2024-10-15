package nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.dashboard;

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

import java.util.ArrayList;
import java.util.List;

import nl.rivm.screenit.dto.mamma.planning.PlanningConceptMeldingenDto;
import nl.rivm.screenit.dto.mamma.planning.PlanningScreeningsEenheidMetaDataDto;
import nl.rivm.screenit.main.service.mamma.MammaScreeningsEenheidService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.component.modal.IDialog;
import nl.rivm.screenit.main.web.component.table.ExportToXslLink;
import nl.rivm.screenit.main.web.component.table.ImageIconCellPanel;
import nl.rivm.screenit.main.web.component.table.NotClickablePropertyColumn;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.MammaPlanningBasePage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.MammaScreeningsEenheidFilter;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.MammaScreeningsEenheidFilterPanel;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.route.MammaRouteConceptAnnulerenMeldingenDialogPanel;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.route.MammaRouteConceptWijzigingMeldingenDialogPanel;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.screeningseenheid.MammaSEDataProvider;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.Instelling_;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid_;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.mamma.MammaBaseConceptPlanningsApplicatie;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.organisatie.model.Organisatie_;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.behavior.AttributeAppender;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.extensions.markup.html.repeater.data.grid.ICellPopulator;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.shiro.ShiroConstraint;

import static nl.rivm.screenit.util.StringUtil.propertyChain;

@SecurityConstraint(
	actie = Actie.INZIEN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = { Recht.GEBRUIKER_SCREENING_MAMMA_PLANNING },
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.MAMMA })
public class MammaPlanningDashboardPage extends MammaPlanningBasePage
{
	private final List<IColumn<MammaScreeningsEenheid, String>> columns = new ArrayList<>();

	private final BootstrapDialog dialog;

	private ScreenitDataTable<MammaScreeningsEenheid, String> screeningsEenheden;

	@SpringBean
	private ICurrentDateSupplier currentDateSupplier;

	@SpringBean
	private MammaBaseConceptPlanningsApplicatie baseConceptPlanningsApplicatie;

	@SpringBean
	private MammaScreeningsEenheidService screeningsEenheidService;

	private MammaScreeningsEenheidFilterPanel filter;

	private final int SCREENINGS_EENHEDEN_COUNT = 9999;

	public MammaPlanningDashboardPage()
	{
		filter = new MammaScreeningsEenheidFilterPanel("filter")
		{
			@Override
			protected WebMarkupContainer zoeken(IModel<MammaScreeningsEenheidFilter> filter)
			{
				return refreshTable(filter);
			}
		};
		add(filter);

		addConceptButtons();

		if (ScreenitSession.get().getScreeningOrganisatie() == null)
		{
			columns.add(new NotClickablePropertyColumn<>(Model.of("Screeningsorganisatie"),
				propertyChain(MammaScreeningsEenheid_.BEOORDELINGS_EENHEID, Instelling_.PARENT, Instelling_.REGIO, Organisatie_.NAAM), "beoordelingsEenheid.parent.regio.naam"));
		}
		columns.add(new NotClickablePropertyColumn<>(Model.of("Screeningseenheid"), MammaScreeningsEenheid_.NAAM, "naam"));
		columns.add(new NotClickablePropertyColumn<>(Model.of(""), "")
		{
			@Override
			public void populateItem(Item<ICellPopulator<MammaScreeningsEenheid>> cell, String id, IModel<MammaScreeningsEenheid> model)
			{
				cell.add(new MammaPlanningDashboardNavigatiePanel(id, model));
			}
		});
		columns.add(new PropertyColumn<>(Model.of("Uitgenodigd tot en met"), MammaScreeningsEenheid_.UITGENODIGD_TOT_EN_MET, "uitgenodigdTotEnMet"));
		columns.add(new PropertyColumn<>(Model.of("Uitnodigen tot en met"), MammaScreeningsEenheid_.UITNODIGEN_TOT_EN_MET, "uitnodigenTotEnMet")
		{
			@Override
			public void populateItem(Item<ICellPopulator<MammaScreeningsEenheid>> cell, String id, IModel<MammaScreeningsEenheid> model)
			{
				super.populateItem(cell, id, model);
				MammaScreeningsEenheid screeningsEenheid = model.getObject();
				if (screeningsEenheid.getUitnodigenTotEnMet() != null && !screeningsEenheid.getUitnodigenTotEnMet().equals(screeningsEenheid.getUitgenodigdTotEnMet()))
				{
					cell.add(new AttributeAppender("class", Model.of("font-bold")));
				}
			}
		});
		columns.add(new PropertyColumn<>(Model.of("Vrijgegeven tot en met"), MammaScreeningsEenheid_.VRIJGEGEVEN_TOT_EN_MET, "vrijgegevenTotEnMet"));
		columns.add(new NotClickablePropertyColumn<>(Model.of("Interval"), "")
		{
			@Override
			public void populateItem(Item<ICellPopulator<MammaScreeningsEenheid>> cell, String id, IModel<MammaScreeningsEenheid> model)
			{
				cell.add(new MammaPlanningDashboardIntervalPanel(id, model));
			}
		});
		columns.add(new NotClickablePropertyColumn<>(Model.of("Indicatie"), "")
		{
			@Override
			public void populateItem(Item<ICellPopulator<MammaScreeningsEenheid>> cell, String id, IModel<MammaScreeningsEenheid> model)
			{
				PlanningScreeningsEenheidMetaDataDto metaDataDto = model.getObject().getMetaDataDto();
				cell.add(new ImageIconCellPanel<>(id, model, "icon-warning-sign " + metaDataDto.niveau.getCssClass(), null));
			}
		});

		ExportToXslLink exportLink = new ExportToXslLink("export", "Overzicht_planning_dashboard_" + currentDateSupplier.getLocalDateTime().format(DateUtil.LOCAL_DATE_TIME_FORMAT))
		{
			@Override
			protected String getCsv() throws NullPointerException
			{
				return screeningsEenheidService.getCsvString(screeningsEenheden.getDataProvider().iterator(0, SCREENINGS_EENHEDEN_COUNT));
			}
		};
		add(exportLink);

		refreshTable(filter.getModel());

		dialog = new BootstrapDialog("dialog");
		dialog.setOutputMarkupId(true);
		add(dialog);
	}

	private void addConceptButtons()
	{
		add(new IndicatingAjaxLink<Void>("conceptOpslaan")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				PlanningConceptMeldingenDto meldingenDto = baseConceptPlanningsApplicatie.saveConcept(ScreenitSession.get().getLoggedInInstellingGebruiker(), true);
				dialog.openWith(target, new MammaRouteConceptWijzigingMeldingenDialogPanel(IDialog.CONTENT_ID, Model.of(meldingenDto))
				{
					@Override
					protected void close(AjaxRequestTarget target)
					{
						target.add(refreshTable(filter.getModel()));
						dialog.close(target);
					}
				});
			}

		}.setVisible(magAanpassen));

		add(new IndicatingAjaxLink<Void>("conceptAnnuleren")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				PlanningConceptMeldingenDto meldingenDto = baseConceptPlanningsApplicatie.saveConcept(ScreenitSession.get().getLoggedInInstellingGebruiker(), true);
				dialog.openWith(target, new MammaRouteConceptAnnulerenMeldingenDialogPanel(IDialog.CONTENT_ID, Model.of(meldingenDto))
				{
					@Override
					protected void close(AjaxRequestTarget target)
					{
						target.add(refreshTable(filter.getModel()));
						dialog.close(target);
					}

				});
			}

		}.setVisible(magAanpassen));
	}

	private WebMarkupContainer refreshTable(IModel<MammaScreeningsEenheidFilter> filterForProvide)
	{
		screeningsEenheden = new ScreenitDataTable<>("screeningsEenheden", columns,
			new MammaSEDataProvider(filterForProvide, true), SCREENINGS_EENHEDEN_COUNT, Model.of("screeningseenheden"))
		{
			@Override
			public void onClick(AjaxRequestTarget target, IModel<MammaScreeningsEenheid> model)
			{
				super.onClick(target, model);
				dialog.openWith(target, new MammaCapaciteitUitnodigenPanel(IDialog.CONTENT_ID, ModelUtil.cModel(model.getObject()))
				{
					@Override
					protected void close(AjaxRequestTarget target)
					{
						target.add(refreshTable(filter.getModel()));
						dialog.close(target);
					}
				});
			}
		};
		addOrReplace(screeningsEenheden);
		return screeningsEenheden;
	}

	@Override
	protected boolean bevatFormulieren()
	{
		return Boolean.FALSE;
	}
}
