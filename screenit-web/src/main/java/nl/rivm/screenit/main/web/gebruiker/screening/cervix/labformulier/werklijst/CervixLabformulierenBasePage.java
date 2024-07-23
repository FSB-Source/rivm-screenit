package nl.rivm.screenit.main.web.gebruiker.screening.cervix.labformulier.werklijst;

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
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ConfirmingIndicatingAjaxLink;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.component.table.EnumPropertyColumn;
import nl.rivm.screenit.main.web.component.table.GeboortedatumColumn;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.main.web.gebruiker.base.ZoekenContextMenuItem;
import nl.rivm.screenit.main.web.gebruiker.screening.cervix.CervixScreeningBasePage;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.GbaPersoon_;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.batch.BatchJob;
import nl.rivm.screenit.model.cervix.CervixLabformulier;
import nl.rivm.screenit.model.cervix.CervixLabformulierenFilter;
import nl.rivm.screenit.model.cervix.enums.CervixLabformulierStatus;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.JobStartParameter;
import nl.rivm.screenit.model.enums.JobType;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.JobService;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.util.NaamUtil;
import nl.topicuszorg.wicket.search.column.BooleanStringPropertyColumn;

import org.apache.wicket.MarkupContainer;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.markup.html.repeater.data.grid.ICellPopulator;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.EmptyPanel;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

import static nl.rivm.screenit.main.service.cervix.impl.CervixLabformulierDataProviderServiceImpl.CLIENT_PROPERTY;
import static nl.rivm.screenit.main.service.cervix.impl.CervixLabformulierDataProviderServiceImpl.PERSOON_PROPERTY;

@ZoekenContextMenuItem
public abstract class CervixLabformulierenBasePage extends CervixScreeningBasePage
{
	@SpringBean
	private LogService logService;

	@SpringBean
	private JobService jobService;

	private List<IColumn<CervixLabformulier, String>> columns;

	public CervixLabformulierenBasePage(CervixLabformulierStatus[] mogelijkeCervixLabformulierStatussen, CervixLabformulierenFilter filter,
		boolean labformulierStatussenVisible, boolean filterVisible, boolean ordersVerwerkenKnopVisible, boolean datumRangeVisible,
		boolean geboortedatumFilterVisible, boolean minimaalWaardesTekstVisible, boolean naHuisartsOnbekendVisible)
	{
		IModel<CervixLabformulierenFilter> labformulierFilterZoekObject = (IModel<CervixLabformulierenFilter>) ScreenitSession.get()
			.getZoekObject(CervixLabformulierenFilterPanel.class);
		if (labformulierFilterZoekObject != null)
		{
			filter = labformulierFilterZoekObject.getObject();
		}

		BootstrapDialog dialog = new BootstrapDialog("dialog");
		add(dialog);

		MarkupContainer ordersVerwerkenKnop = new ConfirmingIndicatingAjaxLink<JobType>("ordersVerwerken", null, dialog, "orders.verwerken")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				Instelling instelling = ScreenitSession.get().getInstelling();
				BatchJob batchJob = new BatchJob();
				batchJob.setJobType(JobType.CERVIX_ORDER);
				batchJob.getJobParameters().put(JobStartParameter.CERVIX_ORDER_LABORATORIUM.name(), instelling.getId());
				jobService.startJob(batchJob, ScreenitSession.get().getLoggedInInstellingGebruiker());
				info("De opdracht om orders te verwerken is ontvangen, deze zal z.s.m. worden uitgevoerd");
			}
		};
		ordersVerwerkenKnop.setVisible(ScreenitSession.get().checkPermission(Recht.GEBRUIKER_BMHK_LABORATORIUM_ORDER_VERWERKEN, Actie.AANPASSEN)
			&& OrganisatieType.BMHK_LABORATORIUM.equals(ScreenitSession.get().getInstelling().getOrganisatieType()) && ordersVerwerkenKnopVisible);
		add(ordersVerwerkenKnop);

		MarkupContainer labformulierenFilter = new CervixLabformulierenFilterPanel("filter", mogelijkeCervixLabformulierStatussen, filter, labformulierStatussenVisible,
			datumRangeVisible, geboortedatumFilterVisible, minimaalWaardesTekstVisible)
		{
			@Override
			protected WebMarkupContainer zoeken(CervixLabformulierenFilter filter)
			{
				return refreshTable(filter);
			}

		};
		labformulierenFilter.setVisible(filterVisible);
		add(labformulierenFilter);

		columns = new ArrayList<>();
		columns.add(new PropertyColumn<>(Model.of(getString("barcode")), "barcode", "barcode"));
		columns.add(
			new PropertyColumn<>(Model.of("Cliënt"), PERSOON_PROPERTY + "." + GbaPersoon_.ACHTERNAAM,
				CLIENT_PROPERTY)
		{
			@Override
			public IModel<Object> getDataModel(IModel<CervixLabformulier> labformulierModel)
			{
				Client client = new PropertyModel<Client>(labformulierModel, getPropertyExpression()).getObject();
				return new Model(client != null ? NaamUtil.titelVoorlettersTussenvoegselEnAanspreekAchternaam(client) : "");
			}
		});
		columns.add(new GeboortedatumColumn<>(PERSOON_PROPERTY + "." + GbaPersoon_.GEBOORTEDATUM,
			PERSOON_PROPERTY));
		columns.add(new PropertyColumn<>(Model.of("BSN"), PERSOON_PROPERTY + "." + GbaPersoon_.BSN,
			PERSOON_PROPERTY + "." + GbaPersoon_.BSN));
		columns.add(new PropertyColumn<>(Model.of("Scandatum"), "scanDatum", "scanDatum"));
		if (labformulierStatussenVisible)
		{
			columns.add(new EnumPropertyColumn<CervixLabformulier, String, CervixLabformulierStatus>(Model.of("Status formulier"), "status", "status"));
		}
		if (naHuisartsOnbekendVisible)
		{
			columns.add(new PropertyColumn<>(Model.of("Na huisarts onbekend"), "huisartsOnbekendBrief", "huisartsOnbekendBrief")
			{
				@Override
				public void populateItem(Item<ICellPopulator<CervixLabformulier>> item, String componentId, IModel<CervixLabformulier> rowModel)
				{
					item.add(new Label(componentId, rowModel.getObject().getHuisartsOnbekendBrief() == null ? "Nee" : "Ja"));
				}
			});
		}
		Map<Boolean, String> booleanStringMap = new HashMap<>();
		booleanStringMap.put(true, "Ja");
		booleanStringMap.put(false, "Nee");
		columns.add(new BooleanStringPropertyColumn<>(Model.of("Digitaal"), booleanStringMap, "digitaal", "digitaal"));

		refreshTable(filter);

	}

	private WebMarkupContainer refreshTable(CervixLabformulierenFilter filter)
	{
		if (showEmptyTable(filter))
		{
			EmptyPanel labformulieren = new EmptyPanel("labformulieren");
			labformulieren.setOutputMarkupPlaceholderTag(true);
			addOrReplace(labformulieren);
			return labformulieren;
		}
		else
		{
			var labformulierProvider = new CervixLabformulierProvider(filter);

			ScreenitDataTable<CervixLabformulier, String> labformulieren = new ScreenitDataTable<>("labformulieren", columns, labformulierProvider, 10,
				Model.of("labformulieren"))
			{
				@Override
				public void onClick(AjaxRequestTarget target, IModel<CervixLabformulier> labformulierModel)
				{
					controlePage(labformulierProvider.getLabformulierenIds(), labformulierModel.getObject());
				}

				@Override
				protected boolean isRowClickable(IModel<CervixLabformulier> rowModel)
				{
					return isTableClickable();
				}
			};
			addOrReplace(labformulieren);

			logService.logGebeurtenis(LogGebeurtenis.CERVIX_LABFORMULIEREN_INGEZIEN, ScreenitSession.get().getLoggedInAccount(), getString("titel") + " - " + filter.toString(),
				Bevolkingsonderzoek.CERVIX);

			return labformulieren;
		}
	}

	protected boolean isTableClickable()
	{
		return true;
	}

	protected abstract void controlePage(List<Long> labformulierenIds, CervixLabformulier object);

	protected abstract boolean showEmptyTable(CervixLabformulierenFilter filter);
}
