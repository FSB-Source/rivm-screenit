package nl.rivm.screenit.main.web.gebruiker.algemeen.parameterisatie;

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

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.List;

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.component.modal.ConfirmPanel;
import nl.rivm.screenit.main.web.component.modal.DefaultConfirmCallback;
import nl.rivm.screenit.main.web.component.price.BigDecimalPricePropertyColumn;
import nl.rivm.screenit.main.web.component.table.AjaxImageCellPanel;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.model.cervix.facturatie.CervixHuisartsTarief;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.enums.ToegangLevel;
import nl.rivm.screenit.service.AutorisatieService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.cervix.CervixVerrichtingService;
import nl.rivm.screenit.util.AutorisatieUtil;
import nl.topicuszorg.wicket.search.column.DateTimePropertyColumn;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.extensions.markup.html.repeater.data.grid.ICellPopulator;
import org.apache.wicket.extensions.markup.html.repeater.data.table.AbstractColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class CervixHuisartsTarievenPanel extends GenericPanel<CervixHuisartsTarief>
{

	@SpringBean
	private ICurrentDateSupplier currentDateSupplier;

	@SpringBean
	private CervixVerrichtingService cervixVerrichtingService;

	@SpringBean
	private AutorisatieService autorisatieService;

	private ToegangLevel level;

	private Actie actie;

	private final BootstrapDialog dialog;

	private WebMarkupContainer tableContainer;

	private SimpleDateFormat format = new SimpleDateFormat("dd-MM-yyyy");

	public CervixHuisartsTarievenPanel(String id)
	{
		super(id);

		actie = autorisatieService.getActieVoorMedewerker(ScreenitSession.get().getLoggedInInstellingGebruiker(), ScreenitSession.get().getCurrentSelectedMedewerker(),
			Recht.GEBRUIKER_CERVIX_HUISARTS_TARIEF);
		level = ScreenitSession.get().getToegangsLevel(Actie.INZIEN, Recht.GEBRUIKER_CERVIX_HUISARTS_TARIEF);
		boolean magToevoegen = AutorisatieUtil.isMinimumActie(actie, Actie.TOEVOEGEN);

		dialog = new BootstrapDialog("dialog");
		add(dialog);

		WebMarkupContainer toevoegenContainer = new WebMarkupContainer("toevoegenContainer");
		toevoegenContainer.setOutputMarkupId(true);
		add(toevoegenContainer);
		toevoegenContainer.add(new IndicatingAjaxLink<Void>("toevoegen")
		{

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				dialog.openWith(target, new CervixHuisartsTarievenPopupPanel(BootstrapDialog.CONTENT_ID)
				{
					@Override
					protected void opslaan(AjaxRequestTarget target)
					{
						info("Tarief succesvol opgeslagen");
						dialog.close(target);
						replaceContainer(target);
					}
				});
			}

		});

		toevoegenContainer.add(new IndicatingAjaxLink<Void>("indexeren")
		{

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				dialog.openWith(target, new CervixHuisartsIndexerenPopupPanel(BootstrapDialog.CONTENT_ID)
				{
					@Override
					protected void opslaan(AjaxRequestTarget target, String melding)
					{
						String infoMelding = "Tarief succesvol opgeslagen. ";
						if (!melding.isEmpty())
						{
							infoMelding += " Bijgewerkte bestaande tarieven: " + melding
								+ " Per bijgewerkte tarief start nu nog het bijwerken van de verrichtingen met het nieuwe tarief. "
								+ " Voor elk bijgewerkte tarief wordt een logregel gemaakt als het bijwerken van de verrichtingen voltooid is.";
						}
						info(infoMelding);
						dialog.close(target);
						replaceContainer(target);
					}
				});
			}

		});
		toevoegenContainer.setVisible(magToevoegen && ToegangLevel.LANDELIJK.equals(level));

		tableContainer = getTarievenContainer();
		add(tableContainer);

	}

	private WebMarkupContainer getTarievenContainer()
	{
		WebMarkupContainer container = new WebMarkupContainer("tarievenTableContainer");
		container.setOutputMarkupPlaceholderTag(true);

		List<IColumn<CervixHuisartsTarief, String>> columns = new ArrayList<IColumn<CervixHuisartsTarief, String>>();
		columns.add(new BigDecimalPricePropertyColumn<CervixHuisartsTarief, String>(Model.of("Tarief"), "tarief"));
		columns.add(new DateTimePropertyColumn<CervixHuisartsTarief, String>(Model.of("Geldig vanaf"), "geldigVanafDatum", "geldigVanafDatum", format));
		columns.add(new DateTimePropertyColumn<CervixHuisartsTarief, String>(Model.of("Geldig t/m"), "geldigTotenmetDatum", "geldigTotenmetDatum", format));
		if (Actie.VERWIJDEREN == actie)
		{
			columns.add(new AbstractColumn<CervixHuisartsTarief, String>(Model.of("Verwijderen"))
			{

				private static final long serialVersionUID = 1L;

				@Override
				public void populateItem(Item<ICellPopulator<CervixHuisartsTarief>> cellItem, String componentId, IModel<CervixHuisartsTarief> rowModel)
				{

					CervixHuisartsTarief tarief = rowModel.getObject();
					if (tarief.getGeldigVanafDatum().after(currentDateSupplier.getDate()))
					{
						cellItem.add(new AjaxImageCellPanel<CervixHuisartsTarief>(componentId, rowModel, "icon-trash")
						{
							@Override
							protected void onClick(AjaxRequestTarget target)
							{

								dialog.openWith(target,
									new ConfirmPanel(dialog.CONTENT_ID, Model.of("Weet u zeker dat u dit tarief wilt verwijderen?"), null, new DefaultConfirmCallback()
									{

										private static final long serialVersionUID = 1L;

										@Override
										public void onYesClick(AjaxRequestTarget target)
										{
											String verwijderdMelding = cervixVerrichtingService.getLogMeldingHuisartsTariefVerwijderd(rowModel.getObject());
											cervixVerrichtingService.verwijderCervixTarief(rowModel.getObject(), ScreenitSession.get().getLoggedInAccount(), verwijderdMelding);

											info("Huisartstarief succesvol verwijderd.");
											replaceContainer(target);

										}

										@Override
										public void onCloseClick(AjaxRequestTarget target)
										{
											dialog.close(target);
										}

										@Override
										public void onNoClick(AjaxRequestTarget target)
										{
											dialog.close(target);
										}

									}, dialog));
							}
						});
					}
					else
					{
						cellItem.add(new Label(componentId, Model.of("Geldig vanaf datum is al verstreken")));
					}
				}
			});
		}

		ScreenitDataTable<CervixHuisartsTarief, String> table = new ScreenitDataTable<CervixHuisartsTarief, String>("tarievenTable", columns,
			new CervixHuisartsTarievenDataProvider(), Model.of("Tarieven"));
		container.add(table);

		return container;
	}

	private void replaceContainer(AjaxRequestTarget target)
	{
		WebMarkupContainer container = getTarievenContainer();
		tableContainer.replaceWith(container);
		tableContainer = container;
		target.add(container);
	}

	protected boolean isMinimumActie(Actie actie, Actie minimaal)
	{
		return Actie.INZIEN.equals(minimaal) && actie == null || actie != null && actie.getNiveau() >= minimaal.getNiveau();
	}

}
