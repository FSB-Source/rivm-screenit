package nl.rivm.screenit.main.web.gebruiker.screening.cervix.facturatie;

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

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.main.service.cervix.CervixBetalingService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.SimpleStringResourceModel;
import nl.rivm.screenit.main.web.component.modal.ConfirmPanel;
import nl.rivm.screenit.main.web.component.modal.DefaultConfirmCallback;
import nl.rivm.screenit.main.web.component.modal.IDialog;
import nl.rivm.screenit.main.web.component.table.AjaxImageCellPanel;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerMenuItem;
import nl.rivm.screenit.main.web.gebruiker.screening.cervix.CervixScreeningBasePage;
import nl.rivm.screenit.model.Instelling_;
import nl.rivm.screenit.model.cervix.facturatie.CervixBetaalopdracht;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.BestandStatus;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.DistributedLockService;
import nl.rivm.screenit.service.LogService;
import nl.topicuszorg.wicket.search.column.DateTimePropertyColumn;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.markup.html.repeater.data.grid.ICellPopulator;
import org.apache.wicket.extensions.markup.html.repeater.data.table.AbstractColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.EnumLabel;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;

import static nl.rivm.screenit.model.cervix.facturatie.CervixBetaalopdracht_.BETALINGSKENMERK;
import static nl.rivm.screenit.model.cervix.facturatie.CervixBetaalopdracht_.HASHTOTAAL;
import static nl.rivm.screenit.model.cervix.facturatie.CervixBetaalopdracht_.OMSCHRIJVING;
import static nl.rivm.screenit.model.cervix.facturatie.CervixBetaalopdracht_.SCREENING_ORGANISATIE;
import static nl.rivm.screenit.model.cervix.facturatie.CervixBetaalopdracht_.SEPA_DOCUMENT;
import static nl.rivm.screenit.model.cervix.facturatie.CervixBetaalopdracht_.STATUS_DATUM;

public class CervixBetalingSepaBestandenPage extends CervixScreeningBasePage
{
	@SpringBean
	private CervixBetalingService betalingService;

	@SpringBean
	private LogService logService;

	@SpringBean
	private DistributedLockService lockService;

	private WebMarkupContainer betalingOpdrachtenContainer;

	public CervixBetalingSepaBestandenPage()
	{
		betalingOpdrachtenContainer = getCervixBetalingopdrachtTabelContainer();
		add(betalingOpdrachtenContainer);
	}

	private WebMarkupContainer getCervixBetalingopdrachtTabelContainer()
	{
		var container = new WebMarkupContainer("sepaBestandTabelContainer");
		container.setOutputMarkupId(true);

		var betaalOpdrachtColumns = new ArrayList<IColumn<CervixBetaalopdracht, String>>();
		betaalOpdrachtColumns.add(new DateTimePropertyColumn<>(Model.of("Status datum"), STATUS_DATUM, STATUS_DATUM, Constants.getDateFormat()));
		betaalOpdrachtColumns.add(new PropertyColumn<>(Model.of("Betalingskenmerk"), BETALINGSKENMERK, BETALINGSKENMERK));
		betaalOpdrachtColumns.add(new PropertyColumn<>(Model.of("Omschrijving"), OMSCHRIJVING, OMSCHRIJVING));
		betaalOpdrachtColumns.add(
			new PropertyColumn<>(Model.of("Screeningorganisatie"), SCREENING_ORGANISATIE + "." + Instelling_.NAAM, SCREENING_ORGANISATIE + "." + Instelling_.NAAM));
		betaalOpdrachtColumns.add(new AbstractColumn<>(Model.of("Status"))
		{

			@Override
			public void populateItem(Item<ICellPopulator<CervixBetaalopdracht>> cellItem, String componentId, IModel<CervixBetaalopdracht> rowModel)
			{
				var status = rowModel.getObject().getStatus();
				if (BestandStatus.CRASH.equals(status))
				{
					cellItem.add(new AjaxImageCellPanel<>(componentId, rowModel, "icon-refresh")
					{
						@Override
						protected void onClick(AjaxRequestTarget target)
						{
							if (lockService.verkrijgLockIndienBeschikbaar(Constants.BMHK_BETALING_GENEREREN_LOCKNAAM))
							{
								try
								{
									betalingService.opslaanBetaalopdracht(rowModel.getObject(), ScreenitSession.get().getLoggedInInstellingGebruiker());
									betalingService.genereerCervixBetalingsSpecificatieEnSepaBestand(rowModel.getObject().getId());

									var container = getCervixBetalingopdrachtTabelContainer();
									betalingOpdrachtenContainer.replaceWith(container);
									betalingOpdrachtenContainer = container;
									target.add(betalingOpdrachtenContainer);

									info("Bestanden worden opnieuw gegenereerd.");
								}
								catch (RuntimeException rte)
								{
									lockService.unlock(Constants.BMHK_BETALING_GENEREREN_LOCKNAAM);
									throw rte;
								}
							}
							else
							{
								info(getString("info.bmhk.sepa.genereren.al.in.gebruik"));
							}
						}
					});
				}
				else
				{
					cellItem.add(new EnumLabel<>(componentId, status));
				}
			}
		});
		betaalOpdrachtColumns.add(new CervixBetalingSepaUploadDocumentColumn<>(Model.of("SEPA specificatie"), "sepaSpecificatiePdf")
		{
			@Override
			protected void loggingBijOnClick(IModel<CervixBetaalopdracht> rowModel)
			{
				var melding = "Betalingskenmerk: " + rowModel.getObject().getBetalingskenmerk();
				logService.logGebeurtenis(LogGebeurtenis.CERVIX_SEPA_SPECIFICATIE_DOCUMENT_GEDOWNLOAD, ScreenitSession.get().getLoggedInAccount(), melding,
					Bevolkingsonderzoek.CERVIX);
			}

			@Override
			protected boolean isEnabled(IModel<CervixBetaalopdracht> rowModel)
			{
				return !BestandStatus.CRASH.equals(rowModel.getObject().getStatus());
			}
		});
		betaalOpdrachtColumns.add(new PropertyColumn<>(Model.of("Controlegetal (SHA-256)"), HASHTOTAAL, HASHTOTAAL));
		betaalOpdrachtColumns.add(new CervixBetalingSepaUploadDocumentColumn<>(Model.of("SEPA document"), SEPA_DOCUMENT)
		{
			@Override
			protected void loggingBijOnClick(IModel<CervixBetaalopdracht> rowModel)
			{
				var melding = "Betalingskenmerk: " + rowModel.getObject().getBetalingskenmerk();
				logService.logGebeurtenis(LogGebeurtenis.CERVIX_SEPA_DOCUMENT_GEDOWNLOAD, ScreenitSession.get().getLoggedInAccount(), melding, Bevolkingsonderzoek.CERVIX);
			}

			@Override
			protected boolean isEnabled(IModel<CervixBetaalopdracht> rowModel)
			{
				return !BestandStatus.CRASH.equals(rowModel.getObject().getStatus());
			}
		});
		if (ScreenitSession.get().checkPermission(Recht.GEBRUIKER_SCREENING_BETALINGEN_BMHK, Actie.VERWIJDEREN))
		{
			betaalOpdrachtColumns.add(new AbstractColumn<>(Model.of("Verwijderen"))
			{

				@Override
				public void populateItem(Item<ICellPopulator<CervixBetaalopdracht>> cellItem, String componentId, final IModel<CervixBetaalopdracht> rowModel)
				{
					final AjaxImageCellPanel<CervixBetaalopdracht> imageCellPanel = new AjaxImageCellPanel<>(componentId, rowModel, "icon-trash")
					{

						@Override
						protected void onClick(AjaxRequestTarget target)
						{
							dialog.openWith(target,
								new ConfirmPanel(IDialog.CONTENT_ID, new SimpleStringResourceModel("betaalOpdrachtVerwijderen"), Model.of(getString("sepa.bestand.verwijderen")),
									new DefaultConfirmCallback()
									{

										private static final long serialVersionUID = 1L;

										@Override
										public void onYesClick(AjaxRequestTarget target)
										{
											verwijderBetaalOpdracht(getModel(), target);
										}

									}, dialog));

						}
					};
					imageCellPanel.setVisible(BestandStatus.VERWERKT.equals(rowModel.getObject().getStatus()));

					cellItem.add(imageCellPanel);
				}

				@Override
				public String getCssClass()
				{
					return "status";
				}

			});
		}
		var dataTable = new ScreenitDataTable<>("sepaBestandTable", betaalOpdrachtColumns,
			new CervixBetalingSepaBestandenDataProvider(), Model.of("Betalingsopdrachten"));

		container.add(dataTable);
		return container;
	}

	private void verwijderBetaalOpdracht(IModel<CervixBetaalopdracht> model, AjaxRequestTarget target)
	{
		try
		{
			betalingService.verwijderSepaBestanden(model.getObject());
		}
		catch (IllegalArgumentException e)
		{
			error(getString(e.getMessage()));
		}
		target.add(betalingOpdrachtenContainer);
		info("Betaalopdracht wordt verwijderd");
	}

	@Override
	protected List<GebruikerMenuItem> getContextMenuItems()
	{
		List<GebruikerMenuItem> contextMenuItems = new ArrayList<>();
		contextMenuItems.add(new GebruikerMenuItem("label.tab.cervixscreening.betalingen", CervixBetalingPage.class));
		contextMenuItems.add(new GebruikerMenuItem("label.tab.cervixscreening.betalingen.sepabestanden", CervixBetalingSepaBestandenPage.class));
		return contextMenuItems;
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
	}
}
