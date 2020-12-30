package nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.bmhklaboratorium;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.dao.cervix.CervixVerrichtingDao;
import nl.rivm.screenit.main.util.EnumStringUtil;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.SimpleStringResourceModel;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.component.modal.ConfirmPanel;
import nl.rivm.screenit.main.web.component.modal.DefaultConfirmCallback;
import nl.rivm.screenit.main.web.component.modal.IDialog;
import nl.rivm.screenit.main.web.component.price.BigDecimalPricePropertyColumn;
import nl.rivm.screenit.main.web.component.table.AjaxImageCellPanel;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.OrganisatieBeheer;
import nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.OrganisatiePaspoortPanel;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.BMHKLaboratorium;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.cervix.enums.CervixTariefType;
import nl.rivm.screenit.model.cervix.facturatie.CervixLabTarief;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.enums.ToegangLevel;
import nl.rivm.screenit.service.AutorisatieService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.cervix.CervixVerrichtingService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import nl.topicuszorg.wicket.search.column.DateTimePropertyColumn;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.extensions.markup.html.repeater.data.grid.ICellPopulator;
import org.apache.wicket.extensions.markup.html.repeater.data.table.AbstractColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.INZIEN,
	constraint = ShiroConstraint.HasPermission,
	recht = { Recht.GEBRUIKER_CERVIX_LABORATORIUM_TARIEF },
	checkScope = true,
	level = ToegangLevel.LANDELIJK,
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.CERVIX })
public class CervixLaboratoriumTarievenPage extends OrganisatieBeheer
{

	@SpringBean
	private ICurrentDateSupplier currentDateSupplier;

	@SpringBean
	private CervixVerrichtingService cervixVerrichtingService;

	@SpringBean
	private CervixVerrichtingDao cervixVerrichtingDao;

	@SpringBean
	private AutorisatieService autorisatieService;

	@SpringBean
	private LogService logService;

	private Actie actie;

	private IModel<CervixLabTarief> zoekObject;

	private IModel<BMHKLaboratorium> labModel;

	private final BootstrapDialog dialog;

	private WebMarkupContainer tableContainer;

	public CervixLaboratoriumTarievenPage()
	{
		InstellingGebruiker gebruiker = ScreenitSession.get().getLoggedInInstellingGebruiker();
		actie = autorisatieService.getActieVoorMedewerker(gebruiker, null, Recht.GEBRUIKER_CERVIX_LABORATORIUM_TARIEF);

		labModel = ModelUtil.sModel((BMHKLaboratorium) ScreenitSession.get().getCurrentSelectedOrganisatie());

		add(new OrganisatiePaspoortPanel("paspoort", ModelUtil.sModel(super.getCurrentSelectedOrganisatie())));

		zoekObject = ModelUtil.cModel(new CervixLabTarief());
		zoekObject.getObject().setBmhkLaboratorium(labModel.getObject());

		dialog = new BootstrapDialog("dialog");
		add(dialog);

		add(new IndicatingAjaxLink<Void>("toevoegen")
		{

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				dialog.openWith(target, new CervixLaboratoriumTarievenPopupPanel(BootstrapDialog.CONTENT_ID)
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

			@Override
			public boolean isVisible()
			{
				return Actie.TOEVOEGEN.getNiveau() <= actie.getNiveau();
			}
		});

		add(new IndicatingAjaxLink<Void>("indexeren")
		{

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				dialog.openWith(target, new CervixLaboratoriumIndexerenPopupPanel(BootstrapDialog.CONTENT_ID)
				{
					@Override
					protected void opslaan(AjaxRequestTarget target, String melding)
					{
						String infoMelding = "Tarief succesvol opgeslagen. ";
						if (!melding.isEmpty())
						{
							infoMelding += " Bijgewerkte bestaande tarieven: " + melding + ". "
								+ " Per bijgewerkte tarief start nu nog het bijwerken van de verrichtingen met het nieuwe tarief."
								+ " Voor elk bijgewerkte tarief wordt een logregel gemaakt als het bijwerken van de verrichtingen voltooid is.";
						}
						info(infoMelding);
						dialog.close(target);
						replaceContainer(target);
					}
				});
			}

			@Override
			public boolean isVisible()
			{
				return Actie.TOEVOEGEN.getNiveau() <= actie.getNiveau();
			}
		});

		tableContainer = getTarievenContainer();
		add(tableContainer);

	}

	private WebMarkupContainer getTarievenContainer()
	{
		SimpleDateFormat format = new SimpleDateFormat("dd-MM-yyyy");

		WebMarkupContainer container = new WebMarkupContainer("tableContainer");
		container.setOutputMarkupPlaceholderTag(true);

		List<IColumn<CervixLabTarief, String>> columns = new ArrayList<IColumn<CervixLabTarief, String>>();
		for (CervixTariefType type : CervixTariefType.getAlleLabTariefTypes())
		{
			columns
				.add(new BigDecimalPricePropertyColumn<CervixLabTarief, String>(new SimpleStringResourceModel(EnumStringUtil.getPropertyString(type)), type.getBedragProperty()));
		}
		columns.add(new DateTimePropertyColumn<>(Model.of("Geldig vanaf"), "geldigVanafDatum", "geldigVanafDatum", format));
		columns.add(new DateTimePropertyColumn<>(Model.of("Geldig t/m"), "geldigTotenmetDatum", "geldigTotenmetDatum", format));

		if (Actie.VERWIJDEREN == actie)
		{
			columns.add(new AbstractColumn<CervixLabTarief, String>(Model.of("Verwijderen"))
			{

				private static final long serialVersionUID = 1L;

				@Override
				public void populateItem(Item<ICellPopulator<CervixLabTarief>> cellItem, String componentId, IModel<CervixLabTarief> rowModel)
				{
					CervixLabTarief tarief = rowModel.getObject();
					if (tarief.getGeldigVanafDatum().after(currentDateSupplier.getDate()))
					{
						cellItem.add(new AjaxImageCellPanel<CervixLabTarief>(componentId, rowModel, "icon-trash")
						{
							@Override
							protected void onClick(AjaxRequestTarget target)
							{
								dialog.openWith(target,
									new ConfirmPanel(IDialog.CONTENT_ID, Model.of("Weet u zeker dat u dit tarief wilt verwijderen?"), null, new DefaultConfirmCallback()
									{

										private static final long serialVersionUID = 1L;

										@Override
										public void onYesClick(AjaxRequestTarget target)
										{
											String verwijderdMelding = cervixVerrichtingService.getLogMeldingLabTariefVerwijderd(rowModel.getObject());
											String melding = "Laboratorium: " + labModel.getObject().getNaam() + verwijderdMelding;
											cervixVerrichtingService.verwijderCervixTarief(rowModel.getObject(), ScreenitSession.get().getLoggedInAccount(), melding);
											info("Laboratoriumtarief succesvol verwijderd.");
											replaceContainer(target);

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

		ScreenitDataTable<CervixLabTarief, String> table = new ScreenitDataTable<CervixLabTarief, String>("table", columns,
			new CervixLaboratoriumTarievenDataProvider(zoekObject), Model.of("Tarieven"));
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

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(zoekObject);
	}
}
