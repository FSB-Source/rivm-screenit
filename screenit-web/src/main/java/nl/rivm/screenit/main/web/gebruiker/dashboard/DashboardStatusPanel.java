package nl.rivm.screenit.main.web.gebruiker.dashboard;

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
import java.util.Arrays;
import java.util.List;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.main.service.cervix.CervixDossierService;
import nl.rivm.screenit.main.service.colon.ColonDossierService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.ConfirmingIndicatingAjaxSubmitLink;
import nl.rivm.screenit.main.web.component.SimpleStringResourceModel;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.component.table.AjaxImageCellPanel;
import nl.rivm.screenit.main.web.component.table.EnumPropertyColumn;
import nl.rivm.screenit.main.web.component.table.NotClickablePropertyColumn;
import nl.rivm.screenit.main.web.component.validator.EmailAddressenValidator;
import nl.rivm.screenit.main.web.gebruiker.algemeen.logging.LoggingTable;
import nl.rivm.screenit.model.dashboard.DashboardStatus;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Level;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.logging.LogRegel;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.DashboardService;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.mamma.MammaBaseDossierService;
import nl.rivm.screenit.util.NaamUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import nl.topicuszorg.wicket.search.column.DateTimePropertyColumn;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxButton;
import org.apache.wicket.extensions.markup.html.repeater.data.grid.ICellPopulator;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class DashboardStatusPanel extends GenericPanel<DashboardStatus>
{
	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private DashboardService dashboardService;

	@SpringBean
	private LogService logService;

	@SpringBean
	private ClientService clientService;

	private BootstrapDialog confirmDialog;

	@SpringBean
	private ColonDossierService colonDossierService;

	@SpringBean
	private CervixDossierService cervixDossierService;

	@SpringBean
	private MammaBaseDossierService mammaDossierService;

	private final WebMarkupContainer dashboardTabelContainer = new WebMarkupContainer("dashboardTabelContainer");

	public DashboardStatusPanel(String id, DashboardStatus dashboardStatus)
	{
		super(id, ModelUtil.ccModel(dashboardStatus));

		confirmDialog = new BootstrapDialog("confirmDialog");
		add(confirmDialog);

		List<IColumn<LogRegel, String>> columns = new ArrayList<>();

		columns.add(new EnumPropertyColumn<>(new SimpleStringResourceModel("label.gebeurtenis"),
			"logRegel.logGebeurtenis", "logGebeurtenis"));
		columns.add(new DateTimePropertyColumn<>(new SimpleStringResourceModel("label.datumtijd"), "gebeurtenisDatum",
			"logRegel.gebeurtenisDatum", Constants.getDateTimeSecondsFormat()));
		columns.add(new PropertyColumn<>(new SimpleStringResourceModel("label.gebruiker"), "gebruiker.gebruikersnaam"));

		columns.add(new PropertyColumn<>(new SimpleStringResourceModel("label.client"), "persoon.achternaam")
		{
			@Override
			public IModel<?> getDataModel(IModel<LogRegel> rowModel)
			{
				return new Model<>(NaamUtil.getNaamClientMetBsnMetGeboortedatum(rowModel.getObject().getClient()));
			}
		});
		if (ScreenitSession.get().getOnderzoeken().contains(Bevolkingsonderzoek.MAMMA)
			&& Arrays.asList(dashboardStatus.getType().getBevolkingsOnderzoek()).contains(Bevolkingsonderzoek.MAMMA))
		{
			columns.add(new PropertyColumn<>(Model.of("SE"), "screeningsEenheid.naam", "screeningsEenheid.naam"));
		}
		columns.add(new PropertyColumn<>(new SimpleStringResourceModel("label.event.melding"), "logEvent.melding")
		{
			@Override
			public void populateItem(final Item<ICellPopulator<LogRegel>> item, final String componentId, final IModel<LogRegel> rowModel)
			{
				item.add(new Label(componentId, getDataModel(rowModel)).setEscapeModelStrings(false));
			}
		});

		if (ScreenitSession.get().checkPermission(Recht.GEBRUIKER_BEHEER_DASHBOARD_AFGEHANDELD_KNOP, Actie.AANPASSEN))
		{
			columns.add(maakAfgehandeldKnop());
		}

		var table = new LoggingTable("logging", columns, new DashboardDataProvider(getModel()), 5);
		table.setOutputMarkupId(true);
		dashboardTabelContainer.add(table);
		dashboardTabelContainer.setOutputMarkupId(true);
		add(dashboardTabelContainer);

		var form = new Form<>("form");
		add(form);

		var magEmailAanpassen = ScreenitSession.get().checkPermission(Recht.GEBRUIKER_BEHEER_DASHBOARD, Actie.AANPASSEN);
		ComponentHelper.addTextField(form, "emailadressen", false, 100, !magEmailAanpassen).add(EmailAddressenValidator.getInstance());
		form.add(new IndicatingAjaxButton("opslaan")
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				hibernateService.saveOrUpdate(DashboardStatusPanel.this.getModelObject());
				info(getString("message.gegevensopgeslagen"));
			}
		}.setVisible(magEmailAanpassen));

		ConfirmingIndicatingAjaxSubmitLink<String> bulkAfhandelLink = new ConfirmingIndicatingAjaxSubmitLink<>("bulkAfhandel", confirmDialog,
			"Afhandelen.bulk.popup.bevestiging")
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				var logregels = logService.getLogRegelsVanDashboard(DashboardStatusPanel.this.getModelObject());

				for (var logRegel : logregels)
				{
					if (!logRegel.getLogEvent().getLevel().equals(Level.INFO))
					{
						afhandelen(target, logRegel);
					}
				}
			}

			@Override
			protected IModel<String> getHeaderStringModel()
			{
				return Model.of(getString("Afhandelen.bulk.header"));
			}

			@Override
			protected IModel<String> getContentStringModel()
			{
				return Model.of(getString("Afhandelen.bulk.popup.bevestiging"));
			}
		};

		bulkAfhandelLink.setVisible(
			ScreenitSession.get().checkPermission(Recht.GEBRUIKER_BEHEER_DASHBOARD_AFGEHANDELD_KNOP, Actie.AANPASSEN) && !dashboardStatus.getLevel().equals(Level.INFO));

		form.add(bulkAfhandelLink);
	}

	private NotClickablePropertyColumn<LogRegel, String> maakAfgehandeldKnop()
	{
		return new NotClickablePropertyColumn<>(Model.of("Afgehandeld"), "")
		{
			@Override
			public void populateItem(Item<ICellPopulator<LogRegel>> item, String componentId, IModel<LogRegel> iModel)
			{
				var emptyLabel = new Label(componentId, "");
				emptyLabel.setOutputMarkupId(true);
				if (!iModel.getObject().getLogEvent().getLevel().equals(Level.INFO))
				{
					item.add(new AjaxImageCellPanel<>(componentId, iModel, "icon-ok-sign")
					{
						@Override
						protected void onClick(AjaxRequestTarget target)
						{
							afhandelen(target, getModelObject());
						}
					});
				}
				else
				{
					item.add(emptyLabel);
				}
				item.setOutputMarkupId(true);
			}
		};
	}

	private void afhandelen(AjaxRequestTarget target, LogRegel logRegel)
	{
		var dashboardStatus = getModelObject();
		var dashboardLevelDowngraded = false;
		var ingelogdeInstellingGebruiker = ScreenitSession.get().getLoggedInInstellingGebruiker();
		var gebruikersnaam = ingelogdeInstellingGebruiker.getMedewerker().getGebruikersnaam();

		switch (dashboardStatus.getType())
		{
		case CERVIX_CONTROLE_MISSENDE_UITSLAGEN:
			dashboardLevelDowngraded = cervixDossierService.setUitslagenGecontroleerdEnUpdateDashboard(logRegel,
				ingelogdeInstellingGebruiker, dashboardStatus);
			break;
		case COLON_CONTROLE_MISSENDE_UITSLAGEN:
			dashboardLevelDowngraded = colonDossierService.setUitslagenGecontroleerdEnUpdateDashboard(logRegel,
				ingelogdeInstellingGebruiker, dashboardStatus);
			break;
		case MAMMA_CONTROLE_MISSENDE_UITSLAGEN:
			dashboardLevelDowngraded = mammaDossierService.setUitslagenGecontroleerdEnUpdateDashboard(logRegel,
				ingelogdeInstellingGebruiker, dashboardStatus);
			break;
		case CERVIX_DIGITALE_LABFORMULIER_FOUT_BERICHTEN:
			dashboardLevelDowngraded = logService.verwijderLogRegelsVanDashboards(List.of(logRegel), ingelogdeInstellingGebruiker,
				LogGebeurtenis.CERVIX_DIGITAAL_LABFORMULIER_FOUT_ONTVANGEN_GECONTROLEERD);
			break;
		case GBA_LANDELIJK:
			if (logRegel.getLogGebeurtenis().equals(LogGebeurtenis.GBA_IMPORT_VERWIJDERD_VAN_PERSOONSLIJST))
			{
				logService.logGebeurtenis(LogGebeurtenis.GBA_DASHBOARDMELDING_AFGEHANDELD, ScreenitSession.get().getLoggedInAccount(), logRegel.getClient());
			}
			dashboardLevelDowngraded = dashboardService.updateLogRegelMetDashboardStatus(logRegel,
				gebruikersnaam, dashboardStatus);
			break;
		default:
			dashboardLevelDowngraded = dashboardService.updateLogRegelMetDashboardStatus(logRegel,
				gebruikersnaam, dashboardStatus);
		}
		if (!dashboardLevelDowngraded && target != null)
		{
			target.add(dashboardTabelContainer);
		}
		else
		{
			setResponsePage(new DashboardPage());
		}
	}

}
