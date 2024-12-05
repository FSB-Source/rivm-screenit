package nl.rivm.screenit.main.web.gebruiker.screening.mamma.sestatus;

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

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.PollingAbstractAjaxTimerBehavior;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.main.web.component.table.ScreenitDateTimePropertyColumn;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerBasePage;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerHoofdMenuItem;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerMenuItem;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.Instelling_;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheidStatus;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheidStatus_;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid_;
import nl.rivm.screenit.service.BerichtToSeRestBkService;
import nl.rivm.screenit.util.DateUtil;

import org.apache.commons.lang.StringUtils;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.behavior.AttributeAppender;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.extensions.markup.html.repeater.data.grid.ICellPopulator;
import org.apache.wicket.extensions.markup.html.repeater.data.table.AbstractColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.basic.MultiLineLabel;
import org.apache.wicket.markup.html.panel.EmptyPanel;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.util.time.Duration;
import org.wicketstuff.shiro.ShiroConstraint;

import static nl.rivm.screenit.util.StringUtil.propertyChain;

@SecurityConstraint(
	constraint = ShiroConstraint.HasPermission,
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.MAMMA },
	recht = { Recht.GEBRUIKER_MAMMA_SE_STATUS_INZIEN },
	organisatieTypeScopes = { OrganisatieType.RIVM, OrganisatieType.SCREENINGSORGANISATIE })
public class MammaSEStatusPage extends GebruikerBasePage
{

	@SpringBean
	private BerichtToSeRestBkService berichtToSeRestBkService;

	private final ScreenitDataTable<MammaScreeningsEenheid, String> statusTabel;

	public MammaSEStatusPage()
	{
		IndicatingAjaxLink<Void> gegevensVerversenButton = new IndicatingAjaxLink<>("verversGegevens")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				berichtToSeRestBkService.statusAanvragenVoorIedereSe();
				info("Verzoek om statusupdate is verstuurd");
			}
		};
		add(gegevensVerversenButton);
		statusTabel = maakStatusTabel();
		add(statusTabel);
		add(createRefreshTimer());
	}

	private ScreenitDataTable<MammaScreeningsEenheid, String> maakStatusTabel()
	{
		ScreeningOrganisatie ingelogdNamensRegio = ScreenitSession.get().getScreeningOrganisatie();
		MammaSEStatusDataProvider dataProvider = new MammaSEStatusDataProvider(Model.of(ingelogdNamensRegio));

		return new ScreenitDataTable<>("tabel", getColumns(), dataProvider, 100, Model.of("screeningseenheden"), false);
	}

	private List<IColumn<MammaScreeningsEenheid, String>> getColumns()
	{
		List<IColumn<MammaScreeningsEenheid, String>> columns = new ArrayList<>();
		columns.add(new PropertyColumn<>(Model.of("Code"), MammaScreeningsEenheid_.CODE, "code"));
		columns.add(new PropertyColumn<>(Model.of("Screeningsorganisatie"),
			propertyChain(MammaScreeningsEenheid_.BEOORDELINGS_EENHEID, Instelling_.PARENT, Instelling_.REGIO, Instelling_.NAAM), "beoordelingsEenheid.parent.regio.naam"));
		columns.add(new PropertyColumn<>(Model.of("Versie"), propertyChain(MammaScreeningsEenheid_.STATUS, MammaScreeningsEenheidStatus_.VERSIE), "status.versie"));
		columns.add(new AbstractColumn<>(Model.of("Stamgegevens aanwezig"))
		{
			@Override
			public void populateItem(Item<ICellPopulator<MammaScreeningsEenheid>> cell, String id, IModel<MammaScreeningsEenheid> model)
			{
				populateStamgegevensKolom(cell, id, model);
			}
		});
		columns.add(new PropertyColumn<>(Model.of("Offline daglijsten"), "status.offlineDaglijsten")
		{
			@Override
			public IModel<?> getDataModel(IModel<MammaScreeningsEenheid> rowModel)
			{
				IModel<?> model = super.getDataModel(rowModel);
				return StringUtils.isBlank((String) model.getObject()) ? model
					: Model.of(Arrays.stream(((String) model.getObject()).split(", "))
					.map(this::getFormattedDate).collect(Collectors.joining(", ")));
			}

			private String getFormattedDate(String isoDate)
			{
				try
				{
					var date = LocalDate.parse(isoDate, DateTimeFormatter.ISO_DATE);
					return date.format(DateUtil.LOCAL_DATE_FORMAT);
				}
				catch (DateTimeParseException e)
				{
					return "?";
				}
			}
		});
		columns.add(new ScreenitDateTimePropertyColumn<>(Model.of("Laatste check"), propertyChain(MammaScreeningsEenheid_.STATUS, MammaScreeningsEenheidStatus_.STATUS_MOMENT),
			"status.statusMoment"));
		columns.add(new PropertyColumn<>(Model.of("Online"), propertyChain(MammaScreeningsEenheid_.STATUS, MammaScreeningsEenheidStatus_.LAATSTE_KEER_OFFLINE_GEGAAN),
			"status.laatsteKeerOfflineGegaan")
		{
			@Override
			public void populateItem(Item<ICellPopulator<MammaScreeningsEenheid>> cell, String id, IModel<MammaScreeningsEenheid> model)
			{
				populateOnlineKolom(cell, id, model);
			}
		});
		return columns;
	}

	private void populateStamgegevensKolom(Item<ICellPopulator<MammaScreeningsEenheid>> cell, String id, IModel<MammaScreeningsEenheid> model)
	{
		MammaScreeningsEenheidStatus status = model.getObject().getStatus();
		if (status == null)
		{
			cell.add(new EmptyPanel(id));
			return;
		}
		if (status.getHuisartsenAanwezig() && status.getMammografenAanwezig() && status.getZorginstellingenAanwezig())
		{
			cell.add(new Label(id, Model.of("Ja")));
		}
		else
		{
			cell.add(new MultiLineLabel(id, Model.of(String.format("Huisartsen: %1$s\nMammografen: %2$s\nZorginstellingen: %3$s",
				status.getHuisartsenAanwezig() ? "Ja" : "Nee", status.getMammografenAanwezig() ? "Ja" : "Nee", status.getZorginstellingenAanwezig() ? "Ja" : "Nee"))));
		}
	}

	private void populateOnlineKolom(Item<ICellPopulator<MammaScreeningsEenheid>> cell, String id, IModel<MammaScreeningsEenheid> model)
	{
		MammaScreeningsEenheidStatus status = model.getObject().getStatus();
		if (status == null)
		{
			cell.add(new EmptyPanel(id));
		}
		else if (status.getLaatsteKeerOfflineGegaan() != null)
		{
			cell.add(new Label(id,
				Model.of("Nee, laatst online: " + status.getLaatsteKeerOfflineGegaan().format(DateUtil.LOCAL_DATE_TIME_FORMAT))));
			cell.add(new AttributeAppender("class", Model.of(" error")));
		}
		else
		{
			cell.add(new Label(id, Model.of("Ja")));
		}
	}

	private PollingAbstractAjaxTimerBehavior createRefreshTimer()
	{
		return new PollingAbstractAjaxTimerBehavior(Duration.seconds(5))
		{
			@Override
			protected void onTimer(AjaxRequestTarget target)
			{
				super.onTimer(target);
				target.add(statusTabel);
				restartTimer(target);
			}
		};
	}

	@Override
	protected GebruikerHoofdMenuItem getActieveMenuItem()
	{
		return GebruikerHoofdMenuItem.MAMMA;
	}

	@Override
	protected List<GebruikerMenuItem> getContextMenuItems()
	{
		return new ArrayList<>(List.of(
			new GebruikerMenuItem("label.tab.mammascreening.sestatus.overzicht", MammaSEStatusPage.class)));
	}
}
