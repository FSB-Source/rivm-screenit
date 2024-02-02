package nl.rivm.screenit.main.web.gebruiker.screening.mamma.ce.werklijst.opschorting;

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

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.main.web.component.table.AjaxLinkTableCellPanel;
import nl.rivm.screenit.main.web.component.table.EnumPropertyColumn;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.main.web.gebruiker.clienten.ClientContactActieTypeWrapper;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.ce.panels.MammaCeZoekPanel;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.ce.werklijst.AbstractMammaCeWerklijst;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus;
import nl.rivm.screenit.service.mamma.MammaBaseAfspraakService;
import nl.rivm.screenit.service.mamma.MammaBaseBeoordelingService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.markup.html.repeater.data.grid.ICellPopulator;
import org.apache.wicket.extensions.markup.html.repeater.data.table.AbstractColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.INZIEN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = { Recht.GEBRUIKER_CENTRALE_EENHEID_OPSCHORTEN_BEOORDELINGEN },
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.MAMMA })
public class MammaCeOpgeschorteBeoordelingenWerklijstPage extends AbstractMammaCeWerklijst
{
	@SpringBean
	private MammaBaseBeoordelingService beoordelingsService;

	@SpringBean
	private MammaBaseAfspraakService afspraakService;

	public MammaCeOpgeschorteBeoordelingenWerklijstPage()
	{
		super();
		createResultTable();
		MammaCeZoekPanel zoekPanel = new MammaCeZoekPanel("zoekContainer", zoekObjectModel, this, resultatenContainer)
		{
			@Override
			protected List<MammaBeoordelingStatus> getRemoveFromDefaultFilter()
			{
				return new ArrayList<>();
			}

			@Override
			protected List<MammaBeoordelingStatus> getMammaMogelijkeBeoordelingFilterStatussen()
			{
				List<MammaBeoordelingStatus> beoordelingStatussen = new ArrayList<>();
				beoordelingStatussen.add(MammaBeoordelingStatus.OPGESCHORT);
				return beoordelingStatussen;
			}
		};
		add(zoekPanel);
	}

	private void createResultTable()
	{
		List<IColumn<MammaBeoordeling, String>> columns = new ArrayList<>();
		columns.add(getOnderzoeksdatumColumn());
		columns.add(getClientColumn());
		columns.add(getGeboortedatumColumn());
		columns.add(getBsnColumn());
		columns.add(getSeColumn());
		columns.add(getBeColumn());
		columns.add(getStatusColumn());
		columns.add(getOpschortredenColumn());
		columns.add(getOpschortOpmerkingRadioloog());
		columns.add(getAfspraakMakenMetClientKnopColumn());
		columns.add(getOpschortenTerugNaarWerklijstKnopColumn());
		ScreenitDataTable<MammaBeoordeling, String> resultatenTable = new ScreenitDataTable<MammaBeoordeling, String>("resultaten", columns, onderzoekDataProvider, 10, null);
		resultatenContainer.add(resultatenTable);
	}

	private IColumn<MammaBeoordeling, String> getOpschortredenColumn()
	{
		return new EnumPropertyColumn<>(Model.of("Opschortreden"), "beoordeling.opschortReden", "opschortReden", this);
	}

	private IColumn<MammaBeoordeling, String> getOpschortOpmerkingRadioloog()
	{
		return new PropertyColumn<>(Model.of("Opschortopmerking radioloog"), "beoordeling.opschortRedenTekst", "opschortRedenTekst");
	}

	private IColumn<MammaBeoordeling, String> getAfspraakMakenMetClientKnopColumn()
	{
		return new AbstractColumn<MammaBeoordeling, String>(Model.of("Afspraak maken met cliënt"))
		{
			@Override
			public void populateItem(Item<ICellPopulator<MammaBeoordeling>> cellItem, String componentId, IModel<MammaBeoordeling> rowModel)
			{
				if (afspraakService.isAfspraakBinnen180Dagen(rowModel.getObject().getOnderzoek()))
				{
					cellItem.add(new AjaxLinkTableCellPanel<MammaBeoordeling>(componentId, rowModel, "button.client.afspraak")
					{
						@Override
						protected void onClick(AjaxRequestTarget target, IModel<MammaBeoordeling> model)
						{
							List<Object> extraParameters = new ArrayList<>();
							extraParameters.add(Constants.CONTACT_EXTRA_PARAMETER_VANUIT_BK_PLANNING);
							extraParameters.add(Constants.CONTACT_EXTRA_PARAMETER_ALLEEN_CLIENT_CONTACT);
							Client client = model.getObject().getOnderzoek().getAfspraak().getUitnodiging().getScreeningRonde().getDossier().getClient();
							ClientContactActieTypeWrapper actie = ClientContactActieTypeWrapper.MAMMA_AFSPRAAK_MAKEN;
							setResponsePage(new MammaClientContactNaOpgeschortOnderzoekPage(ModelUtil.cModel(client), extraParameters, actie));
						}
					});
				}
				else
				{
					cellItem.add(new Label(componentId, getString("button.client.afspraak.niet.mogelijk")));
				}
			}
		};
	}

	private IColumn<MammaBeoordeling, String> getOpschortenTerugNaarWerklijstKnopColumn()
	{
		return new AbstractColumn<MammaBeoordeling, String>(Model.of("Onderzoek terugzetten"))
		{
			@Override
			public void populateItem(Item<ICellPopulator<MammaBeoordeling>> cellItem, String componentId, IModel<MammaBeoordeling> rowModel)
			{
				cellItem.add(new AjaxLinkTableCellPanel<MammaBeoordeling>(componentId, rowModel, "button.onderzoek.terugzetten")
				{
					@Override
					protected void onClick(AjaxRequestTarget target, IModel<MammaBeoordeling> beoordelingModel)
					{
						beoordelingsService.opgeschortOnderzoekTerugNaarWerklijst(beoordelingModel.getObject());
						target.add(resultatenContainer);
					}
				});
			}
		};
	}

}
