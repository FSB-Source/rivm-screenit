package nl.rivm.screenit.main.web.gebruiker.screening.mamma.followup.followup;

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

import nl.rivm.screenit.main.model.mamma.beoordeling.MammaCeWerklijstZoekObject;
import nl.rivm.screenit.main.service.mamma.MammaFollowUpService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.table.NotClickableAbstractColumn;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerMenuItem;
import nl.rivm.screenit.main.web.gebruiker.clienten.dossier.ClientDossierPage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.ce.panels.MammaCeFilter;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.ce.panels.MammaCeZoekPanel;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.ce.werklijst.AbstractMammaCeWerklijst;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.followup.AbstractMammaFollowUpPage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.followup.MammaFollowUpGebeldCellPanel;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.followup.MammaFollowUpNietTeVerwachtenCellPanel;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.CentraleEenheid;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaOnderzoek_;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde_;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus;
import nl.rivm.screenit.model.mamma.enums.MammaFollowUpConclusieStatus;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.InstellingService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.markup.html.repeater.data.grid.ICellPopulator;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
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
	recht = { Recht.GEBRUIKER_MAMMA_FOLLOW_UP_NIET_GEDOWNLOAD_WERKLIJST },
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.MAMMA },
	organisatieTypeScopes = { OrganisatieType.SCREENINGSORGANISATIE })
public class MammaFollowUpNietGedownloadWerklijst extends AbstractMammaCeWerklijst
{
	@SpringBean
	private InstellingService instellingService;

	private MammaCeFollowUpDataProvider dataProvider;

	@SpringBean
	private ICurrentDateSupplier dateSupplier;

	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private MammaFollowUpService followUpService;

	public MammaFollowUpNietGedownloadWerklijst()
	{
		super();
		dataProvider = new MammaCeFollowUpDataProvider(MammaOnderzoek_.CREATIE_DATUM, zoekObjectModel);
		List<CentraleEenheid> alleMogelijkeCentraleEenheden = getAlleMogelijkeCentraleEenheden();
		MammaCeWerklijstZoekObject zoekObject = zoekObjectModel.getObject();
		if (zoekObject.getCentraleEenheden() == null)
		{
			zoekObject.setCentraleEenheden(alleMogelijkeCentraleEenheden);
		}
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
				return new ArrayList<>();
			}

			@Override
			protected List<MammaCeFilter> getTeTonenCeFilters()
			{
				return Arrays.asList(MammaCeFilter.CE, MammaCeFilter.BE);
			}
		};
		add(zoekPanel);
	}

	private List<CentraleEenheid> getAlleMogelijkeCentraleEenheden()
	{
		return instellingService.getMogelijkeCentraleEenheden(ScreenitSession.get().getInstelling());
	}

	private void createResultTable()
	{
		List<IColumn<MammaBeoordeling, String>> columns = new ArrayList<>();
		columns.add(getOnderzoeksdatumColumn());
		columns.add(getClientColumn());
		columns.add(getGeboortedatumColumn());
		columns.add(getBsnColumn());
		columns.add(getBeColumn());
		columns.add(new PropertyColumn<>(Model.of("Gebeld op"), propertyChain(rondeSortProperty(), MammaScreeningRonde_.LAATST_GEBELD_FOLLOW_UP_NIET_GEDOWNLOAD),
			"onderzoek.afspraak.uitnodiging.screeningRonde.laatstGebeldFollowUpNietGedownload"));

		columns.add(new NotClickableAbstractColumn<>(Model.of(""))
		{
			@Override
			public void populateItem(Item<ICellPopulator<MammaBeoordeling>> item, String s, IModel<MammaBeoordeling> beoordelingModel)
			{
				item.add(new MammaFollowUpGebeldCellPanel(s, dialog, "gebeld")
				{
					@Override
					protected void onOpslaan(AjaxRequestTarget ajaxRequestTarget)
					{
						super.onOpslaan(ajaxRequestTarget);
						MammaScreeningRonde screeningRonde = beoordelingModel.getObject().getOnderzoek().getAfspraak().getUitnodiging().getScreeningRonde();
						screeningRonde.setLaatstGebeldFollowUpNietGedownload(dateSupplier.getDate());
						hibernateService.saveOrUpdate(screeningRonde);
						ajaxRequestTarget.add(resultatenContainer);
					}
				});
			}
		});

		columns.add(new NotClickableAbstractColumn<>(Model.of(""))
		{
			@Override
			public void populateItem(Item<ICellPopulator<MammaBeoordeling>> item, String s, IModel<MammaBeoordeling> beoordelingModel)
			{
				item.add(new MammaFollowUpNietTeVerwachtenCellPanel(s, dialog, "nietTeVerwachten")
				{
					@Override
					protected void onOpslaan(AjaxRequestTarget ajaxRequestTarget)
					{
						super.onOpslaan(ajaxRequestTarget);

						MammaScreeningRonde screeningRonde = beoordelingModel.getObject().getOnderzoek().getAfspraak().getUitnodiging().getScreeningRonde();
						MammaFollowUpConclusieStatus conclusieStatus = MammaFollowUpConclusieStatus.NIET_TE_VERWACHTEN;
						followUpService.saveFollowUpConclusieStatus(screeningRonde, conclusieStatus, ScreenitSession.get().getLoggedInAccount());
						ajaxRequestTarget.add(resultatenContainer);
					}
				});
			}
		});

		resultatenContainer.add(new ScreenitDataTable<>("resultaten", columns, dataProvider, 10, Model.of("onderzoek(en)"))
		{
			@Override
			public void onClick(AjaxRequestTarget target, IModel<MammaBeoordeling> model)
			{
				super.onClick(target, model);
				if (ScreenitSession.get().getAuthorizationStrategy().isInstantiationAuthorized(ClientDossierPage.class))
				{
					setResponsePage(
						new ClientDossierPage(ModelUtil.sModel(model.getObject().getOnderzoek().getAfspraak().getUitnodiging().getScreeningRonde().getDossier().getClient())));
				}
			}
		});
	}

	@Override
	protected List<GebruikerMenuItem> getContextMenuItems()
	{
		return AbstractMammaFollowUpPage.getContextMenuItemsList();
	}
}
