package nl.rivm.screenit.main.web.gebruiker.screening.mamma.followup.followuppathologie;

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

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.main.service.mamma.MammaFollowUpService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.table.ClientColumn;
import nl.rivm.screenit.main.web.component.table.ImageIconCellPanel;
import nl.rivm.screenit.main.web.component.table.NotClickablePropertyColumn;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerBasePage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.followup.AbstractMammaFollowUpPage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.followup.MammaFollowUpGebeldCellPanel;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.followup.MammaFollowUpNietTeVerwachtenCellPanel;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.MammaFollowUpRadiologieVerslag;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.markup.html.repeater.data.grid.ICellPopulator;
import org.apache.wicket.extensions.markup.html.repeater.data.table.AbstractColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.EmptyPanel;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	constraint = ShiroConstraint.HasPermission,
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.MAMMA },
	recht = { Recht.GEBRUIKER_MAMMA_FOLLOW_UP_PATHOLOGIE_WERKLIJST })
public class MammaFollowUpPathologieWerklijstPage extends AbstractMammaFollowUpPage
{
	private WebMarkupContainer refreshContainer;

	@SpringBean
	private ICurrentDateSupplier dateSupplier;

	@SpringBean
	private SimplePreferenceService preferenceService;

	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private MammaFollowUpService followUpService;

	public MammaFollowUpPathologieWerklijstPage(IModel<Instelling> instellingModel)
	{
		setDefaultModel(new CompoundPropertyModel<>(instellingModel));
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();
		add(new Label("naam"));

		MammaFollowUpPathologieProvider followUpPathologieProvider = new MammaFollowUpPathologieProvider((IModel<Instelling>) getDefaultModel());

		refreshContainer = new WebMarkupContainer("refreshContainer");
		refreshContainer.setOutputMarkupId(Boolean.TRUE);
		add(refreshContainer);

		List<IColumn<MammaFollowUpRadiologieVerslag, String>> columns = new ArrayList<>();
		columns.add(new PropertyColumn<>(Model.of("Invoer radiologieverslag ziekenhuis"), "ingevoerdOp", "ingevoerdOp"));
		columns.add(new ClientColumn<>("screeningRonde.dossier.client"));
		columns.add(new PropertyColumn<>(Model.of("Geboortedatum"), "screeningRonde.dossier.client.persoon.geboortedatum"));
		columns.add(new PropertyColumn<>(Model.of("Bsn"), "screeningRonde.dossier.client.persoon.bsn"));
		columns.add(new NotClickablePropertyColumn<MammaFollowUpRadiologieVerslag, String>(Model.of("Urgentie"), "")
		{
			@Override
			public void populateItem(Item<ICellPopulator<MammaFollowUpRadiologieVerslag>> item, String componentId, IModel<MammaFollowUpRadiologieVerslag> rowModel)
			{
				Date urgentVanaf = DateUtil
					.toUtilDate(
						dateSupplier.getLocalDate().minusDays(preferenceService.getInteger(PreferenceKey.MAMMA_FOLLOW_UP_PATHOLOGIE_WERKLIJST_NA_RADIOLOGIEVERSLAG.name())));
				if (rowModel.getObject().getIngevoerdOp().compareTo(urgentVanaf) <= 0)
				{
					item.add(new ImageIconCellPanel<>(componentId, rowModel, "icon-large-font fa fa-exclamation", "Urgent"));
				}
				else
				{
					item.add(new EmptyPanel(componentId));
				}
			}
		});
		columns.add(new PropertyColumn<>(Model.of("Gebeld op"), "laatstGebeldOverPaVerslag", "laatstGebeldOverPaVerslag"));
		columns.add(new AbstractColumn<MammaFollowUpRadiologieVerslag, String>(Model.of(""))
		{
			@Override
			public void populateItem(Item<ICellPopulator<MammaFollowUpRadiologieVerslag>> item, String s, IModel<MammaFollowUpRadiologieVerslag> iModel)
			{
				item.add(new MammaFollowUpGebeldCellPanel(s, dialog, "gebeld")
				{
					@Override
					protected void onOpslaan(AjaxRequestTarget ajaxRequestTarget)
					{
						super.onOpslaan(ajaxRequestTarget);

						MammaFollowUpRadiologieVerslag verslag = iModel.getObject();
						verslag.setLaatstGebeldOverPaVerslag(dateSupplier.getDate());
						hibernateService.saveOrUpdate(verslag);
						ajaxRequestTarget.add(refreshContainer);
					}
				});
			}
		});

		columns.add(new AbstractColumn<MammaFollowUpRadiologieVerslag, String>(Model.of(""))
		{
			@Override
			public void populateItem(Item<ICellPopulator<MammaFollowUpRadiologieVerslag>> item, String s, IModel<MammaFollowUpRadiologieVerslag> verslagModel)
			{
				item.add(new MammaFollowUpNietTeVerwachtenCellPanel(s, dialog, "nietTeVerwachten")
				{
					@Override
					protected void onOpslaan(AjaxRequestTarget ajaxRequestTarget)
					{
						super.onOpslaan(ajaxRequestTarget);
						followUpService.savePaVerslagNietTeVerwachten(verslagModel.getObject(), ScreenitSession.get().getLoggedInInstellingGebruiker());
						ajaxRequestTarget.add(refreshContainer);
					}
				});
			}
		});

		ScreenitDataTable<MammaFollowUpRadiologieVerslag, String> table = new ScreenitDataTable<>("resultaten", columns,
			followUpPathologieProvider,
			10, Model.of("radiologieverslag(en)"));

		refreshContainer.add(table);
	}

	@Override
	protected Class<? extends GebruikerBasePage> getActiveContextMenuClass()
	{
		return MammaFollowUpPathologieRegioWerklijstPage.class;
	}
}
