package nl.rivm.screenit.main.web.gebruiker.dashboard;

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
import java.util.List;

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.bootstrap.BootstrapCollapsePanel;
import nl.rivm.screenit.main.web.component.bootstrap.BootstrapCollapsePanel.PanelCreator;
import nl.rivm.screenit.main.web.component.form.FilterBvoFormPanel;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerBasePage;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerHoofdMenuItem;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.batch.BvoZoekCriteria;
import nl.rivm.screenit.model.dashboard.DashboardStatus;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.DashboardService;
import nl.rivm.screenit.service.ScopeService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(actie = Actie.INZIEN, constraint = ShiroConstraint.HasPermission, recht = Recht.GEBRUIKER_BEHEER_DASHBOARD, bevolkingsonderzoekScopes = {
	Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
public class DashboardPage extends GebruikerBasePage
{

	private static final long serialVersionUID = 1L;

	private BootstrapDialog dialog;

	@SpringBean
	private ScopeService scopeService;

	@SpringBean
	private DashboardService dashboardService;

	private WebMarkupContainer container;

	private IModel<BvoZoekCriteria> dashboardZoekCriteria;

	public DashboardPage()
	{
		maakListView(null);
	}

	public void maakListView(AjaxRequestTarget target)
	{
		BvoZoekCriteria zoekCriteria = new BvoZoekCriteria();
		zoekCriteria.setBevolkingsonderzoeken(ScreenitSession.get().getOnderzoeken());
		if (ScreenitSession.get().isZoekObjectGezetForComponent(DashboardPage.class))
		{
			dashboardZoekCriteria = (IModel<BvoZoekCriteria>) ScreenitSession.get().getZoekObject(DashboardPage.class);
			dashboardZoekCriteria.getObject().getBevolkingsonderzoeken().retainAll(zoekCriteria.getBevolkingsonderzoeken());
		}
		if (dashboardZoekCriteria == null || dashboardZoekCriteria.getObject().getBevolkingsonderzoeken().isEmpty())
		{
			dashboardZoekCriteria = Model.of(zoekCriteria);
		}

		container = new WebMarkupContainer("dashboardContainer");
		container.setOutputMarkupId(true);
		addOrReplace(container);

		addOrReplace(new FilterBvoFormPanel<BvoZoekCriteria>("bvoFilter", dashboardZoekCriteria)
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void doFilter(IModel<BvoZoekCriteria> filterModel, AjaxRequestTarget target)
			{
				ScreenitSession.get().setZoekObject(DashboardPage.class, filterModel);
				maakListView(target);
				target.add(container);
			}

		});

		List<DashboardStatus> statussen = dashboardService.getListOfDashboardStatussen(ScreenitSession.get().getInstelling(), dashboardZoekCriteria.getObject()
			.getBevolkingsonderzoeken());

		List<PanelCreator> panels = new ArrayList<>();
		for (DashboardStatus item : statussen)
		{
			panels.add(new DashboardStatusPanelCreator(ModelUtil.sModel(item)));
		}

		BootstrapCollapsePanel accordion = new BootstrapCollapsePanel("accordion", panels);
		accordion.setOutputMarkupId(true);
		container.addOrReplace(accordion);

	}

	@Override
	protected GebruikerHoofdMenuItem getActieveMenuItem()
	{
		return GebruikerHoofdMenuItem.DASHBOARD;
	}
}
