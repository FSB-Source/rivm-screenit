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

import nl.rivm.screenit.main.web.component.bootstrap.BootstrapCollapsePanel.PanelCreator;
import nl.rivm.screenit.model.dashboard.DashboardType;
import nl.rivm.screenit.model.dashboard.DashboardStatus;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Level;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IDetachable;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;

public class DashboardStatusPanelCreator implements PanelCreator, IDetachable
{

	private static final long serialVersionUID = 1L;

	private IModel<DashboardStatus> model;

	public DashboardStatusPanelCreator(IModel<DashboardStatus> statusModel)
	{
		model = statusModel;
	}

	@Override
	public IModel<String> getPanelName()
	{
		DashboardType dItem = ModelUtil.nullSafeGet(model).getType();
		String bvoString = Bevolkingsonderzoek.getAfkortingen(dItem.getBevolkingsOnderzoek());
		String naamDasboardItem = " - " + dItem.getNaam();
		return Model.of(bvoString + naamDasboardItem);
	}

	@Override
	public Panel createPanel(String markupId)
	{
		return new DashboardStatusPanel(markupId, ModelUtil.nullSafeGet(model));
	}

	@Override
	public boolean isDefaultOpen()
	{
		return false;
	}

	@Override
	public Level getLevel()
	{
		return ModelUtil.nullSafeGet(model).getLevel();
	}

	@Override
	public void detach()
	{
		ModelUtil.nullSafeDetach(model);

	}

}
