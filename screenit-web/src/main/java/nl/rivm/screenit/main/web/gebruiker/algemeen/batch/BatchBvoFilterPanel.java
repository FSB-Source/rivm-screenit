
package nl.rivm.screenit.main.web.gebruiker.algemeen.batch;

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

import nl.rivm.screenit.main.util.EnumStringUtil;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.form.FilterBvoFormPanel;
import nl.rivm.screenit.model.batch.BvoZoekCriteria;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.JobType;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;

public abstract class BatchBvoFilterPanel extends Panel
{

	private static final long serialVersionUID = 1L;

	private IModel<BvoZoekCriteria> batchJobZoekCriteria;

	public BatchBvoFilterPanel(String id)
	{
		super(id);

		BvoZoekCriteria zoekCriteria = new BvoZoekCriteria();
		zoekCriteria.setBevolkingsonderzoeken(ScreenitSession.get().getOnderzoeken());
		if (ScreenitSession.get().isZoekObjectGezetForComponent(BatchBvoFilterPanel.class))
		{
			batchJobZoekCriteria = (IModel<BvoZoekCriteria>) ScreenitSession.get().getZoekObject(BatchBvoFilterPanel.class);
			batchJobZoekCriteria.getObject().getBevolkingsonderzoeken().retainAll(zoekCriteria.getBevolkingsonderzoeken());
		}
		if (batchJobZoekCriteria == null || batchJobZoekCriteria.getObject().getBevolkingsonderzoeken().isEmpty())
		{
			batchJobZoekCriteria = Model.of(zoekCriteria);
		}

		addOrReplace(new FilterBvoFormPanel<BvoZoekCriteria>("bvoFilter", batchJobZoekCriteria)
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void doFilter(IModel<BvoZoekCriteria> filterModel, AjaxRequestTarget target)
			{
				ScreenitSession.get().setZoekObject(BatchBvoFilterPanel.class, filterModel);
				bvoFilterChanged(filterModel, target);
			}

		});
	}

	protected String getJobName(JobType job)
	{
		String naam = getString(EnumStringUtil.getPropertyString(job));
		String afkortingen = "(" + Bevolkingsonderzoek.getAfkortingen(job.getBevolkingsOnderzoeken()) + ")";
		return naam + " " + afkortingen;
	}

	protected BvoZoekCriteria getBatchJobZoekCriteria()
	{
		return batchJobZoekCriteria.getObject();
	}

	protected abstract void bvoFilterChanged(IModel<BvoZoekCriteria> filterModel, AjaxRequestTarget target);
}
