package nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.route;

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

import nl.rivm.screenit.dao.mamma.MammaBaseStandplaatsDao;
import nl.rivm.screenit.dto.mamma.planning.PlanningStandplaatsPeriodeDto;
import nl.rivm.screenit.model.mamma.MammaStandplaats;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.behavior.AttributeAppender;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public abstract class MammaStandplaatsPeriodeOpmerkingenPanel extends GenericPanel<PlanningStandplaatsPeriodeDto>
{

	private static final long serialVersionUID = 1L;

	@SpringBean
	private MammaBaseStandplaatsDao baseStandplaatsDao;

	@SpringBean
	private HibernateService hibernateService;

	public MammaStandplaatsPeriodeOpmerkingenPanel(String id, IModel<PlanningStandplaatsPeriodeDto> model)
	{
		super(id, model);
		PlanningStandplaatsPeriodeDto standplaatsPeriodeDto = getModelObject();

		AjaxLink<PlanningStandplaatsPeriodeDto> opmerkingen = new AjaxLink<PlanningStandplaatsPeriodeDto>("opmerkingen")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				MammaStandplaatsPeriodeOpmerkingenPanel.this.openOpmerkingen(target, MammaStandplaatsPeriodeOpmerkingenPanel.this.getModel());
			}
		};
		MammaStandplaats standplaats = hibernateService.get(MammaStandplaats.class, standplaatsPeriodeDto.standplaatsId);
		if (!baseStandplaatsDao.heeftActieveOpmerking(standplaats))
		{
			opmerkingen.add(new AttributeAppender("class", " opacity-05"));
		}
		add(opmerkingen);
	}

	protected abstract void openOpmerkingen(AjaxRequestTarget target, IModel<PlanningStandplaatsPeriodeDto> standplaatsPeriodeModel);

}
