package nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.route;

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

import java.util.List;

import nl.rivm.screenit.dto.mamma.planning.PlanningStandplaatsPeriodeDto;
import nl.rivm.screenit.main.service.mamma.MammaStandplaatsPeriodeService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode;
import nl.rivm.screenit.model.mamma.enums.MammaAfspraakStatus;
import nl.rivm.screenit.service.mamma.MammaBaseAfspraakService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.googlecode.wicket.jquery.core.Options;
import com.googlecode.wicket.jquery.ui.interaction.sortable.Sortable;

public abstract class MammaStandplaatsPeriodeSortable extends Sortable<PlanningStandplaatsPeriodeDto>
{
	private static final long serialVersionUID = 1L;

	private static final Logger LOG = LoggerFactory.getLogger(MammaStandplaatsPeriodeSortable.class);

	@SpringBean
	private MammaStandplaatsPeriodeService standplaatsPeriodeService;

	@SpringBean
	private MammaBaseAfspraakService baseAfspraakService;

	private IModel<MammaScreeningsEenheid> screeningsEenheidModel;

	private Sortable<PlanningStandplaatsPeriodeDto> connectedSortable;

	private Integer offset;

	public MammaStandplaatsPeriodeSortable(String id, MammaScreeningsEenheid screeningsEenheid, IModel<List<PlanningStandplaatsPeriodeDto>> list, Options options)
	{
		super(id, list, options);
		offset = list.getObject().stream().min((p1, p2) -> Integer.compare(p1.screeningsEenheidVolgNr, p2.screeningsEenheidVolgNr))
			.orElse(new PlanningStandplaatsPeriodeDto()).screeningsEenheidVolgNr;
		if (offset == null)
		{
			MammaStandplaatsPeriode defaultPeriode = new MammaStandplaatsPeriode();
			defaultPeriode.setScreeningsEenheidVolgNr(-1);
			offset = screeningsEenheid.getStandplaatsPerioden().stream().max((p1, p2) -> Integer.compare(p1.getScreeningsEenheidVolgNr(), p2.getScreeningsEenheidVolgNr()))
				.orElse(defaultPeriode).getScreeningsEenheidVolgNr() + 1;
		}
		screeningsEenheidModel = ModelUtil.sModel(screeningsEenheid);
	}

	protected MammaScreeningsEenheid getScreeningsEenheid()
	{
		return screeningsEenheidModel.getObject();
	}

	@Override
	public void onUpdate(AjaxRequestTarget target, PlanningStandplaatsPeriodeDto item, int index)
	{
		LOG.debug("onUpdate " + index + "/" + getScreeningsEenheid().getNaam() + "/" + item);
		if (item != null)
		{
			index--;

			if (magGeplaatstWordenOp(index))
			{
				modelChanging();
				standplaatsPeriodeService.updateSortList(index + offset, item, getScreeningsEenheid(), ScreenitSession.get().getLoggedInInstellingGebruiker());
				finalizeMovement(target, true);
			}
			else
			{
				target.add(this);
				error(getString("may.not.moved"));
				updateTooltips(target);
			}
		}
	}

	@Override
	public void onReceive(AjaxRequestTarget target, PlanningStandplaatsPeriodeDto item, int index)
	{
		LOG.debug("onReceive " + index + "/" + getScreeningsEenheid().getNaam() + "/" + item.standplaatsId + "/" + item.screeningsEenheidVolgNr);

		index--;
		if (magGeplaatstWordenOp(index))
		{
			modelChanging();
			standplaatsPeriodeService.updateSortList(index + offset, item, getScreeningsEenheid(), ScreenitSession.get().getLoggedInInstellingGebruiker());
			super.onReceive(target, item, index);

			finalizeMovement(target, true);
		}
		else
		{
			target.add(this, connectedSortable);
			error(getString("may.not.moved"));
			updateTooltips(target);
		}
	}

	@Override
	public void onRemove(AjaxRequestTarget target, PlanningStandplaatsPeriodeDto item)
	{
		LOG.debug("onRemove " + getScreeningsEenheid().getNaam() + "/" + item.standplaatsId + "/" + item.screeningsEenheidVolgNr);
		modelChanging();
		finalizeMovement(target, false);
	}

	private boolean magGeplaatstWordenOp(int index)
	{
		List<PlanningStandplaatsPeriodeDto> perioden = getModelObject();

		return perioden.size() <= index ||
			(perioden.get(index).prognose &&
				(perioden.get(index).id == null ||
					baseAfspraakService.countAfspraken(perioden.get(index).id, MammaAfspraakStatus.GEPLAND) == 0));
	}

	private void finalizeMovement(AjaxRequestTarget target, boolean isLast)
	{
		resetModel();
		modelChanged();

		target.add(this);
		if (isLast)
		{
			updateTooltips(target);
			info(getString("message.gegevens.onthouden"));
		}
	}

	public void resetModel()
	{
		getModel().setObject(standplaatsPeriodeService.getStandplaatsPeriodesSorted(getScreeningsEenheid()));
	}

	@Override
	public Sortable<PlanningStandplaatsPeriodeDto> connectWith(Sortable<PlanningStandplaatsPeriodeDto> sortable)
	{
		this.connectedSortable = sortable;
		return super.connectWith(sortable);
	}

	protected abstract void updateTooltips(AjaxRequestTarget target);

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(screeningsEenheidModel);
	}
}
