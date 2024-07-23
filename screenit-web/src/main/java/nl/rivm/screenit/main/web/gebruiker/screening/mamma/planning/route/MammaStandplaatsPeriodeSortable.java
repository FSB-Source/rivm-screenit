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

import java.util.List;
import java.util.Optional;

import lombok.extern.slf4j.Slf4j;

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

import com.googlecode.wicket.jquery.core.Options;
import com.googlecode.wicket.jquery.ui.interaction.sortable.Sortable;

@Slf4j
public abstract class MammaStandplaatsPeriodeSortable extends Sortable<PlanningStandplaatsPeriodeDto>
{
	private static final long serialVersionUID = 1L;

	@SpringBean
	private MammaStandplaatsPeriodeService standplaatsPeriodeService;

	@SpringBean
	private MammaBaseAfspraakService baseAfspraakService;

	private final IModel<MammaScreeningsEenheid> screeningsEenheidModel;

	private Sortable<PlanningStandplaatsPeriodeDto> connectedSortable;

	public MammaStandplaatsPeriodeSortable(String id, MammaScreeningsEenheid screeningsEenheid, IModel<List<PlanningStandplaatsPeriodeDto>> list, Options options)
	{
		super(id, list, options);

		screeningsEenheidModel = ModelUtil.sModel(screeningsEenheid);
	}

	protected MammaScreeningsEenheid getScreeningsEenheid()
	{
		return screeningsEenheidModel.getObject();
	}

	@Override
	public void onUpdate(AjaxRequestTarget target, PlanningStandplaatsPeriodeDto teVerplaatsenStandplaatsPeriode, int rijNummerWaarheenGeschovenIs)
	{
		LOG.debug("onUpdate " + rijNummerWaarheenGeschovenIs + "/" + getScreeningsEenheid().getNaam() + "/" + teVerplaatsenStandplaatsPeriode);
		if (teVerplaatsenStandplaatsPeriode != null)
		{
			var naarBenedenTeVerplaatsenStandplaatsPeriode = getNaarBenedenTeVerplaatsenStandplaatsPeriode(rijNummerWaarheenGeschovenIs);

			if (naarBenedenTeVerplaatsenStandplaatsPeriode.isEmpty() || magGeplaatstWordenOpPlekStandplaatsperiode(naarBenedenTeVerplaatsenStandplaatsPeriode.get()))
			{
				modelChanging();

				var nieuwVolgnr = naarBenedenTeVerplaatsenStandplaatsPeriode.map(p -> p.screeningsEenheidVolgNr)
					.orElse(bepaalVolgNummerIndienGeenPeriodeVerplaatstWordt(teVerplaatsenStandplaatsPeriode));

				standplaatsPeriodeService.updateSortList(nieuwVolgnr, teVerplaatsenStandplaatsPeriode, getScreeningsEenheid(),
					ScreenitSession.get().getLoggedInInstellingGebruiker());
				finalizeMovement(target, true);
			}
			else
			{
				target.add(this);
				error(getString("may.not.moved"));
			}
		}
	}

	@Override
	public void onReceive(AjaxRequestTarget target, PlanningStandplaatsPeriodeDto teVerplaatsenStandplaatsPeriode, int rijNummerWaarheenGeschovenIs)
	{
		LOG.debug("onReceive " + rijNummerWaarheenGeschovenIs + "/" + getScreeningsEenheid().getNaam() + "/" + teVerplaatsenStandplaatsPeriode.standplaatsId + "/"
			+ teVerplaatsenStandplaatsPeriode.screeningsEenheidVolgNr);

		var naarBenedenTeVerplaatsenStandplaatsPeriode = getNaarBenedenTeVerplaatsenStandplaatsPeriode(rijNummerWaarheenGeschovenIs);

		if (naarBenedenTeVerplaatsenStandplaatsPeriode.isEmpty() || magGeplaatstWordenOpPlekStandplaatsperiode(naarBenedenTeVerplaatsenStandplaatsPeriode.get()))
		{
			modelChanging();

			var nieuwVolgnr = naarBenedenTeVerplaatsenStandplaatsPeriode.map(p -> p.screeningsEenheidVolgNr)
				.orElse(bepaalVolgNummerIndienGeenPeriodeVerplaatstWordt(teVerplaatsenStandplaatsPeriode));

			standplaatsPeriodeService.updateSortList(nieuwVolgnr, teVerplaatsenStandplaatsPeriode, getScreeningsEenheid(),
				ScreenitSession.get().getLoggedInInstellingGebruiker());
			super.onReceive(target, teVerplaatsenStandplaatsPeriode, rijNummerWaarheenGeschovenIs - 1);

			finalizeMovement(target, true);
		}
		else
		{
			target.add(this, connectedSortable);
			error(getString("may.not.moved"));
		}
	}

	private int bepaalVolgNummerIndienGeenPeriodeVerplaatstWordt(PlanningStandplaatsPeriodeDto teVerplaatsenStandplaatsPeriode)
	{
		if (getModelObject().isEmpty())
		{
			return bepaalVolgnummerGeenStandplaatsInView();
		}
		return teVerplaatsenStandplaatsPeriode.screeningsEenheidVolgNr;
	}

	private int bepaalVolgnummerGeenStandplaatsInView()
	{
		var standplaatsPerioden = getScreeningsEenheid().getStandplaatsPerioden();
		var optioneleMaximaleVolgnummerUitVerleden = standplaatsPerioden.stream().mapToInt(MammaStandplaatsPeriode::getScreeningsEenheidVolgNr).max();

		return optioneleMaximaleVolgnummerUitVerleden.isEmpty() ? 0 : optioneleMaximaleVolgnummerUitVerleden.getAsInt() + 1;
	}

	private Optional<PlanningStandplaatsPeriodeDto> getNaarBenedenTeVerplaatsenStandplaatsPeriode(int rijNummer)
	{
		var periodes = getModelObject();

		return periodes.size() <= rijNummer - 1 ? Optional.empty() : Optional.of(periodes.get(rijNummer - 1));
	}

	private boolean magGeplaatstWordenOpPlekStandplaatsperiode(PlanningStandplaatsPeriodeDto standplaatsPeriodeDto)
	{
		return standplaatsPeriodeDto.prognose &&
			(standplaatsPeriodeDto.id == null || !baseAfspraakService.heeftAfspraken(standplaatsPeriodeDto.id, MammaAfspraakStatus.GEPLAND));
	}

	@Override
	public void onRemove(AjaxRequestTarget target, PlanningStandplaatsPeriodeDto teVerwijderenStandplaatsPeriode)
	{
		LOG.debug(
			"onRemove " + getScreeningsEenheid().getNaam() + "/" + teVerwijderenStandplaatsPeriode.standplaatsId + "/" + teVerwijderenStandplaatsPeriode.screeningsEenheidVolgNr);
		modelChanging();
		finalizeMovement(target, false);
	}

	private void finalizeMovement(AjaxRequestTarget target, boolean isLast)
	{
		resetModel();
		modelChanged();

		target.add(this);
		if (isLast)
		{
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

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(screeningsEenheidModel);
	}
}
