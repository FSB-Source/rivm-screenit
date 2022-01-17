package nl.rivm.screenit.main.web.gebruiker.screening.mamma.followup.followupradiologie;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.Iterator;
import java.util.List;

import nl.rivm.screenit.dto.mamma.MammaFollowUpInstellingRadiologieDto;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.enums.MammaFollowUpDoorverwezenFilterOptie;
import nl.rivm.screenit.service.mamma.MammaBaseFollowUpService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.extensions.markup.html.repeater.data.sort.SortOrder;
import org.apache.wicket.extensions.markup.html.repeater.util.SortableDataProvider;
import org.apache.wicket.injection.Injector;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;

import org.springframework.beans.support.PropertyComparator;

public class MammaFollowUpRadiologieRegioProvider extends SortableDataProvider<MammaFollowUpInstellingRadiologieDto, String>
{
	@SpringBean
	private MammaBaseFollowUpService followUpService;

	private IModel<ScreeningOrganisatie> regioModel;

	private List<MammaFollowUpInstellingRadiologieDto> instellingList;

	private IModel<MammaFollowUpDoorverwezenFilterOptie> doorverwezenFilterOptieModel;

	private IModel<Integer> jaarModel;

	MammaFollowUpRadiologieRegioProvider(IModel<ScreeningOrganisatie> regioModel, IModel<MammaFollowUpDoorverwezenFilterOptie> doorverwezenFilterOptieModel,
		IModel<Integer> jaarModel)
	{
		Injector.get().inject(this);
		setSort("laatstGebeld", SortOrder.ASCENDING);
		this.regioModel = regioModel;
		this.doorverwezenFilterOptieModel = doorverwezenFilterOptieModel;
		this.jaarModel = jaarModel;
	}

	@Override
	public Iterator<? extends MammaFollowUpInstellingRadiologieDto> iterator(long first, long count)
	{
		setList();

		return instellingList.stream().sorted(new PropertyComparator<>(getSort().getProperty(), true, getSort().isAscending())).skip(first).limit(count)
			.iterator();
	}

	@Override
	public long size()
	{
		setList();
		return instellingList.size();
	}

	@Override
	public IModel<MammaFollowUpInstellingRadiologieDto> model(MammaFollowUpInstellingRadiologieDto object)
	{
		return Model.of(object);
	}

	public void resetList()
	{
		instellingList = null;
	}

	private void setList()
	{
		if (instellingList == null)
		{
			instellingList = followUpService.zoekOpenstaandeRadiologieVerslagenPerOrganisatie(ModelUtil.nullSafeGet(regioModel),
				ModelUtil.nullSafeGet(doorverwezenFilterOptieModel), ModelUtil.nullSafeGet(jaarModel));
		}
	}

	@Override
	public void detach()
	{
		super.detach();
		ModelUtil.nullSafeDetach(regioModel);
		ModelUtil.nullSafeDetach(doorverwezenFilterOptieModel);
		ModelUtil.nullSafeDetach(jaarModel);
	}
}
