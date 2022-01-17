package nl.rivm.screenit.main.web.gebruiker.clienten.contact.mamma;

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

import nl.rivm.screenit.dto.mamma.afspraken.MammaStandplaatsPeriodeMetAfstandDto;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode;
import nl.rivm.screenit.service.mamma.MammaBaseStandplaatsService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.extensions.markup.html.repeater.util.SortableDataProvider;
import org.apache.wicket.injection.Injector;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class MammaStandplaatsPeriodeProvider extends SortableDataProvider<MammaStandplaatsPeriodeMetAfstandDto, String>
{

	private static final long serialVersionUID = 1L;

	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private MammaBaseStandplaatsService baseStandplaatsService;

	private IModel<MammaAfspraakWijzigenFilter> filterModel;

	private IModel<Client> clientModel;

	private List<MammaStandplaatsPeriodeMetAfstandDto> standplaatsPeriodeMetAfstandDtos;

	public MammaStandplaatsPeriodeProvider(IModel<Client> clientModel, IModel<MammaAfspraakWijzigenFilter> filterModel)
	{
		Injector.get().inject(this);
		this.filterModel = filterModel;
		this.clientModel = clientModel;
	}

	@Override
	public Iterator<? extends MammaStandplaatsPeriodeMetAfstandDto> iterator(long first, long count)
	{
		return standplaatsPeriodeMetAfstandDtos.subList((int) first, (int) (first + count)).iterator();
	}

	@Override
	public long size()
	{
		if (filterModel.getObject().getVanaf() != null)
		{
			standplaatsPeriodeMetAfstandDtos = baseStandplaatsService.getStandplaatsPeriodeMetAfstandDtos(clientModel.getObject(), filterModel.getObject());
			standplaatsPeriodeMetAfstandDtos.sort((standplaatsPeriodeMetAfstandDto1, standplaatsPeriodeMetAfstandDto2) ->
			{
				int compareTo = standplaatsPeriodeMetAfstandDto1.getAfstand().compareTo(standplaatsPeriodeMetAfstandDto2.getAfstand());
				if (compareTo == 0)
				{
					MammaStandplaatsPeriode standplaatsPeriode1 = hibernateService.load(MammaStandplaatsPeriode.class, standplaatsPeriodeMetAfstandDto1.getStandplaatsPeriodeId());
					MammaStandplaatsPeriode standplaatsPeriode2 = hibernateService.load(MammaStandplaatsPeriode.class, standplaatsPeriodeMetAfstandDto2.getStandplaatsPeriodeId());
					compareTo = standplaatsPeriode1.getVanaf().compareTo(standplaatsPeriode2.getVanaf());
				}
				return compareTo;
			});
			return standplaatsPeriodeMetAfstandDtos.size();
		}
		return 0;
	}

	@Override
	public IModel<MammaStandplaatsPeriodeMetAfstandDto> model(MammaStandplaatsPeriodeMetAfstandDto standplaatsPeriodeMetAfstandDto)
	{
		return Model.of(standplaatsPeriodeMetAfstandDto);
	}

	@Override
	public void detach()
	{
		ModelUtil.nullSafeDetach(filterModel);
		ModelUtil.nullSafeDetach(clientModel);
	}
}
