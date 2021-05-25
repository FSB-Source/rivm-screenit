package nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.tehuis;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.util.concurrent.Callable;

import nl.rivm.screenit.main.service.mamma.IMammaTehuisDto;
import nl.rivm.screenit.main.service.mamma.MammaTehuisService;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode;
import nl.rivm.screenit.model.mamma.MammaTehuis;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.extensions.markup.html.repeater.data.sort.SortOrder;
import org.apache.wicket.extensions.markup.html.repeater.util.SortableDataProvider;
import org.apache.wicket.injection.Injector;
import org.apache.wicket.model.IDetachable;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;

import com.google.common.primitives.Ints;

public class MammaTehuisDataProvider extends SortableDataProvider<IMammaTehuisDto, String>
{

	private static final long serialVersionUID = 1L;

	@SpringBean
	private MammaTehuisService tehuisService;

	private IModel<MammaTehuisFilter> criteria;

	public MammaTehuisDataProvider(String sortProperty, IModel<MammaTehuisFilter> criteria)
	{
		Injector.get().inject(this);
		setSort(sortProperty, SortOrder.ASCENDING);
		this.criteria = criteria;
	}

	private class MammaTehuisDto implements IMammaTehuisDto, IDetachable
	{

		private IModel<MammaTehuis> tehuis;

		private IModel<MammaStandplaatsPeriode> periode;

		@Override
		public void setTehuis(MammaTehuis tehuis)
		{
			this.tehuis = ModelUtil.sModel(tehuis);
		}

		@Override
		public MammaTehuis getTehuis()
		{
			return ModelUtil.nullSafeGet(tehuis);
		}

		@Override
		public void setStandplaatsPeriode(MammaStandplaatsPeriode periode)
		{
			this.periode = ModelUtil.sModel(periode);
		}

		@Override
		public MammaStandplaatsPeriode getStandplaatsPeriode()
		{
			return ModelUtil.nullSafeGet(periode);
		}

		@Override
		public Boolean getActief()
		{
			return getTehuis().getActief();
		}

		@Override
		public void setActief(Boolean actief)
		{
			getTehuis().setActief(actief);
		}

		@Override
		public void detach()
		{
			ModelUtil.nullSafeDetach(tehuis);
			ModelUtil.nullSafeDetach(periode);
		}

	}

	@Override
	public Iterator<? extends IMammaTehuisDto> iterator(long first, long count)
	{
		return tehuisService.zoekTehuizen(criteria.getObject().getTehuis(), criteria.getObject().getRegio(), Ints.checkedCast(first), Ints.checkedCast(count),
			getSort().getProperty(), getSort().isAscending(), new Callable<IMammaTehuisDto>()
			{

				@Override
				public IMammaTehuisDto call()
				{
					return new MammaTehuisDto();
				}
			}).iterator();
	}

	@Override
	public long size()
	{
		return tehuisService.countTehuizen(criteria.getObject().getTehuis(), criteria.getObject().getRegio());
	}

	@Override
	public IModel<IMammaTehuisDto> model(IMammaTehuisDto object)
	{
		return Model.of(object);
	}

	@Override
	public void detach()
	{
		super.detach();
		ModelUtil.nullSafeDetach(criteria);
	}
}
