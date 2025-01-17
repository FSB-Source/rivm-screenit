package nl.rivm.screenit.main.dto.mamma;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.main.service.mamma.IMammaTehuisDto;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode;
import nl.rivm.screenit.model.mamma.MammaTehuis;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.model.IDetachable;
import org.apache.wicket.model.IModel;

public class MammaTehuisDto implements IMammaTehuisDto, IDetachable
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
