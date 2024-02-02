package nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.tehuis;

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

import nl.rivm.screenit.model.IActief;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.mamma.MammaTehuis;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.model.IDetachable;
import org.apache.wicket.model.IModel;

public class MammaTehuisFilter implements IActief, IDetachable
{
	private static final long serialVersionUID = 1L;

	private final IModel<MammaTehuis> tehuis = ModelUtil.cModel(new MammaTehuis());

	private IModel<ScreeningOrganisatie> regio;

	public MammaTehuis getTehuis()
	{
		return ModelUtil.nullSafeGet(tehuis);
	}

	public ScreeningOrganisatie getRegio()
	{
		return ModelUtil.nullSafeGet(regio);
	}

	public void setRegio(ScreeningOrganisatie regio)
	{
		this.regio = ModelUtil.sModel(regio);
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
		ModelUtil.nullSafeDetach(regio);
		ModelUtil.nullSafeDetach(tehuis);
	}
}
