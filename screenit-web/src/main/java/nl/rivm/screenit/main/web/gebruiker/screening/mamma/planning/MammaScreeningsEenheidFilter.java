package nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning;

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

import nl.rivm.screenit.model.IActief;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.model.IDetachable;
import org.apache.wicket.model.IModel;

public class MammaScreeningsEenheidFilter implements IActief, IDetachable
{
	private static final long serialVersionUID = 1L;

	private final IModel<MammaScreeningsEenheid> screeningsEenheid = ModelUtil.sModel(new MammaScreeningsEenheid());

	private IModel<ScreeningOrganisatie> regio;

	public MammaScreeningsEenheid getScreeningsEenheid()
	{
		return ModelUtil.nullSafeGet(screeningsEenheid);
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
		return getScreeningsEenheid().getActief();
	}

	@Override
	public void setActief(Boolean actief)
	{
		getScreeningsEenheid().setActief(actief);
	}

	@Override
	public void detach()
	{
		ModelUtil.nullSafeDetach(regio);
		ModelUtil.nullSafeDetach(screeningsEenheid);
	}
}
