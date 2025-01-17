package nl.rivm.screenit.main.web.gebruiker.screening.cervix.monster;

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

import java.util.Iterator;
import java.util.List;

import nl.rivm.screenit.model.cervix.CervixUitnodiging;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.extensions.markup.html.repeater.util.SortableDataProvider;
import org.apache.wicket.model.IModel;

public class CervixUitnodigingenProvider extends SortableDataProvider<CervixUitnodiging, String>
{

	private static final long serialVersionUID = 1L;

	private IModel<List<CervixUitnodiging>> uitnodigingenModel;

	public CervixUitnodigingenProvider(List<CervixUitnodiging> uitnodigingen)
	{
		uitnodigingenModel = ModelUtil.listRModel(uitnodigingen, false);
	}

	@Override
	public Iterator<? extends CervixUitnodiging> iterator(long first, long count)
	{
		return uitnodigingenModel.getObject().iterator();
	}

	@Override
	public long size()
	{
		return uitnodigingenModel.getObject().size();
	}

	@Override
	public IModel<CervixUitnodiging> model(CervixUitnodiging uitnodiging)
	{
		return ModelUtil.sModel(uitnodiging);
	}

	@Override
	public void detach()
	{
		ModelUtil.nullSafeDetach(uitnodigingenModel);
	}
}
