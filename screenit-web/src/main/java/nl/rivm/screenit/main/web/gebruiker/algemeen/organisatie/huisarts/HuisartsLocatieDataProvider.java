package nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.huisarts;

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

import java.util.Iterator;

import nl.rivm.screenit.huisartsenportaal.enums.CervixLocatieStatus;
import nl.rivm.screenit.main.dao.cervix.CervixHuisartsDao;
import nl.rivm.screenit.model.SortState;
import nl.rivm.screenit.model.cervix.CervixHuisarts;
import nl.rivm.screenit.model.cervix.CervixHuisartsLocatie;
import nl.rivm.screenit.model.cervix.SearchHuisartsLocatieDto;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.extensions.markup.html.repeater.data.sort.SortOrder;
import org.apache.wicket.extensions.markup.html.repeater.util.SortableDataProvider;
import org.apache.wicket.injection.Injector;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class HuisartsLocatieDataProvider extends SortableDataProvider<CervixHuisartsLocatie, String>
{
	@SpringBean
	private CervixHuisartsDao huisartsDao;

	private IModel<CervixHuisarts> huisartsIModel;

	private IModel<SearchHuisartsLocatieDto> searchLocatieModel;

	public HuisartsLocatieDataProvider(IModel<CervixHuisarts> huisartsIModel, IModel<SearchHuisartsLocatieDto> searchLocatieModel)
	{
		Injector.get().inject(this);
		setSort("naam", SortOrder.ASCENDING);
		this.huisartsIModel = huisartsIModel;
		this.searchLocatieModel = searchLocatieModel;
	}

	@Override
	public Iterator<? extends CervixHuisartsLocatie> iterator(long first, long count)
	{
		return huisartsDao.getCervixHuisartsLocatieVanHuisarts(getZoekObject(), first, count,
			new SortState<>(getSort().getProperty(), getSort().isAscending())).iterator();
	}

	private CervixHuisartsLocatie getZoekObject()
	{
		CervixHuisartsLocatie zoekObject = new CervixHuisartsLocatie();
		zoekObject.setStatus(getStatusFromActief(searchLocatieModel.getObject().getActief()));
		zoekObject.setHuisarts(huisartsIModel.getObject());
		return zoekObject;
	}

	private CervixLocatieStatus getStatusFromActief(Boolean actief)
	{
		if (actief != null)
		{
			if (actief)
			{
				return CervixLocatieStatus.ACTIEF;
			}
			return CervixLocatieStatus.INACTIEF;
		}
		return null;
	}

	@Override
	public long size()
	{
		return huisartsDao.getAantalCervixHuisartsLocatieVanHuisarts(getZoekObject());
	}

	@Override
	public IModel<CervixHuisartsLocatie> model(CervixHuisartsLocatie object)
	{
		return ModelUtil.ccModel(object);
	}

	@Override
	public void detach()
	{
		super.detach();
		ModelUtil.nullSafeDetach(searchLocatieModel);
		ModelUtil.nullSafeDetach(huisartsIModel);
	}
}
