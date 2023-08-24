package nl.rivm.screenit.main.web.gebruiker.screening.cervix.labformulier.aanvragen;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.main.dao.cervix.CervixHuisartsDao;
import nl.rivm.screenit.model.SortState;
import nl.rivm.screenit.model.cervix.CervixHuisarts;
import nl.rivm.screenit.model.cervix.CervixLabformulierAanvraag;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.extensions.markup.html.repeater.data.sort.SortOrder;
import org.apache.wicket.extensions.markup.html.repeater.util.SortableDataProvider;
import org.apache.wicket.injection.Injector;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class CervixLabformulierAanvraagDataProvider extends SortableDataProvider<CervixLabformulierAanvraag, String>
{

	@SpringBean
	private CervixHuisartsDao huisartsDao;

	private IModel<CervixHuisarts> zoekObject;

	public CervixLabformulierAanvraagDataProvider(IModel<CervixHuisarts> zoekObject)
	{
		Injector.get().inject(this);
		setSort("aanvraagDatum", SortOrder.DESCENDING);
		this.zoekObject = zoekObject;
	}

	@Override
	public Iterator<? extends CervixLabformulierAanvraag> iterator(long first, long count)
	{
		return huisartsDao.getCervixLabformulierOrdersVanInstelling(ModelUtil.nullSafeGet(zoekObject), first, count,
			new SortState<>(getSort().getProperty(), getSort().isAscending())).iterator();
	}

	@Override
	public long size()
	{
		return huisartsDao.getAantalCervixLabformulierOrdersVanInstelling(ModelUtil.nullSafeGet(zoekObject));
	}

	@Override
	public IModel<CervixLabformulierAanvraag> model(CervixLabformulierAanvraag object)
	{
		return ModelUtil.sModel(object);
	}

	@Override
	public void detach()
	{
		super.detach();
		ModelUtil.nullSafeDetach(zoekObject);
	}
}
