package nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.tehuis;

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

import com.google.common.primitives.Ints;
import java.util.Iterator;
import nl.rivm.screenit.main.service.mamma.MammaTehuisAdresService;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.mamma.MammaTehuisAdres;
import nl.rivm.screenit.service.mamma.enums.MammaTehuisSelectie;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import org.apache.wicket.extensions.markup.html.repeater.data.sort.SortOrder;
import org.apache.wicket.extensions.markup.html.repeater.util.SortableDataProvider;
import org.apache.wicket.injection.Injector;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class MammaTehuisClientenDataProvider extends SortableDataProvider<Client, String>
{
	@SpringBean
	private MammaTehuisAdresService tehuisAdresService;

	private IModel<MammaTehuisAdres> zoekAdresModel;

	public MammaTehuisClientenDataProvider(String sortProperty, IModel<MammaTehuisAdres> zoekAdresModel)
	{
		Injector.get().inject(this);
		setSort(sortProperty, SortOrder.ASCENDING);
		this.zoekAdresModel = zoekAdresModel;
	}

	@Override
	public Iterator<? extends Client> iterator(long first, long count)
	{
		MammaTehuisAdres zoekAdres = zoekAdresModel.getObject();
		return tehuisAdresService
			.getTehuisAdresClienten(zoekAdres.getTehuis(), zoekAdres, Ints.checkedCast(first), Ints.checkedCast(count), getSort().getProperty(), getSort().isAscending())
			.iterator();
	}

	@Override
	public long size()
	{
		MammaTehuisAdres zoekAdres = zoekAdresModel.getObject();
		return tehuisAdresService.countClienten(zoekAdres.getTehuis(), MammaTehuisSelectie.TEHUIS_ADRES, zoekAdres);
	}

	@Override
	public IModel<Client> model(Client client)
	{
		return ModelUtil.sModel(client);
	}

	@Override
	public void detach()
	{
		super.detach();
		ModelUtil.nullSafeDetach(zoekAdresModel);
	}
}
