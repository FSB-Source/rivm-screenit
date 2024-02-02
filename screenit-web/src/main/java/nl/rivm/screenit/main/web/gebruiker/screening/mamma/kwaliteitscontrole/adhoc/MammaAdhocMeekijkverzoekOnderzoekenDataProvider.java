package nl.rivm.screenit.main.web.gebruiker.screening.mamma.kwaliteitscontrole.adhoc;

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

import nl.rivm.screenit.main.model.mamma.beoordeling.MammaAdhocMeekijkverzoekWerklijstZoekObject;
import nl.rivm.screenit.main.service.mamma.MammaKwaliteitscontroleService;
import nl.rivm.screenit.model.SortState;
import nl.rivm.screenit.model.mamma.MammaAdhocMeekijkverzoek;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.extensions.markup.html.repeater.data.sort.SortOrder;
import org.apache.wicket.extensions.markup.html.repeater.util.SortableDataProvider;
import org.apache.wicket.injection.Injector;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

import com.google.common.primitives.Ints;

public class MammaAdhocMeekijkverzoekOnderzoekenDataProvider extends SortableDataProvider<MammaAdhocMeekijkverzoek, String>
{
	@SpringBean
	private MammaKwaliteitscontroleService kwaliteitscontroleService;

	private IModel<MammaAdhocMeekijkverzoekWerklijstZoekObject> zoekObjectModel;

	MammaAdhocMeekijkverzoekOnderzoekenDataProvider(IModel<MammaAdhocMeekijkverzoekWerklijstZoekObject> zoekObjectModel)
	{
		this.zoekObjectModel = zoekObjectModel;
		Injector.get().inject(this);
		setSort("onderzoek.creatieDatum", SortOrder.DESCENDING);
	}

	@Override
	public Iterator<? extends MammaAdhocMeekijkverzoek> iterator(long first, long count)
	{
		return kwaliteitscontroleService
			.zoekAdhocMeekijkverzoekOnderzoeken(ModelUtil.nullSafeGet(zoekObjectModel), Ints.checkedCast(first), Ints.checkedCast(count),
				new SortState<String>(getSort().getProperty(), getSort().isAscending()))
			.iterator();
	}

	@Override
	public long size()
	{
		return kwaliteitscontroleService.countAdhocMeekijkverzoekOnderzoeken(ModelUtil.nullSafeGet(zoekObjectModel));
	}

	@Override
	public IModel<MammaAdhocMeekijkverzoek> model(MammaAdhocMeekijkverzoek object)
	{
		return ModelUtil.sModel(object);
	}

	@Override
	public void detach()
	{
		super.detach();
		ModelUtil.nullSafeDetach(zoekObjectModel);
	}
}
