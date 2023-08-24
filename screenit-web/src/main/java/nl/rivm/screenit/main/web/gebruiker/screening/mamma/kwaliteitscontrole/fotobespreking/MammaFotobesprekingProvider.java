package nl.rivm.screenit.main.web.gebruiker.screening.mamma.kwaliteitscontrole.fotobespreking;

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

import nl.rivm.screenit.main.model.mamma.beoordeling.MammaFotobesprekingWerklijstZoekObject;
import nl.rivm.screenit.main.service.mamma.MammaKwaliteitscontroleService;
import nl.rivm.screenit.model.mamma.MammaFotobespreking;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.extensions.markup.html.repeater.data.sort.SortOrder;
import org.apache.wicket.extensions.markup.html.repeater.util.SortableDataProvider;
import org.apache.wicket.injection.Injector;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

import com.google.common.primitives.Ints;

public class MammaFotobesprekingProvider extends SortableDataProvider<MammaFotobespreking, String>
{
	@SpringBean
	private MammaKwaliteitscontroleService kwaliteitscontroleService;

	private IModel<MammaFotobesprekingWerklijstZoekObject> zoekModel;

	public MammaFotobesprekingProvider(IModel<MammaFotobesprekingWerklijstZoekObject> zoekModel)
	{
		Injector.get().inject(this);
		setSort("aangemaaktOp", SortOrder.ASCENDING);
		this.zoekModel = zoekModel;
	}

	@Override
	public Iterator<MammaFotobespreking> iterator(long first, long count)
	{
		String sortProperty = null;
		boolean asc = true;
		if (getSort() != null)
		{
			sortProperty = getSort().getProperty();
			asc = getSort().isAscending();
		}
		return kwaliteitscontroleService.zoekFotobesprekingen(ModelUtil.nullSafeGet(zoekModel), Ints.checkedCast(first), Ints.checkedCast(count), sortProperty, asc).iterator();
	}

	@Override
	public long size()
	{
		return kwaliteitscontroleService.countFotobesprekingen(ModelUtil.nullSafeGet(zoekModel));
	}

	@Override
	public IModel<MammaFotobespreking> model(MammaFotobespreking object)
	{
		return ModelUtil.sModel(object);
	}

	@Override
	public void detach()
	{
		super.detach();
		ModelUtil.nullSafeDetach(zoekModel);
	}
}
