package nl.rivm.screenit.main.web.gebruiker.screening.mamma.kwaliteitscontrole.fotobespreking;

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

import nl.rivm.screenit.main.model.mamma.beoordeling.MammaFotobesprekingOnderzoekenWerklijstZoekObject;
import nl.rivm.screenit.main.service.mamma.MammaKwaliteitscontroleService;
import nl.rivm.screenit.model.mamma.MammaFotobesprekingOnderzoek;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.extensions.markup.html.repeater.data.sort.SortOrder;
import org.apache.wicket.extensions.markup.html.repeater.util.SortableDataProvider;
import org.apache.wicket.injection.Injector;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

import com.google.common.primitives.Ints;

public class MammaFotobesprekingOnderzoekenDataProvider extends SortableDataProvider<MammaFotobesprekingOnderzoek, String>
{
	@SpringBean
	private MammaKwaliteitscontroleService kwaliteitscontroleService;

	private IModel<MammaFotobesprekingOnderzoekenWerklijstZoekObject> criteria;

	MammaFotobesprekingOnderzoekenDataProvider(String sortProperty, IModel<MammaFotobesprekingOnderzoekenWerklijstZoekObject> criteria)
	{
		Injector.get().inject(this);
		setSort(sortProperty, SortOrder.ASCENDING);
		this.criteria = criteria;
	}

	@Override
	public Iterator<? extends MammaFotobesprekingOnderzoek> iterator(long first, long count)
	{
		MammaFotobesprekingOnderzoekenWerklijstZoekObject zoekObject = getZoekObject();

		return kwaliteitscontroleService
			.zoekFotobesprekingOnderzoeken(zoekObject, Ints.checkedCast(first), Ints.checkedCast(count), getSort().getProperty(), getSort().isAscending()).iterator();
	}

	private MammaFotobesprekingOnderzoekenWerklijstZoekObject getZoekObject()
	{
		return ModelUtil.nullSafeGet(criteria);
	}

	@Override
	public long size()
	{
		MammaFotobesprekingOnderzoekenWerklijstZoekObject zoekObject = getZoekObject();
		return kwaliteitscontroleService.countFotobesprekingOnderzoeken(zoekObject);
	}

	@Override
	public IModel<MammaFotobesprekingOnderzoek> model(MammaFotobesprekingOnderzoek object)
	{
		return ModelUtil.sModel(object);
	}

	@Override
	public void detach()
	{
		super.detach();
		ModelUtil.nullSafeDetach(criteria);
	}
}
