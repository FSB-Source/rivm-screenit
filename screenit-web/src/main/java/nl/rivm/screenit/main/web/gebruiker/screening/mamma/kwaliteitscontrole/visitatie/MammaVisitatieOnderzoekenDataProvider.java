package nl.rivm.screenit.main.web.gebruiker.screening.mamma.kwaliteitscontrole.visitatie;

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

import nl.rivm.screenit.main.model.mamma.beoordeling.MammaVisitatieOnderzoekenWerklijstZoekObject;
import nl.rivm.screenit.main.service.mamma.MammaVisitatieService;
import nl.rivm.screenit.model.mamma.MammaVisitatieOnderzoek;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.extensions.markup.html.repeater.data.sort.SortOrder;
import org.apache.wicket.extensions.markup.html.repeater.util.SortableDataProvider;
import org.apache.wicket.injection.Injector;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

import static nl.rivm.screenit.main.util.WicketSpringDataUtil.toSpringSort;

public class MammaVisitatieOnderzoekenDataProvider extends SortableDataProvider<MammaVisitatieOnderzoek, String>
{
	@SpringBean
	private MammaVisitatieService visitatieService;

	private final IModel<MammaVisitatieOnderzoekenWerklijstZoekObject> zoekObject;

	MammaVisitatieOnderzoekenDataProvider(String sortProperty, IModel<MammaVisitatieOnderzoekenWerklijstZoekObject> zoekObject)
	{
		Injector.get().inject(this);
		setSort(sortProperty, SortOrder.ASCENDING);
		this.zoekObject = zoekObject;
	}

	@Override
	public Iterator<? extends MammaVisitatieOnderzoek> iterator(long first, long count)
	{
		return visitatieService.zoekVisitatieOnderzoeken(getZoekObject().getOnderdeel(), getZoekObject().getVisitatie(), first, count, toSpringSort(getSort())).iterator();
	}

	@Override
	public long size()
	{
		return visitatieService.countVisitatieOnderzoeken(getZoekObject().getOnderdeel(), getZoekObject().getVisitatie());
	}

	@Override
	public IModel<MammaVisitatieOnderzoek> model(MammaVisitatieOnderzoek object)
	{
		return ModelUtil.sModel(object);
	}

	@Override
	public void detach()
	{
		super.detach();
		ModelUtil.nullSafeDetach(zoekObject);
	}

	private MammaVisitatieOnderzoekenWerklijstZoekObject getZoekObject()
	{
		return ModelUtil.nullSafeGet(zoekObject);
	}
}
