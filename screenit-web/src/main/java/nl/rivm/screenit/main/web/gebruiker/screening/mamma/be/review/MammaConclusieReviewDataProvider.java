package nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.review;

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

import nl.rivm.screenit.main.model.mamma.beoordeling.MammaConclusieReviewZoekObject;
import nl.rivm.screenit.main.service.mamma.impl.MammaConclusieReviewDataProviderServiceImpl;
import nl.rivm.screenit.main.util.WicketSpringDataUtil;
import nl.rivm.screenit.model.mamma.MammaConclusieReview;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.extensions.markup.html.repeater.data.sort.SortOrder;
import org.apache.wicket.extensions.markup.html.repeater.util.SortableDataProvider;
import org.apache.wicket.injection.Injector;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

import com.google.common.primitives.Ints;

public class MammaConclusieReviewDataProvider extends SortableDataProvider<MammaConclusieReview, String>
{
	@SpringBean
	private MammaConclusieReviewDataProviderServiceImpl conclusieReviewDataProviderService;

	private final IModel<MammaConclusieReviewZoekObject> conclusieFilterOptieModel;

	MammaConclusieReviewDataProvider(IModel<MammaConclusieReviewZoekObject> conclusieFilterOptieModel)
	{
		Injector.get().inject(this);
		setSort("screeningRonde.laatsteOnderzoek.creatieDatum", SortOrder.ASCENDING);
		this.conclusieFilterOptieModel = conclusieFilterOptieModel;
	}

	@Override
	public Iterator<MammaConclusieReview> iterator(long first, long count)
	{
		var sort = WicketSpringDataUtil.toSpringSort(getSort());

		var lijstOnderzoeken = conclusieReviewDataProviderService.findPage(Ints.checkedCast(first), Ints.checkedCast(count), getZoekObject(),
			sort);
		return lijstOnderzoeken.iterator();
	}

	private MammaConclusieReviewZoekObject getZoekObject()
	{
		return ModelUtil.nullSafeGet(conclusieFilterOptieModel);
	}

	@Override
	public long size()
	{
		return conclusieReviewDataProviderService.size(getZoekObject());
	}

	@Override
	public IModel<MammaConclusieReview> model(MammaConclusieReview object)
	{
		return ModelUtil.sModel(object);
	}

	@Override
	public void detach()
	{
		super.detach();
		ModelUtil.nullSafeDetach(conclusieFilterOptieModel);
	}

}
