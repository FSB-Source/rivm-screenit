package nl.rivm.screenit.main.web.gebruiker.screening.mamma.exchange.followup;

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

import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.SortState;
import nl.rivm.screenit.model.enums.MammaFollowUpDoorverwezenFilterOptie;
import nl.rivm.screenit.model.mamma.MammaFollowUpRadiologieVerslag;
import nl.rivm.screenit.service.mamma.MammaBaseFollowUpService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.extensions.markup.html.repeater.data.sort.SortOrder;
import org.apache.wicket.extensions.markup.html.repeater.util.SortableDataProvider;
import org.apache.wicket.injection.Injector;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

import com.google.common.primitives.Ints;

public class MammaFollowUpRadiologieInstellingProvider extends SortableDataProvider<MammaFollowUpRadiologieVerslag, String>
{
	@SpringBean
	private MammaBaseFollowUpService followUpService;

	private IModel<Instelling> instellingModel;

	private IModel<MammaFollowUpDoorverwezenFilterOptie> doorverwezenFilterOptieModel;

	MammaFollowUpRadiologieInstellingProvider(IModel<Instelling> instellingModel, IModel<MammaFollowUpDoorverwezenFilterOptie> doorverwezenFilterOptieModel)
	{
		Injector.get().inject(this);
		setSort("aangemaaktOp", SortOrder.ASCENDING);
		this.instellingModel = instellingModel;
		this.doorverwezenFilterOptieModel = doorverwezenFilterOptieModel;
	}

	@Override
	public Iterator<? extends MammaFollowUpRadiologieVerslag> iterator(long first, long count)
	{
		return followUpService
			.zoekRadiologieVerslagen(ModelUtil.nullSafeGet(instellingModel), ModelUtil.nullSafeGet(doorverwezenFilterOptieModel), Ints.checkedCast(first), Ints.checkedCast(count),
				new SortState<String>(getSort().getProperty(), getSort().isAscending()))
			.iterator();
	}

	@Override
	public long size()
	{
		return followUpService.countRadiologieVerslagen(ModelUtil.nullSafeGet(instellingModel), ModelUtil.nullSafeGet(doorverwezenFilterOptieModel));
	}

	@Override
	public IModel<MammaFollowUpRadiologieVerslag> model(MammaFollowUpRadiologieVerslag object)
	{
		return ModelUtil.sModel(object);
	}

	@Override
	public void detach()
	{
		super.detach();
		ModelUtil.nullSafeDetach(instellingModel);
		ModelUtil.nullSafeDetach(doorverwezenFilterOptieModel);
	}
}
