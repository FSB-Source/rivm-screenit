package nl.rivm.screenit.main.web.gebruiker.screening.mamma.followup.followuppathologie;

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

import nl.rivm.screenit.main.service.mamma.MammaFollowUpService;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.mamma.MammaFollowUpRadiologieVerslag;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.extensions.markup.html.repeater.data.sort.SortOrder;
import org.apache.wicket.extensions.markup.html.repeater.util.SortableDataProvider;
import org.apache.wicket.injection.Injector;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

import static nl.rivm.screenit.main.util.WicketSpringDataUtil.toSpringSort;

public class MammaFollowUpPathologieProvider extends SortableDataProvider<MammaFollowUpRadiologieVerslag, String>
{
	@SpringBean
	private MammaFollowUpService followUpService;

	private IModel<Instelling> instellingModel;

	MammaFollowUpPathologieProvider(IModel<Instelling> instellingModel)
	{
		Injector.get().inject(this);
		setSort("ingevoerdOp", SortOrder.ASCENDING);
		this.instellingModel = instellingModel;
	}

	@Override
	public Iterator<? extends MammaFollowUpRadiologieVerslag> iterator(long first, long count)
	{
		return followUpService.zoekDossiersMetOpenstaandePaVerslagen(ModelUtil.nullSafeGet(instellingModel), first, count,
			toSpringSort(getSort())).iterator();
	}

	@Override
	public long size()
	{
		return followUpService.countDossiersMetOpenstaandePaVerslagen(ModelUtil.nullSafeGet(instellingModel));
	}

	@Override
	public IModel<MammaFollowUpRadiologieVerslag> model(MammaFollowUpRadiologieVerslag followUpRadiologieVerslag)
	{
		return ModelUtil.sModel(followUpRadiologieVerslag);
	}

}
