package nl.rivm.screenit.main.web.gebruiker.screening.mamma.followup.followupconclusie;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.SortState;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.service.mamma.MammaBaseFollowUpService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import org.apache.wicket.extensions.markup.html.repeater.data.sort.SortOrder;
import org.apache.wicket.extensions.markup.html.repeater.util.SortableDataProvider;
import org.apache.wicket.injection.Injector;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

import java.util.Iterator;

public class MammaFollowUpConclusieProvider extends SortableDataProvider<MammaBeoordeling, String>
{
	@SpringBean
	private MammaBaseFollowUpService followUpService;

	private IModel<ScreeningOrganisatie> regioModel;

	MammaFollowUpConclusieProvider(IModel<ScreeningOrganisatie> regioModel)
	{
		Injector.get().inject(this);
		setSort("onderzoek.creatieDatum", SortOrder.ASCENDING);
		this.regioModel = regioModel;
	}

	@Override
	public Iterator<? extends MammaBeoordeling> iterator(long first, long count)
	{
		return followUpService.zoekOpenstaandeFollowUpConclusies(ModelUtil.nullSafeGet(regioModel), Ints.checkedCast(first), Ints.checkedCast(count),
			new SortState<>(getSort().getProperty(), getSort().isAscending())).iterator();
	}

	@Override
	public long size()
	{
		return followUpService.countOpenstaandeFollowUpConclusies(ModelUtil.nullSafeGet(regioModel));
	}

	@Override
	public IModel<MammaBeoordeling> model(MammaBeoordeling beoordeling)
	{
		return ModelUtil.sModel(beoordeling);
	}

	@Override
	public void detach()
	{
		super.detach();
		ModelUtil.nullSafeDetach(regioModel);
	}

}
