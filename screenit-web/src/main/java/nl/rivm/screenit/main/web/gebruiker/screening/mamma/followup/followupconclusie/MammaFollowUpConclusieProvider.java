package nl.rivm.screenit.main.web.gebruiker.screening.mamma.followup.followupconclusie;

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

import nl.rivm.screenit.main.service.mamma.MammaFollowUpService;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaBeoordeling_;
import nl.rivm.screenit.model.mamma.MammaDossier_;
import nl.rivm.screenit.model.mamma.MammaOnderzoek_;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.extensions.markup.html.repeater.data.sort.SortOrder;
import org.apache.wicket.extensions.markup.html.repeater.util.SortableDataProvider;
import org.apache.wicket.injection.Injector;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

import static nl.rivm.screenit.main.util.WicketSpringDataUtil.toSpringSort;
import static nl.rivm.screenit.util.StringUtil.propertyChain;

public class MammaFollowUpConclusieProvider extends SortableDataProvider<MammaBeoordeling, String>
{
	@SpringBean
	private MammaFollowUpService followUpService;

	private final IModel<ScreeningOrganisatie> regioModel;

	MammaFollowUpConclusieProvider(IModel<ScreeningOrganisatie> regioModel)
	{
		Injector.get().inject(this);
		setSort(propertyChain(MammaDossier_.LAATSTE_BEOORDELING_MET_UITSLAG, MammaBeoordeling_.ONDERZOEK, MammaOnderzoek_.CREATIE_DATUM), SortOrder.ASCENDING);
		this.regioModel = regioModel;
	}

	@Override
	public Iterator<? extends MammaBeoordeling> iterator(long first, long count)
	{
		return followUpService.zoekOpenstaandeFollowUpConclusies(ModelUtil.nullSafeGet(regioModel), first, count, toSpringSort(getSort())).iterator();
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
