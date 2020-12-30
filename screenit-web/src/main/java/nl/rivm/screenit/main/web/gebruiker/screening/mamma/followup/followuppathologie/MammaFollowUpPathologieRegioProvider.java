package nl.rivm.screenit.main.web.gebruiker.screening.mamma.followup.followuppathologie;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.dto.mamma.MammaFollowUpInstellingDto;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.service.mamma.MammaBaseFollowUpService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import org.apache.wicket.extensions.markup.html.repeater.data.sort.SortOrder;
import org.apache.wicket.extensions.markup.html.repeater.util.SortableDataProvider;
import org.apache.wicket.injection.Injector;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.springframework.beans.support.PropertyComparator;

import java.util.Iterator;
import java.util.List;

public class MammaFollowUpPathologieRegioProvider extends SortableDataProvider<MammaFollowUpInstellingDto, String>
{
	@SpringBean
	private MammaBaseFollowUpService followUpService;

	private IModel<ScreeningOrganisatie> regioModel;

	private List<MammaFollowUpInstellingDto> instellingList;

	MammaFollowUpPathologieRegioProvider(IModel<ScreeningOrganisatie> regioModel)
	{
		Injector.get().inject(this);
		setSort("instellingNaam", SortOrder.ASCENDING);
		this.regioModel = regioModel;
	}

	@Override
	public Iterator<? extends MammaFollowUpInstellingDto> iterator(long first, long count)
	{
		setList();
		return instellingList.stream().sorted(new PropertyComparator<>(getSort().getProperty(), true, getSort().isAscending())).skip(first).limit(count)
			.iterator();
	}

	@Override
	public long size()
	{
		setList();
		return instellingList.size();
	}

	@Override
	public IModel<MammaFollowUpInstellingDto> model(MammaFollowUpInstellingDto mammaFollowUpRadiologieVerslag)
	{
		return Model.of(mammaFollowUpRadiologieVerslag);
	}

	private void setList()
	{
		if (instellingList == null)
		{
			instellingList = followUpService.zoekInstellingenMetOpenstaandePaVerslagen(ModelUtil.nullSafeGet(regioModel));
		}
	}

	@Override
	public void detach()
	{
		super.detach();
		ModelUtil.nullSafeDetach(regioModel);
	}
}
