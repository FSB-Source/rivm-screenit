package nl.rivm.screenit.main.web.gebruiker.screening.mamma.ce.werklijst.uploadbeeldenverzoeken;

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

import nl.rivm.screenit.dto.mamma.MammaUploadBeeldenVerzoekDto;
import nl.rivm.screenit.main.service.mamma.MammaUploadBeeldenService;
import nl.rivm.screenit.model.ScreeningOrganisatie;
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

public class MammaCeUploadBeeldenVerzoekDataProvider extends SortableDataProvider<MammaUploadBeeldenVerzoekDto, String>
{
	@SpringBean
	private MammaUploadBeeldenService uploadBeeldenService;

	private List<MammaUploadBeeldenVerzoekDto> instellingList;

	private IModel<ScreeningOrganisatie> regioModel;

	public MammaCeUploadBeeldenVerzoekDataProvider(IModel<ScreeningOrganisatie> regio)
	{
		Injector.get().inject(this);
		setSort("instellingNaam", SortOrder.ASCENDING);
		this.regioModel = regio;
	}

	@Override
	public Iterator<? extends MammaUploadBeeldenVerzoekDto> iterator(long first, long count)
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

	private void setList()
	{
		if (instellingList == null)
		{
			instellingList = uploadBeeldenService.zoekInstellingenMetOpenstaandeUploadVerzoeken(ModelUtil.nullSafeGet(regioModel));
		}
	}

	public void resetList()
	{
		instellingList = null;
	}

	@Override
	public IModel<MammaUploadBeeldenVerzoekDto> model(MammaUploadBeeldenVerzoekDto mammaUploadBeeldenVerzoekDto)
	{
		return Model.of(mammaUploadBeeldenVerzoekDto);
	}

	@Override
	public void detach()
	{
		super.detach();
		ModelUtil.nullSafeDetach(regioModel);
	}
}
