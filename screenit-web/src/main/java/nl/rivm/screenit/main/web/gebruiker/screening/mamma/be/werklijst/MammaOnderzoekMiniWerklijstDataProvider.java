package nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.werklijst;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import nl.topicuszorg.hibernate.object.model.HibernateObject;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.extensions.markup.html.repeater.util.SortableDataProvider;
import org.apache.wicket.injection.Injector;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class MammaOnderzoekMiniWerklijstDataProvider<T extends HibernateObject> extends SortableDataProvider<T, String>
{
	private static final long serialVersionUID = 1L;

	@SpringBean
	private HibernateService hibernateService;

	private Long huidigeBeoordelingId;

	private List<Long> beoordelingenIds;

	private int openVerslag = 0;

	private Class<T> clazz;

	public MammaOnderzoekMiniWerklijstDataProvider(Long huidigeBeoordelingId, List<Long> beoordelingenIds, Class<T> clazz)
	{
		this.clazz = clazz;
		Injector.get().inject(this);
		this.huidigeBeoordelingId = huidigeBeoordelingId;
		this.beoordelingenIds = beoordelingenIds;
	}

	@Override
	public Iterator<? extends T> iterator(long first, long count)
	{
		List<T> beoordelingen = new ArrayList<>();
		for (Long id : getSublist())
		{
			beoordelingen.add(hibernateService.get(clazz, id));
		}
		return beoordelingen.iterator();
	}

	@Override
	public long size()
	{
		return beoordelingenIds.size();
	}

	@Override
	public IModel<T> model(T object)
	{
		return ModelUtil.sModel(object);
	}

	public int getOpenVerslag()
	{
		return openVerslag;
	}

	private List<Long> getSublist()
	{
		int huidigeBeoordelingIndex = beoordelingenIds.indexOf(huidigeBeoordelingId);

		int eerste = Math.max(huidigeBeoordelingIndex - 2, 0);
		int laatste = Math.min(huidigeBeoordelingIndex + 3, beoordelingenIds.size());

		List<Long> sublist = beoordelingenIds.subList(eerste, laatste);

		openVerslag = sublist.indexOf(huidigeBeoordelingId);

		return sublist;
	}
}
