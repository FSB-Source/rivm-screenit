package nl.rivm.screenit.main.web.gebruiker.clienten.complicatie;

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

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.model.colon.Complicatie;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.SortState;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.enums.ToegangLevel;
import nl.rivm.screenit.service.colon.ComplicatieService;
import nl.rivm.screenit.service.OrganisatieZoekService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.extensions.markup.html.repeater.data.sort.SortOrder;
import org.apache.wicket.extensions.markup.html.repeater.util.SortableDataProvider;
import org.apache.wicket.injection.Injector;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class ComplicatieDataProvider extends SortableDataProvider<Complicatie, String>
{

	private static final long serialVersionUID = 1L;

	@SpringBean
	private ComplicatieService complicatieService;

	@SpringBean
	private OrganisatieZoekService organisatieZoekService;

	private IModel<Complicatie> criteria;

	public ComplicatieDataProvider(IModel<Complicatie> criteria)
	{
		Injector.get().inject(this);
		setSort("datum", SortOrder.DESCENDING);
		this.criteria = criteria;
	}

	@Override
	public Iterator<? extends Complicatie> iterator(long first, long count)
	{
		List<Long> hierarchieCriteria = getHierarchieCriteria();
		return complicatieService.getComplicaties(ModelUtil.nullSafeGet(criteria), hierarchieCriteria, (int) first, (int) count,
			new SortState<>(getSort().getProperty(), getSort().isAscending()));
	}

	@Override
	public long size()
	{
		List<Long> hierarchieCriteria = getHierarchieCriteria();
		return complicatieService.countComplicaties(ModelUtil.nullSafeGet(criteria), hierarchieCriteria);
	}

	@Override
	public IModel<Complicatie> model(Complicatie object)
	{
		return ModelUtil.sModel(object);
	}

	@Override
	public void detach()
	{
		super.detach();
		ModelUtil.nullSafeDetach(criteria);
	}

	private List<Long> getHierarchieCriteria()
	{
		List<Long> hierarchieCriteria = new ArrayList<Long>();

		ToegangLevel toegangsLevel = ScreenitSession.get().getToegangsLevel(Actie.INZIEN, Recht.GEBRUIKER_CLIENT_COMPLICATIE_REGISTREREN);

		InstellingGebruiker loggedInInstellingGebruiker = ScreenitSession.get().getLoggedInInstellingGebruiker();

		ArrayList<OrganisatieType> types = new ArrayList<OrganisatieType>();
		types.add(OrganisatieType.COLOSCOPIELOCATIE);

		if (toegangsLevel != null)
		{
			hierarchieCriteria = organisatieZoekService.getZichtbateInstellingenOpToegangLevel(loggedInInstellingGebruiker.getOrganisatie(), toegangsLevel, types);

			Instelling instelling = loggedInInstellingGebruiker.getOrganisatie();
			hierarchieCriteria.add(instelling.getId());
		}

		return hierarchieCriteria;
	}
}
