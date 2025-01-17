package nl.rivm.screenit.main.web.gebruiker.algemeen.medewerker;

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
import java.util.List;

import nl.rivm.screenit.main.service.algemeen.MedewerkerZoekService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.model.Functie;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.Rol;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.enums.ToegangLevel;
import nl.topicuszorg.wicket.hibernate.SimpleHibernateModel;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.extensions.markup.html.repeater.data.sort.SortOrder;
import org.apache.wicket.extensions.markup.html.repeater.util.SortableDataProvider;
import org.apache.wicket.injection.Injector;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class MedewerkerDataProvider extends SortableDataProvider<Gebruiker, String>
{
	@SpringBean
	private MedewerkerZoekService medewerkerZoekService;

	private final IModel<Gebruiker> zoekObjectModel;

	private final IModel<List<Functie>> selectedFunctiesModel;

	private final IModel<List<Rol>> selectedRollenModel;

	private final boolean voorOrganisatieKoppelen;

	public MedewerkerDataProvider(String sortProperty, IModel<Gebruiker> zoekObjectModel, IModel<List<Functie>> selectedFunctiesModel, IModel<List<Rol>> selectedRollenModel,
		boolean voorOrganisatieKoppelen)
	{
		this.selectedFunctiesModel = selectedFunctiesModel;
		this.selectedRollenModel = selectedRollenModel;
		this.voorOrganisatieKoppelen = voorOrganisatieKoppelen;
		Injector.get().inject(this);
		setSort(sortProperty, SortOrder.ASCENDING);
		this.zoekObjectModel = zoekObjectModel;
	}

	public MedewerkerDataProvider(String sortProperty, IModel<Gebruiker> zoekObjectModel)
	{
		this(sortProperty, zoekObjectModel, null, null, true);
	}

	@Override
	public Iterator<? extends Gebruiker> iterator(long first, long count)
	{
		updateZoekObjectVoorZoekActie();
		var searchMedewerkers = medewerkerZoekService.searchMedewerkers(getZoekObject(), getSelectedFuncties(), getSelectedRollen(), getLoggedInInstellingGebruiker(),
			voorOrganisatieKoppelen, (int) first, (int) count, getSort().getProperty(), getSort().isAscending());
		updateZoekObjectNaZoekActie();
		return searchMedewerkers.iterator();
	}

	@Override
	public long size()
	{
		updateZoekObjectVoorZoekActie();
		var countMedewerkers = medewerkerZoekService.countMedewerkers(getZoekObject(), getSelectedFuncties(), getSelectedRollen(), getLoggedInInstellingGebruiker(),
			voorOrganisatieKoppelen);
		updateZoekObjectNaZoekActie();
		return countMedewerkers;
	}

	private void updateZoekObjectVoorZoekActie()
	{
		var toeganglevel = ScreenitSession.get().getToegangsLevel(Actie.INZIEN, Recht.GEBRUIKER_MEDEWERKER_BEHEER);
		if (ToegangLevel.EIGEN.equals(toeganglevel))
		{
			getZoekObject().setId(getLoggedInInstellingGebruiker().getMedewerker().getId());
		}
	}

	private InstellingGebruiker getLoggedInInstellingGebruiker()
	{
		return ScreenitSession.get().getLoggedInInstellingGebruiker();
	}

	private void updateZoekObjectNaZoekActie()
	{
		getZoekObject().setId(null); 
	}

	private Gebruiker getZoekObject()
	{
		return ModelUtil.nullSafeGet(zoekObjectModel);
	}

	private List<Rol> getSelectedRollen()
	{
		return ModelUtil.nullSafeGet(selectedRollenModel);
	}

	private List<Functie> getSelectedFuncties()
	{
		return ModelUtil.nullSafeGet(selectedFunctiesModel);
	}

	@Override
	public IModel<Gebruiker> model(Gebruiker object)
	{
		return new SimpleHibernateModel<>(object);
	}

	@Override
	public void detach()
	{
		ModelUtil.nullSafeDetach(zoekObjectModel);
		ModelUtil.nullSafeDetach(selectedFunctiesModel);
		ModelUtil.nullSafeDetach(selectedRollenModel);
	}
}
