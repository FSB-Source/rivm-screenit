
package nl.rivm.screenit.main.web.gebruiker.algemeen.medewerker;

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

import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import nl.rivm.screenit.main.service.MedewerkerService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.model.Functie;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.Rol;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.enums.ToegangLevel;
import nl.rivm.screenit.service.OrganisatieZoekService;
import nl.rivm.screenit.service.ScopeService;
import nl.topicuszorg.wicket.hibernate.SimpleHibernateModel;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.extensions.markup.html.repeater.data.sort.SortOrder;
import org.apache.wicket.extensions.markup.html.repeater.util.SortableDataProvider;
import org.apache.wicket.injection.Injector;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class MedewerkerDataProvider extends SortableDataProvider<Gebruiker, String>
{

	private static final long serialVersionUID = 1L;

	@SpringBean
	private MedewerkerService medewerkerService;

	@SpringBean
	private ScopeService scopeService;

	@SpringBean
	private OrganisatieZoekService organisatieZoekService;

	private IModel<Gebruiker> criteria;

	private final IModel<List<Functie>> selectedFuncties;

	private final IModel<List<Rol>> selectedRollen;

	private final boolean checkOpHierarchieScope;

	private final boolean checkOpBevolkingsonderzoeken;

	public MedewerkerDataProvider(String sortProperty, IModel<Gebruiker> criteria, IModel<List<Functie>> selectedFuncties, IModel<List<Rol>> selectedRollen,
		boolean checkOpHierarchieScope, boolean checkOpBevolkingsonderzoeken)
	{
		this.selectedFuncties = selectedFuncties;
		this.selectedRollen = selectedRollen;
		this.checkOpHierarchieScope = checkOpHierarchieScope;
		this.checkOpBevolkingsonderzoeken = checkOpBevolkingsonderzoeken;
		Injector.get().inject(this);
		setSort(sortProperty, SortOrder.ASCENDING);
		this.criteria = criteria;
	}

	public MedewerkerDataProvider(String sortProperty, IModel<Gebruiker> criteria, boolean bevolkingsonderzoekenCheck)
	{
		this(sortProperty, criteria, null, null, false, bevolkingsonderzoekenCheck);
	}

	@Override
	public Iterator<? extends Gebruiker> iterator(long first, long count)
	{
		InstellingGebruiker loggedInInstellingGebruiker = ScreenitSession.get().getLoggedInInstellingGebruiker();
		Gebruiker zoekObject = getZoekObject();
		ToegangLevel toeganglevel = ScreenitSession.get().getToegangsLevel(Actie.INZIEN, Recht.GEBRUIKER_MEDEWERKER_BEHEER);
		if (ToegangLevel.EIGEN.equals(toeganglevel))
		{
			zoekObject.setId(loggedInInstellingGebruiker.getMedewerker().getId());
		}
		List<Bevolkingsonderzoek> bevolkingsonderzoeken = null;
		if (checkOpBevolkingsonderzoeken)
		{
			bevolkingsonderzoeken = loggedInInstellingGebruiker.getBevolkingsonderzoeken();
		}
		List<Gebruiker> searchMedewerkers = medewerkerService.searchMedewerkers(zoekObject, ModelUtil.nullSafeGet(selectedFuncties), ModelUtil.nullSafeGet(selectedRollen),
			getHierarchieCriteria(), bevolkingsonderzoeken, (int) first, (int) count, getSort().getProperty(), getSort().isAscending());
		zoekObject.setId(null);
		return searchMedewerkers.iterator();
	}

	private Map<OrganisatieType, List<Instelling>> getHierarchieCriteria()
	{
		Map<OrganisatieType, List<Instelling>> hierarchieCriteria = new HashMap<>();
		if (checkOpHierarchieScope)
		{
			ToegangLevel toegangsLevel = ScreenitSession.get().getToegangsLevel(Actie.INZIEN, Recht.GEBRUIKER_MEDEWERKER_BEHEER);
			InstellingGebruiker loggedInInstellingGebruiker = ScreenitSession.get().getLoggedInInstellingGebruiker();
			boolean addToHierarchieCriteria = false;
			for (OrganisatieType type : OrganisatieType.values())
			{
				switch (type)
				{
				case RIVM:
				case INPAKCENTRUM:
				case LABORATORIUM:
					addToHierarchieCriteria = toegangsLevel.getNiveau() == ToegangLevel.LANDELIJK.getNiveau();
					break;
				case SCREENINGSORGANISATIE:
					addToHierarchieCriteria = toegangsLevel.getNiveau() >= ToegangLevel.REGIO.getNiveau();
					break;
				default:
					addToHierarchieCriteria = toegangsLevel.getNiveau() >= ToegangLevel.INSTELLING.getNiveau();
					break;
				}
				if (addToHierarchieCriteria)
				{
					hierarchieCriteria.put(type, organisatieZoekService.getOrganisatiesForNiveau(loggedInInstellingGebruiker, type, toegangsLevel));
				}
			}
		}
		return hierarchieCriteria;
	}

	private Gebruiker getZoekObject()
	{
		return ModelUtil.nullSafeGet(criteria);
	}

	@Override
	public long size()
	{
		InstellingGebruiker loggedInInstellingGebruiker = ScreenitSession.get().getLoggedInInstellingGebruiker();
		Gebruiker zoekObject = getZoekObject();
		ToegangLevel toegangsLevel = ScreenitSession.get().getToegangsLevel(Actie.INZIEN, Recht.GEBRUIKER_MEDEWERKER_BEHEER);
		if (ToegangLevel.EIGEN.equals(toegangsLevel))
		{
			zoekObject.setId(loggedInInstellingGebruiker.getMedewerker().getId());
		}
		List<Bevolkingsonderzoek> bevolkingsonderzoeken = null;
		if (checkOpBevolkingsonderzoeken)
		{
			bevolkingsonderzoeken = loggedInInstellingGebruiker.getBevolkingsonderzoeken();
		}
		long countMedewerkers = medewerkerService.countMedewerkers(getZoekObject(), ModelUtil.nullSafeGet(selectedFuncties), ModelUtil.nullSafeGet(selectedRollen),
			getHierarchieCriteria(), bevolkingsonderzoeken);
		zoekObject.setId(null); 
		return countMedewerkers;
	}

	@Override
	public IModel<Gebruiker> model(Gebruiker object)
	{
		return new SimpleHibernateModel<Gebruiker>(object);
	}

	@Override
	public void detach()
	{
		ModelUtil.nullSafeDetach(criteria);
		ModelUtil.nullSafeDetach(selectedFuncties);
		ModelUtil.nullSafeDetach(selectedRollen);
	}
}
