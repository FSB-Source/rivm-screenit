package nl.rivm.screenit.main.dao.impl;

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

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import nl.rivm.screenit.dao.InstellingDao;
import nl.rivm.screenit.dao.OrganisatieZoekDao;
import nl.rivm.screenit.main.dao.MedewerkerDao;
import nl.rivm.screenit.model.Functie;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.Rol;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.collections.MapUtils;
import org.apache.commons.lang.StringUtils;
import org.hibernate.Criteria;
import org.hibernate.criterion.DetachedCriteria;
import org.hibernate.criterion.Disjunction;
import org.hibernate.criterion.Junction;
import org.hibernate.criterion.MatchMode;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.hibernate.criterion.Subqueries;
import org.hibernate.sql.JoinType;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

@Repository
public class MedewerkerDaoImpl extends AbstractAutowiredDao implements MedewerkerDao
{
	@Autowired
	private InstellingDao instellingDao;

	@Autowired
	private OrganisatieZoekDao organisatieZoekDao;

	@Override
	public List<Gebruiker> searchMedewerkers(Gebruiker searchObject, List<Functie> selectedFuncties, List<Rol> selectedRollen,
		Map<OrganisatieType, List<Instelling>> hierarchieCriteria, List<Bevolkingsonderzoek> bevolkingsonderzoeken, int first, int count, String sortProperty, boolean ascending)
	{
		Criteria criteria = createCriteria(searchObject, selectedFuncties, selectedRollen, hierarchieCriteria, bevolkingsonderzoeken);

		if (sortProperty != null)
		{
			if (ascending)
			{
				criteria.addOrder(Order.asc(sortProperty));
			}
			else
			{
				criteria.addOrder(Order.desc(sortProperty));
			}
		}
		criteria.setFirstResult(first);
		criteria.setMaxResults(count);
		criteria.setResultTransformer(Criteria.DISTINCT_ROOT_ENTITY);

		return criteria.list();
	}

	private Criteria createCriteria(Gebruiker zoekObject, List<Functie> selectedFuncties, List<Rol> selectedRollen, Map<OrganisatieType, List<Instelling>> hierarchieCriteria,
		List<Bevolkingsonderzoek> bevolkingsonderzoeken)
	{
		boolean needOrganisatieAlias = false;
		boolean needOrganisatiesAlias = false;
		boolean needRollenAlias = false;
		boolean isOrganisatieZoeken = false;

		Criteria criteria = getSession().createCriteria(Gebruiker.class);

		DetachedCriteria subcriteria = null;

		if (zoekObject.getId() != null)
		{
			criteria.add(Restrictions.eq("id", zoekObject.getId()));
		}
		if (zoekObject.getActief() != null)
		{
			criteria.add(Restrictions.eq("actief", zoekObject.getActief()));
		}
		if (zoekObject.getActiefVanaf() != null)
		{
			criteria.add(Restrictions.eq("actiefVanaf", zoekObject.getActiefVanaf()));
		}
		if (zoekObject.getActiefTotEnMet() != null)
		{
			criteria.add(Restrictions.eq("getActiefTotEnMet", zoekObject.getActiefTotEnMet()));
		}
		if (StringUtils.isNotBlank(zoekObject.getEmailextra()))
		{
			criteria.add(Restrictions.ilike("emailextra", zoekObject.getEmailextra(), MatchMode.EXACT));
		}
		if (CollectionUtils.isNotEmpty(zoekObject.getOrganisatieMedewerkers()))
		{
			Instelling organisatie = zoekObject.getOrganisatieMedewerkers().get(0).getOrganisatie();
			subcriteria = DetachedCriteria.forClass(InstellingGebruiker.class);
			if (StringUtils.isNotBlank(organisatie.getNaam()))
			{
				subcriteria.add(Restrictions.ilike("organisatie.naam", organisatie.getNaam(), MatchMode.ANYWHERE));
				needOrganisatieAlias = true;
				isOrganisatieZoeken = true;
			}
			if (StringUtils.isNotBlank(organisatie.getUziAbonneenummer()))
			{

				subcriteria.add(Restrictions.disjunction().add(Restrictions.ilike("organisatie.uziAbonneenummer", organisatie.getUziAbonneenummer(), MatchMode.ANYWHERE))
					.add(Restrictions.ilike("organisatie.agbcode", organisatie.getUziAbonneenummer(), MatchMode.ANYWHERE)));
				needOrganisatieAlias = true;
				isOrganisatieZoeken = true;
			}
			if (organisatie.getHuidigAdres() != null && StringUtils.isNotBlank(organisatie.getHuidigAdres().getPlaats()))
			{
				needOrganisatieAlias = true;
				isOrganisatieZoeken = true;
				subcriteria.createAlias("organisatie.adressen", "adres");
				subcriteria.add(Restrictions.ilike("adres.plaats", organisatie.getHuidigAdres().getPlaats(), MatchMode.ANYWHERE));
			}
		}
		if (StringUtils.isNotBlank(zoekObject.getAchternaam()))
		{
			criteria.add(Restrictions.ilike("achternaam", zoekObject.getAchternaam(), MatchMode.ANYWHERE));
		}
		if (StringUtils.isNotBlank(zoekObject.getOndertekenaar()))
		{
			criteria.add(Restrictions.ilike("ondertekenaar", zoekObject.getOndertekenaar(), MatchMode.ANYWHERE));
		}
		final String uzinummer = zoekObject.getUzinummer();
		if (StringUtils.isNotBlank(uzinummer))
		{

			Junction disjunction = Restrictions.disjunction().add(Restrictions.ilike("uzinummer", uzinummer, MatchMode.ANYWHERE));
			try
			{
				disjunction = disjunction.add(Restrictions.eq("medewerkercode", Integer.valueOf(uzinummer)));
			}
			catch (NumberFormatException e)
			{
			}
			criteria.add(disjunction);

		}
		if (CollectionUtils.isNotEmpty(selectedFuncties))
		{
			criteria.add(Restrictions.in("functie", selectedFuncties));
		}
		else if (zoekObject.getFunctie() != null)
		{
			criteria.add(Restrictions.eq("functie", zoekObject.getFunctie()));
		}

		if (CollectionUtils.isNotEmpty(selectedRollen))
		{
			needOrganisatiesAlias = true;
			needRollenAlias = true;
			if (subcriteria == null)
			{
				subcriteria = DetachedCriteria.forClass(InstellingGebruiker.class);
			}

			subcriteria.add(Restrictions.and(
				instellingDao.getActieveRolCriterion("igrol", "rol"),
				Restrictions.in("igrol.rol", selectedRollen)));
		}
		if (CollectionUtils.isNotEmpty(bevolkingsonderzoeken))
		{
			needOrganisatiesAlias = true;
			needRollenAlias = true;
			if (subcriteria == null)
			{
				subcriteria = DetachedCriteria.forClass(InstellingGebruiker.class);
			}
			subcriteria.createAlias("igrol.bevolkingsonderzoeken", "bevolkingsonderzoeken");
			subcriteria.add(Restrictions.in("bevolkingsonderzoeken.elements", bevolkingsonderzoeken));
		}
		if (MapUtils.isNotEmpty(hierarchieCriteria))
		{
			Disjunction disjunction = Restrictions.disjunction();
			Map<String, String> aliassen = new HashMap<>();
			if (subcriteria == null)
			{
				subcriteria = DetachedCriteria.forClass(InstellingGebruiker.class);
			}
			for (Entry<OrganisatieType, List<Instelling>> type : hierarchieCriteria.entrySet())
			{
				disjunction.add(organisatieZoekDao.addHierarchieCrit(type, aliassen, "organisatie."));
			}
			subcriteria.add(disjunction);
			for (Entry<String, String> aliasConf : aliassen.entrySet())
			{
				subcriteria.createAlias(aliasConf.getValue(), aliasConf.getKey(), JoinType.LEFT_OUTER_JOIN);
			}
			needOrganisatieAlias = true;
		}

		if (selectedFuncties == null && selectedRollen == null)
		{

			if (subcriteria == null)
			{
				subcriteria = DetachedCriteria.forClass(Gebruiker.class);
			}
			subcriteria.createAlias("organisatieMedewerkers", "organisatieMedewerkers", JoinType.LEFT_OUTER_JOIN);
			subcriteria.createAlias("organisatieMedewerkers.organisatie", "organisatie", JoinType.LEFT_OUTER_JOIN);
			subcriteria.add(Restrictions.or(Restrictions.isNull("organisatie.organisatieType"), Restrictions.ne("organisatie.organisatieType", OrganisatieType.HUISARTS)));
			subcriteria.setProjection(Projections.property("id"));
			criteria.add(Subqueries.propertyIn("id", subcriteria));
		}
		else
		{
			if (needOrganisatieAlias)
			{
				subcriteria.createAlias("organisatie", "organisatie");
				subcriteria.add(Restrictions.eq("organisatie.actief", true));
				needOrganisatiesAlias = true;
			}
			if (needOrganisatiesAlias)
			{
				subcriteria.add(Restrictions.eq("actief", true));
				subcriteria.setProjection(Projections.property("medewerker.id"));

				if (!isOrganisatieZoeken)
				{

					DetachedCriteria subcriteriaGekoppeld = DetachedCriteria.forClass(Gebruiker.class);
					subcriteriaGekoppeld.createAlias("organisatieMedewerkers", "organisatieMedewerkers");
					subcriteriaGekoppeld.createAlias("organisatieMedewerkers.rollen", "rollen", JoinType.LEFT_OUTER_JOIN);
					subcriteriaGekoppeld.createAlias("organisatieMedewerkers.organisatie", "organisatie");
					subcriteriaGekoppeld.add(
						Restrictions.or(
							Restrictions.eq("organisatie.organisatieType", OrganisatieType.HUISARTS), 
							Restrictions.and(
								Restrictions.eq("organisatie.actief", true), 
								Restrictions.eq("organisatieMedewerkers.actief", true),
								Restrictions.isNotNull("rollen.id"))));
					subcriteriaGekoppeld.setProjection(Projections.distinct(Projections.id()));
					Disjunction disjunction = Restrictions.disjunction();
					disjunction.add(Subqueries.propertyIn("id", subcriteria));
					if (CollectionUtils.isEmpty(selectedRollen))
					{
						disjunction.add(Subqueries.propertyNotIn("id", subcriteriaGekoppeld));
					}
					criteria.add(disjunction);
				}
				else
				{
					criteria.add(Subqueries.propertyIn("id", subcriteria));
				}

			}
			if (needRollenAlias)
			{
				subcriteria.createAlias("rollen", "igrol");
				subcriteria.createAlias("igrol.rol", "rol");
			}
		}

		return criteria;
	}

	@Override
	public long countMedewerkers(Gebruiker searchObject, List<Functie> selectedFuncties, List<Rol> selectedRollen, Map<OrganisatieType, List<Instelling>> hierarchieCriteria,
		List<Bevolkingsonderzoek> bevolkingsonderzoeken)
	{
		Criteria criteria = createCriteria(searchObject, selectedFuncties, selectedRollen, hierarchieCriteria, bevolkingsonderzoeken);
		criteria.setProjection(Projections.countDistinct("id"));
		@SuppressWarnings("unchecked")
		List<Long> countResult = criteria.list();
		return countResult.get(0).longValue();
	}

	@Override
	public void saveOrUpdateInstellingGebruiker(InstellingGebruiker organisatieMedewerker)
	{
		getSession().saveOrUpdate(organisatieMedewerker);
		getSession().saveOrUpdate(organisatieMedewerker.getOrganisatie());
		getSession().saveOrUpdate(organisatieMedewerker.getMedewerker());
	}

}
