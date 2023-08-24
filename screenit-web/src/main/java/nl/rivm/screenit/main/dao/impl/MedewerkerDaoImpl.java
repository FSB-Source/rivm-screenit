package nl.rivm.screenit.main.dao.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.Date;
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
import nl.rivm.screenit.model.InstellingGebruikerRol;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.Rol;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.topicuszorg.hibernate.criteria.BaseCriteria;
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
	public List<InstellingGebruiker> searchInstellingGebruiker(InstellingGebruiker zoekInstellingGebruiker, long first, long count, String orderByProperty, boolean ascending)
	{
		Criteria criteria = getCriteriaForInstellingGebruiker(zoekInstellingGebruiker);
		criteria.setResultTransformer(Criteria.DISTINCT_ROOT_ENTITY);

		if (orderByProperty != null)
		{
			if (ascending)
			{
				criteria.addOrder(Order.asc(orderByProperty));
			}
			else
			{
				criteria.addOrder(Order.desc(orderByProperty));
			}
		}

		@SuppressWarnings("unchecked")
		List<InstellingGebruiker> organisatieMedewerkers = criteria.list();

		if (first < 0 && count < 0)
		{
			return organisatieMedewerkers;
		}
		else
		{
			return organisatieMedewerkers.subList((int) first, (int) (first + count));
		}
	}

	@Override
	public List<InstellingGebruiker> getActieveRadiologen(InstellingGebruiker zoekInstellingGebruiker, List<Long> exclIds, String orderByProperty, boolean ascending)
	{
		Gebruiker medewerker = zoekInstellingGebruiker.getMedewerker();
		if (medewerker == null)
		{
			medewerker = new Gebruiker();
			zoekInstellingGebruiker.setMedewerker(medewerker);
		}
		medewerker.setActief(true);
		zoekInstellingGebruiker.setActief(true);
		Criteria criteria = getCriteriaForInstellingGebruiker(zoekInstellingGebruiker);
		criteria.setResultTransformer(Criteria.DISTINCT_ROOT_ENTITY);

		if (exclIds.size() > 0)
		{
			criteria.add(Restrictions.not(Restrictions.in("id", exclIds)));
		}

		criteria.add(Restrictions.isNotNull("medewerker.handtekening"));
		criteria.createAlias("rollen", "rollen");
		criteria.createAlias("rollen.rol", "rol");
		criteria.createAlias("rol.permissies", "permissie");
		criteria.add(Restrictions.eq("permissie.recht", Recht.GEBRUIKER_SCREENING_MAMMA_BEOORDELING_WERKLIJST));

		if (orderByProperty != null)
		{
			if (ascending)
			{
				criteria.addOrder(Order.asc(orderByProperty));
			}
			else
			{
				criteria.addOrder(Order.desc(orderByProperty));
			}
		}

		return criteria.list();
	}

	private Criteria getCriteriaForInstellingGebruiker(InstellingGebruiker instellingGebruiker)
	{
		Criteria criteria = getSession().createCriteria(InstellingGebruiker.class);

		if (instellingGebruiker.getActief() != null)
		{
			criteria.add(Restrictions.eq("actief", instellingGebruiker.getActief()));
		}
		criteria.createAlias("organisatie", "organisatie");
		criteria.createAlias("medewerker", "medewerker");
		Instelling instelling = instellingGebruiker.getOrganisatie();
		if (instelling != null)
		{
			if (instelling.getId() != null)
			{
				criteria.add(Restrictions.eq("organisatie.id", instelling.getId()));
			}
			else
			{
				criteria.add(Restrictions.isNull("organisatie.id"));
			}
		}
		Gebruiker medewerker = instellingGebruiker.getMedewerker();
		if (medewerker != null)
		{
			if (medewerker.getActief() != null)
			{
				criteria.add(Restrictions.eq("medewerker.actief", medewerker.getActief()));
			}
			if (medewerker.getActiefVanaf() != null)
			{
				criteria.add(Restrictions.eq("medewerker.actiefVanaf", medewerker.getActiefVanaf()));
			}
			if (medewerker.getActiefTotEnMet() != null)
			{
				criteria.add(Restrictions.eq("medewerker.actiefTotEnMet", medewerker.getActiefTotEnMet()));
			}
			if (medewerker.getId() != null)
			{
				criteria.add(Restrictions.eq("medewerker.id", medewerker.getId()));
			}
		}
		return criteria;
	}

	@Override
	public long countInstellingGebruiker(InstellingGebruiker instellinGebruiker)
	{
		Criteria criteria = getCriteriaForInstellingGebruiker(instellinGebruiker);

		criteria.setProjection(Projections.countDistinct("id"));

		@SuppressWarnings("unchecked")
		List<Long> countResult = criteria.list();
		return countResult.get(0).longValue();
	}

	@Override
	public InstellingGebruiker getInstellingGebruiker(Instelling organisatie, Gebruiker medewerker)
	{
		Criteria criteria = getSession().createCriteria(InstellingGebruiker.class);
		criteria.add(Restrictions.eq("organisatie.id", organisatie.getId()));
		criteria.add(Restrictions.eq("medewerker.id", medewerker.getId()));

		@SuppressWarnings("unchecked")
		List<InstellingGebruiker> organisatieMedewerkers = criteria.list();

		InstellingGebruiker organisatieMedewerker = null;
		if (organisatieMedewerkers.size() == 1)
		{
			organisatieMedewerker = organisatieMedewerkers.get(0);
		}
		else if (organisatieMedewerkers.size() > 1)
		{
			throw new IllegalStateException("Meerdere combinaties voor Organisatie en Medewerker gevonden, dit mag niet voorkomen.");
		}
		return organisatieMedewerker;
	}

	@Override
	public void saveOrUpdateInstellingGebruiker(InstellingGebruiker organisatieMedewerker)
	{
		getSession().saveOrUpdate(organisatieMedewerker);
		getSession().saveOrUpdate(organisatieMedewerker.getOrganisatie());
		getSession().saveOrUpdate(organisatieMedewerker.getMedewerker());
	}

	@Override
	public List<InstellingGebruikerRol> getInstellingGebruikersRollenMetRol(Rol rol, Date nu)
	{
		BaseCriteria<InstellingGebruikerRol> criteria = new BaseCriteria<InstellingGebruikerRol>(InstellingGebruikerRol.class);
		criteria.add(Restrictions.eq("actief", Boolean.TRUE));
		criteria.add(Restrictions.eq("rol", rol));
		return criteria.list(getSession());
	}

	@Override
	public List<Gebruiker> getActieveGebruikersMetRecht(Recht recht)
	{
		BaseCriteria<Gebruiker> crit = new BaseCriteria<>(Gebruiker.class);
		crit.createAlias("organisatieMedewerkers", "instellingGebruiker");
		crit.createAlias("instellingGebruiker.rollen", "rollen");
		crit.createAlias("rollen.rol", "rol");
		crit.createAlias("rol.permissies", "permissies");

		crit.add(Restrictions.eq("permissies.recht", recht));
		crit.add(Restrictions.eq("actief", true));
		crit.add(Restrictions.eq("instellingGebruiker.actief", true));
		crit.setResultTransformer(Criteria.DISTINCT_ROOT_ENTITY);
		return crit.list(getSession());
	}

}
