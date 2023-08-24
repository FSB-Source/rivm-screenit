
package nl.rivm.screenit.dao.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.dao.OrganisatieZoekDao;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.topicuszorg.hibernate.restrictions.NvlRestrictions;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;
import nl.topicuszorg.organisatie.model.Adres;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.collections.MapUtils;
import org.apache.commons.lang.StringUtils;
import org.hibernate.Criteria;
import org.hibernate.criterion.Conjunction;
import org.hibernate.criterion.Criterion;
import org.hibernate.criterion.Disjunction;
import org.hibernate.criterion.MatchMode;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.Restrictions;
import org.hibernate.sql.JoinType;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

@Repository
public class OrganisatieZoekDaoImpl extends AbstractAutowiredDao implements OrganisatieZoekDao
{
	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Override
	public Iterator<Instelling> searchOrganisatie(Instelling searchObject, Map<OrganisatieType, List<Instelling>> hierarchieCriteria, List<OrganisatieType> excludeOrganisatieTypes,
		long first, long count, String sortProperty, boolean asc)
	{
		Criteria criteria = getCriteriaForOrganisatie(searchObject, hierarchieCriteria, excludeOrganisatieTypes, sortProperty);
		criteria.setResultTransformer(Criteria.DISTINCT_ROOT_ENTITY);
		if (sortProperty != null)
		{
			if (asc)
			{
				criteria.addOrder(Order.asc(sortProperty));
				if (sortProperty.startsWith("adres.plaats"))
				{
					criteria.addOrder(Order.asc("postadreswoonplaats.naam"));
				}
				if (sortProperty.startsWith("adres.straat"))
				{
					criteria.addOrder(Order.asc("postadres.straat"));
				}
			}
			else
			{
				criteria.addOrder(Order.desc(sortProperty));
				if (sortProperty.startsWith("adres.plaats"))
				{
					criteria.addOrder(Order.desc("postadreswoonplaats.naam"));
				}
				if (sortProperty.startsWith("adres.straat"))
				{
					criteria.addOrder(Order.desc("postadres.straat"));
				}
			}
		}

		@SuppressWarnings("unchecked")
		List<Instelling> organisaties = criteria.list();
		if (first != -1)
		{
			if (organisaties.size() > first + count)
			{
				organisaties = organisaties.subList((int) first, (int) (first + count));
			}
			else if (organisaties.size() > first)
			{
				organisaties = organisaties.subList((int) first, organisaties.size());
			}
		}
		return organisaties.iterator();
	}

	private Criteria getCriteriaForOrganisatie(Instelling organisatie, Map<OrganisatieType, List<Instelling>> hierarchieCriteria, List<OrganisatieType> excludeOrganisatieTypes,
		String sortProperty)
	{
		Criteria criteria = getSession().createCriteria(Instelling.class);
		criteria.setResultTransformer(Criteria.DISTINCT_ROOT_ENTITY);

		boolean needsMedewerkerAlias = false;
		boolean needsOrgMedAlias = false;
		boolean needsAdresAlias = false;

		if (organisatie.getActief() != null)
		{
			criteria.add(Restrictions.eq("actief", organisatie.getActief()));
		}
		if (StringUtils.isNotBlank(organisatie.getNaam()))
		{
			criteria.add(Restrictions.ilike("naam", organisatie.getNaam(), MatchMode.ANYWHERE));
		}
		if (StringUtils.isNotBlank(organisatie.getEmail()))
		{
			criteria.add(
				Restrictions.or(
					Restrictions.ilike("email", organisatie.getEmail(), MatchMode.EXACT),
					Restrictions.ilike("email2", organisatie.getEmail(), MatchMode.EXACT),
					Restrictions.ilike("email3", organisatie.getEmail(), MatchMode.EXACT),
					Restrictions.ilike("email4", organisatie.getEmail(), MatchMode.EXACT),
					Restrictions.ilike("extraEmails", organisatie.getEmail(), MatchMode.ANYWHERE)));
		}
		if (organisatie.getId() != null)
		{
			criteria.add(Restrictions.eq("id", organisatie.getId()));
		}

		if (CollectionUtils.isNotEmpty(excludeOrganisatieTypes))
		{
			criteria.add(Restrictions.not(Restrictions.in("organisatieType", excludeOrganisatieTypes)));
		}

		if (MapUtils.isNotEmpty(hierarchieCriteria))
		{
			Disjunction disjunction = Restrictions.disjunction();
			Map<String, String> aliassen = new HashMap<>();
			for (Entry<OrganisatieType, List<Instelling>> type : hierarchieCriteria.entrySet())
			{
				disjunction.add(addHierarchieCrit(type, aliassen, ""));
			}
			criteria.add(disjunction);
			for (Entry<String, String> aliasConf : aliassen.entrySet())
			{
				criteria.createAlias(aliasConf.getValue(), aliasConf.getKey(), JoinType.LEFT_OUTER_JOIN);
			}
		}

		if (CollectionUtils.isNotEmpty(organisatie.getOrganisatieMedewerkers()))
		{
			Gebruiker medewerker = organisatie.getOrganisatieMedewerkers().get(0).getMedewerker();
			if (StringUtils.isNotBlank(medewerker.getAchternaam()))
			{
				criteria.add(Restrictions.ilike("medewerker.achternaam", medewerker.getAchternaam(), MatchMode.ANYWHERE));
				needsMedewerkerAlias = true;
			}
			if (StringUtils.isNotBlank(medewerker.getUzinummer()))
			{

				criteria.add(Restrictions.disjunction().add(Restrictions.ilike("medewerker.uzinummer", medewerker.getUzinummer(), MatchMode.ANYWHERE))
					.add(Restrictions.ilike("medewerker.rolCode", medewerker.getUzinummer(), MatchMode.ANYWHERE)));
				needsMedewerkerAlias = true;
			}

		}

		if (needsMedewerkerAlias)
		{
			criteria.createAlias("medewerkers.medewerker", "medewerker");
			criteria.add(Restrictions.eq("medewerker.actief", true));
			final Date nu = currentDateSupplier.getDate();
			criteria.add(NvlRestrictions.le("medewerker.actiefVanaf", nu, Constants.BEGIN_OF_TIME));
			criteria.add(NvlRestrictions.ge("medewerker.actiefTotEnMet", nu, Constants.END_OF_TIME));
			needsOrgMedAlias = true;
		}

		if (StringUtils.isNotBlank(organisatie.getUziAbonneenummer()))
		{

			criteria.add(Restrictions.disjunction().add(Restrictions.ilike("uziAbonneenummer", organisatie.getUziAbonneenummer(), MatchMode.ANYWHERE))
				.add(Restrictions.ilike("agbcode", organisatie.getUziAbonneenummer(), MatchMode.ANYWHERE))
				.add(Restrictions.ilike("rootOid", organisatie.getUziAbonneenummer(), MatchMode.ANYWHERE)));
		}

		if (CollectionUtils.isNotEmpty(organisatie.getAdressen()))
		{
			Adres adres = organisatie.getHuidigAdres();
			if (adres.getPlaats() != null)
			{
				criteria.add(Restrictions.or(Restrictions.ilike("adres.plaats", adres.getPlaats(), MatchMode.ANYWHERE),
					Restrictions.ilike("postadreswoonplaats.naam", adres.getPlaats(), MatchMode.ANYWHERE)));
				needsAdresAlias = true;
			}
			if (adres.getPostcode() != null)
			{
				String postcode = StringUtils.deleteWhitespace(adres.getPostcode());
				criteria.add(
					Restrictions.or(Restrictions.ilike("adres.postcode", postcode, MatchMode.ANYWHERE), Restrictions.ilike("postadres.postcode", postcode, MatchMode.ANYWHERE)));
				needsAdresAlias = true;
			}
		}

		if (sortProperty != null && sortProperty.startsWith("adres."))
		{
			needsAdresAlias = true;
		}
		if (needsAdresAlias)
		{
			criteria.createAlias("adressen", "adres", JoinType.LEFT_OUTER_JOIN);
			criteria.createAlias("postadres", "postadres", JoinType.LEFT_OUTER_JOIN);
			criteria.createAlias("postadres.woonplaats", "postadreswoonplaats", JoinType.LEFT_OUTER_JOIN);

		}
		if (needsOrgMedAlias)
		{
			criteria.createAlias("organisatieMedewerkers", "medewerkers");
			criteria.add(Restrictions.eq("medewerkers.actief", true));
		}

		return criteria;
	}

	@Override
	public Criterion addHierarchieCrit(Entry<OrganisatieType, List<Instelling>> type, Map<String, String> aliassen, String root)
	{
		Conjunction criteria = Restrictions.conjunction();
		OrganisatieType organisatieType = type.getKey();
		List<Instelling> instellingen = type.getValue();
		criteria.add(Restrictions.eq(root + "organisatieType", organisatieType));
		if (CollectionUtils.isNotEmpty(instellingen))
		{
			switch (organisatieType)
			{
			case SCREENINGSORGANISATIE:
			case INPAKCENTRUM:
			case RIVM:
			case LABORATORIUM:
			case BMHK_LABORATORIUM:
			case HUISARTS:
				criteria.add(createCriteriaInstelling(root + "id", instellingen));
				break;
			case PA_LABORATORIUM:
				if (instellingen.get(0).getOrganisatieType().equals(OrganisatieType.SCREENINGSORGANISATIE))
				{
					aliassen.put("coloscopielocatie", root + "coloscopielocaties");
					aliassen.put("zorginstelling1", "coloscopielocatie.parent");
					criteria.add(createCriteriaInstelling("zorginstelling1.parent", instellingen));
				}
				else
				{
					criteria.add(createCriteriaInstelling(root + "id", instellingen));
				}
				break;
			case COLOSCOPIECENTRUM:
			case COLOSCOPIELOCATIE:
			case MAMMAPOLI:
			case RADIOLOGIEAFDELING:
				if (instellingen.get(0).getOrganisatieType().equals(OrganisatieType.SCREENINGSORGANISATIE))
				{
					aliassen.put("parentorganisatie", root + "parent");
					criteria.add(createCriteriaInstelling("parentorganisatie.parent", instellingen));
				}
				else
				{
					criteria.add(createCriteriaInstelling(root + "id", instellingen));
				}
				break;
			case ZORGINSTELLING:
				if (instellingen.get(0).getOrganisatieType().equals(OrganisatieType.SCREENINGSORGANISATIE))
				{
					criteria.add(createCriteriaInstelling(root + "parent", instellingen));
				}
				else
				{
					criteria.add(createCriteriaInstelling(root + "id", instellingen));
				}
				break;
			case BEOORDELINGSEENHEID:
				if (instellingen.get(0).getOrganisatieType().equals(OrganisatieType.SCREENINGSORGANISATIE))
				{
					aliassen.put("parentorganisatie", root + "parent");
					criteria.add(createCriteriaInstelling("parentorganisatie.regio", instellingen));
				}
				break;
			default:
				break;
			}

		}
		return criteria;
	}

	private static Criterion createCriteriaInstelling(String alias, List<Instelling> instellingen)
	{
		Criterion criterion = null;
		if (instellingen.size() == 1)
		{
			if (alias.endsWith("id"))
			{
				criterion = Restrictions.eq(alias, instellingen.get(0).getId());
			}
			else
			{
				criterion = Restrictions.eq(alias, instellingen.get(0));
			}
		}
		else
		{
			criterion = Restrictions.in(alias, instellingen);
		}
		return criterion;
	}

	@Override
	public long countOrganisatie(Instelling searchObject, Map<OrganisatieType, List<Instelling>> hierarchieCriteria, List<OrganisatieType> excludeOrganisatieTypes)
	{
		Criteria criteria = getCriteriaForOrganisatie(searchObject, hierarchieCriteria, excludeOrganisatieTypes, null);
		criteria.setResultTransformer(Criteria.DISTINCT_ROOT_ENTITY);

		@SuppressWarnings("unchecked")
		List<Instelling> countResult = criteria.list();
		return countResult.size();
	}

	@Override
	public List<Instelling> zoekOrganisatieMetFqdn(String fqdn)
	{
		Criteria criteria = getSession().createCriteria(Instelling.class);
		criteria.add(Restrictions.eq("fqdn", fqdn));
		criteria.add(Restrictions.eq("actief", true));
		return criteria.list();
	}
}
