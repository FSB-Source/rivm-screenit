package nl.rivm.screenit.dao.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import nl.rivm.screenit.dao.OrganisatieZoekDao;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.OrganisatieType;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.apache.commons.collections.CollectionUtils;
import org.hibernate.criterion.Conjunction;
import org.hibernate.criterion.Criterion;
import org.hibernate.criterion.Restrictions;
import org.springframework.stereotype.Repository;

@Repository
public class OrganisatieZoekDaoImpl extends AbstractAutowiredDao implements OrganisatieZoekDao
{

	@Override
	public Criterion addHierarchieCrit(Entry<OrganisatieType, List<Instelling>> type, Map<String, String> aliassen, String root)
	{
		Conjunction criteria = Restrictions.conjunction();
		OrganisatieType organisatieType = type.getKey();
		List<Instelling> instellingen = type.getValue();
		criteria.add(Restrictions.eq(root + "organisatieType", organisatieType));
		if (CollectionUtils.isNotEmpty(instellingen))
		{
			var organisatieTypeInstellingen = instellingen.get(0).getOrganisatieType();
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
				if (organisatieTypeInstellingen == OrganisatieType.SCREENINGSORGANISATIE)
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
			case INTAKELOCATIE:
			case COLOSCOPIELOCATIE:
			case MAMMAPOLI:
			case RADIOLOGIEAFDELING:
				if (organisatieTypeInstellingen == OrganisatieType.SCREENINGSORGANISATIE)
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
				if (organisatieTypeInstellingen == OrganisatieType.SCREENINGSORGANISATIE)
				{
					criteria.add(createCriteriaInstelling(root + "parent", instellingen));
				}
				else
				{
					criteria.add(createCriteriaInstelling(root + "id", instellingen));
				}
				break;
			case BEOORDELINGSEENHEID:
				if (organisatieTypeInstellingen == OrganisatieType.SCREENINGSORGANISATIE)
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
}
