
package nl.rivm.screenit.main.dao.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.main.dao.OngeldigeBerichtenDao;
import nl.rivm.screenit.model.BerichtZoekFilter;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.berichten.cda.MeldingOngeldigCdaBericht;
import nl.rivm.screenit.model.berichten.enums.BerichtType;
import nl.topicuszorg.hibernate.criteria.BaseCriteria;
import nl.topicuszorg.hibernate.criteria.ListCriteria;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.apache.commons.lang3.StringUtils;
import org.hibernate.criterion.Disjunction;
import org.hibernate.criterion.MatchMode;
import org.hibernate.criterion.Restrictions;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.google.common.primitives.Ints;

@Repository
@Transactional(propagation = Propagation.SUPPORTS)
public class OngeldigeBerichtenDaoImpl extends AbstractAutowiredDao implements OngeldigeBerichtenDao
{

	@Override
	public List<MeldingOngeldigCdaBericht> searchOngeldigeBerichten(BerichtZoekFilter filter, long first, long count, String sortProperty, boolean ascending)
	{
		BaseCriteria<MeldingOngeldigCdaBericht> criteria = createBerichtenZoekCriteria(filter);
		if (sortProperty.startsWith("ontvangenCdaBericht"))
		{
			criteria.alias("ontvangenCdaBericht");
		}
		if (sortProperty.startsWith("screeningOrganisatie"))
		{
			criteria.alias("screeningOrganisatie");
		}
		return criteria.list(getSession(), new ListCriteria(Ints.checkedCast(first), Ints.checkedCast(count), sortProperty, ascending));
	}

	private BaseCriteria<MeldingOngeldigCdaBericht> createBerichtenZoekCriteria(BerichtZoekFilter filter)
	{
		BaseCriteria<MeldingOngeldigCdaBericht> crit = new BaseCriteria<>(MeldingOngeldigCdaBericht.class);
		crit.add(Restrictions.eq("actief", Boolean.TRUE));

		crit.alias("ontvangenCdaBericht");
		if (Boolean.TRUE.equals(filter.getMldBerichten()) || Boolean.TRUE.equals(filter.getPaLabBerichten()) || Boolean.TRUE.equals(filter.getCytologieBerichten())
			|| Boolean.TRUE.equals(filter.getFollowUpBerichten()))
		{
			Disjunction or = Restrictions.disjunction();
			if (Boolean.TRUE.equals(filter.getMldBerichten()))
			{
				or.add(Restrictions.eq("ontvangenCdaBericht.berichtType", BerichtType.MDL_VERSLAG));
			}
			if (Boolean.TRUE.equals(filter.getPaLabBerichten()))
			{
				or.add(Restrictions.eq("ontvangenCdaBericht.berichtType", BerichtType.PA_LAB_VERSLAG));
			}
			if (Boolean.TRUE.equals(filter.getCytologieBerichten()))
			{
				or.add(Restrictions.eq("ontvangenCdaBericht.berichtType", BerichtType.CERVIX_CYTOLOGIE_VERSLAG));
			}
			if (Boolean.TRUE.equals(filter.getFollowUpBerichten()))
			{
				or.add(Restrictions.eq("ontvangenCdaBericht.berichtType", BerichtType.MAMMA_PA_FOLLOW_UP_VERSLAG));
			}
			crit.add(or);
		}
		else
		{
			crit.add(Restrictions.isNull("ontvangenCdaBericht.berichtType"));
		}

		if (filter.getBsn() != null)
		{
			crit.add(Restrictions.eq("bsn", filter.getBsn()));
		}
		if (StringUtils.isNotBlank(filter.getText()))
		{
			crit.add(Restrictions.ilike("melding", filter.getText(), MatchMode.ANYWHERE));
		}
		ScreeningOrganisatie screeningOrganisatie = filter.getScreeningOrganisatie();
		if (screeningOrganisatie != null)
		{
			crit.add(Restrictions.eq("screeningOrganisatie", screeningOrganisatie));
		}

		return crit;
	}

	@Override
	public long countOngeldigeBerichten(BerichtZoekFilter filter)
	{
		return createBerichtenZoekCriteria(filter).count(getSession());
	}

}
