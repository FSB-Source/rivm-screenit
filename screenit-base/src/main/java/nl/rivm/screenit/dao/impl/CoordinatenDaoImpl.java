
package nl.rivm.screenit.dao.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.math.BigDecimal;

import nl.rivm.screenit.dao.CoordinatenDao;
import nl.rivm.screenit.model.Gemeente;
import nl.rivm.screenit.model.PostcodeCoordinaten;
import nl.rivm.screenit.model.TijdelijkAdres;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;
import nl.topicuszorg.organisatie.model.Adres;

import org.apache.commons.lang.StringUtils;
import org.hibernate.Criteria;
import org.hibernate.FetchMode;
import org.hibernate.criterion.Restrictions;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class CoordinatenDaoImpl extends AbstractAutowiredDao implements CoordinatenDao
{

	@Override
	public PostcodeCoordinaten getCoordinaten(String postcode, Integer huisnummer, String huisnummerToevoeging, String huisletter)
	{
		Criteria crit = this.getSession().createCriteria(PostcodeCoordinaten.class);
		String postcodeWithoutWhitespace = StringUtils.deleteWhitespace(postcode);
		crit.add(Restrictions.eq("postcode", postcodeWithoutWhitespace));
		crit.add(Restrictions.eq("huisnummer", huisnummer));

		if (StringUtils.isBlank(huisnummerToevoeging) && StringUtils.isBlank(huisletter))
		{
			crit.add(Restrictions.isNull("huisnummerToevoeging"));
		}
		else
		{
			if (!StringUtils.isBlank(huisnummerToevoeging))
			{
				crit.add(Restrictions.eq("huisnummerToevoeging", huisnummerToevoeging));
			}
			else
			{
				crit.add(Restrictions.eq("huisnummerToevoeging", huisletter));
			}
		}

		PostcodeCoordinaten result = (PostcodeCoordinaten) crit.uniqueResult();

		if (result == null && StringUtils.isNotBlank(huisnummerToevoeging))
		{
			result = getCoordinaten(postcodeWithoutWhitespace, huisnummer, null, null);
		}

		return result;
	}

	@Override
	public PostcodeCoordinaten getCoordinaten(Adres adres)
	{
		return getCoordinaten(adres.getPostcode(), adres.getHuisnummer(), adres.getHuisnummerToevoeging(), adres.getHuisletter());
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void addOrUpdateCoordinaten(String postcode, String huisnr, String huisnummerToevoeging, String lat, String lon)
	{
		PostcodeCoordinaten coordinaten = getCoordinaten(postcode, Integer.valueOf(huisnr), huisnummerToevoeging, null);
		if (coordinaten == null)
		{
			coordinaten = new PostcodeCoordinaten();
			coordinaten.setHuisnummer(Integer.valueOf(huisnr));
			if (StringUtils.isNotBlank(huisnummerToevoeging))
			{
				coordinaten.setHuisnummerToevoeging(huisnummerToevoeging);
			}
			coordinaten.setPostcode(postcode);
		}
		coordinaten.setLatitude(new BigDecimal(lat));
		coordinaten.setLongitude(new BigDecimal(lon));
		getSession().saveOrUpdate(coordinaten);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void addOrUpdateCoordinaten(String gemcode, String latitude, String longitude)
	{
		Criteria criteria = getSession().createCriteria(Gemeente.class);
		criteria.add(Restrictions.eq("code", gemcode));
		criteria.setFetchMode("screeningOrganisatie", FetchMode.SELECT);
		criteria.setFetchMode("opvolgGemeente", FetchMode.SELECT);
		Gemeente gemeente = (Gemeente) criteria.uniqueResult();
		if (gemeente != null)
		{
			gemeente.setLatitude(new BigDecimal(latitude));
			gemeente.setLongitude(new BigDecimal(longitude));
			getSession().saveOrUpdate(gemeente);
		}
	}

	@Override
	public <A extends TijdelijkAdres> Criteria getCriteriaAdressenZonderPostcodeCoordinaten(Class<A> clazz)
	{
		Criteria criteria = getSession().createCriteria(clazz);
		criteria.add(Restrictions.isNull("postcodeCoordinaten"));
		return criteria;
	}
}
