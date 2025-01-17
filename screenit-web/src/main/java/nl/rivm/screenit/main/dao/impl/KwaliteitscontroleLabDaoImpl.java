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

import java.util.List;

import nl.rivm.screenit.main.dao.KwaliteitscontroleLabDao;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.colon.IFOBTTest;
import nl.rivm.screenit.model.colon.SKMLControleBarcode;
import nl.rivm.screenit.model.colon.SKMLExterneControleBarcode;
import nl.rivm.screenit.model.colon.SKMLInterneControleBarcode;
import nl.rivm.screenit.model.colon.SKMLInterneControleSet;
import nl.rivm.screenit.model.colon.SKMLSentineelControleBarcode;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.apache.commons.lang3.StringUtils;
import org.hibernate.Criteria;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.Restrictions;
import org.springframework.stereotype.Repository;

@Repository
public class KwaliteitscontroleLabDaoImpl extends AbstractAutowiredDao implements KwaliteitscontroleLabDao
{

	@Override
	public SKMLInterneControleSet laagOfHoogSample(Instelling instelling)
	{
		Criteria criteria = getSession().createCriteria(SKMLInterneControleBarcode.class);
		criteria.addOrder(Order.desc("id"));
		criteria.add(Restrictions.eq("laboratorium.id", instelling.getId()));
		criteria.setMaxResults(1);
		SKMLInterneControleBarcode result = (SKMLInterneControleBarcode) criteria.uniqueResult();

		SKMLInterneControleSet volgendeSet = null;

		criteria = getSession().createCriteria(SKMLInterneControleSet.class);
		criteria.addOrder(Order.asc("volgorde"));
		List<SKMLInterneControleSet> alleSets = criteria.list();

		if (result != null)
		{
			boolean foundSet = false;

			for (SKMLInterneControleSet set : alleSets)
			{
				if (set.getQbaseId() != null && StringUtils.isNotBlank(set.getControleTekst()))
				{
					if (foundSet)
					{
						volgendeSet = set;
						break;
					}
					if (set.getQbaseId().equals(result.getQbaseId()) && set.getVolgorde().equals(result.getVolgorde()))
					{
						foundSet = true;
					}
				}
			}

		}

		if (volgendeSet == null)
		{
			for (SKMLInterneControleSet set : alleSets)
			{
				if (set.getQbaseId() != null && StringUtils.isNotBlank(set.getControleTekst()))
				{
					volgendeSet = set;
					break;
				}
			}
		}
		return volgendeSet;
	}

	@Override
	public SKMLInterneControleBarcode getInterneControleBarcode(String barcode)
	{
		Criteria criteria = getSession().createCriteria(SKMLInterneControleBarcode.class);
		criteria.add(Restrictions.eq("barcode", barcode));
		return (SKMLInterneControleBarcode) criteria.uniqueResult();
	}

	@Override
	public SKMLExterneControleBarcode getExterneControleBarcode(String barcode)
	{
		Criteria criteria = getSession().createCriteria(SKMLExterneControleBarcode.class);
		criteria.add(Restrictions.eq("barcode", barcode));
		return (SKMLExterneControleBarcode) criteria.uniqueResult();
	}

	@Override
	public SKMLSentineelControleBarcode getSentineelControleBarcode(String barcode)
	{
		Criteria criteria = getSession().createCriteria(SKMLSentineelControleBarcode.class);
		criteria.add(Restrictions.eq("barcode", barcode));
		return (SKMLSentineelControleBarcode) criteria.uniqueResult();
	}

	@Override
	public boolean checkOfBarcodeAlBestaat(String barcode)
	{
		Criteria criteria = getSession().createCriteria(SKMLControleBarcode.class);
		criteria.add(Restrictions.eq("barcode", barcode));
		boolean skmlBarcodeBestaat = criteria.uniqueResult() != null;

		criteria = getSession().createCriteria(IFOBTTest.class);
		criteria.add(Restrictions.eq("barcode", barcode));
		boolean ifobtBarcodeBestaat = criteria.uniqueResult() != null;

		return skmlBarcodeBestaat || ifobtBarcodeBestaat;
	}
}
