package nl.rivm.screenit.main.dao.mamma.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import nl.rivm.screenit.dto.mamma.MammaUploadBeeldenVerzoekDto;
import nl.rivm.screenit.main.dao.mamma.MammaUploadBeeldenDao;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.SortState;
import nl.rivm.screenit.model.mamma.MammaUploadBeeldenVerzoek;
import nl.rivm.screenit.model.mamma.MammaUploadBeeldenVerzoekStatus;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;
import org.hibernate.Criteria;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.hibernate.transform.Transformers;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@Repository
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class MammaUploadBeeldenDaoImpl extends AbstractAutowiredDao implements MammaUploadBeeldenDao
{
	@Override
	public List<MammaUploadBeeldenVerzoek> zoekOpenstaandeUploadBeeldenVerzoeken(Instelling instelling, ScreeningOrganisatie regio, int first, int count,
		SortState<String> sortState)
	{
		Criteria crit = createOpenstaandeUploadBeeldenVerzoekenCriteria(instelling, regio);
		String sortProperty = sortState.getSortParam();
		if (sortProperty != null)
		{
			if (sortState.isAsc())
			{
				crit.addOrder(Order.asc(sortProperty));
			}
			else
			{
				crit.addOrder(Order.desc(sortProperty));
			}
		}
		crit.setFirstResult(Math.max(first, 0));
		if (count > 0)
		{
			crit.setMaxResults(count);
		}
		return crit.list();
	}

	@Override
	public long countOpenstaandeUploadBeeldenVerzoeken(Instelling instelling, ScreeningOrganisatie regio)
	{
		Criteria crit = createOpenstaandeUploadBeeldenVerzoekenCriteria(instelling, regio);
		crit.setProjection(Projections.rowCount());
		return (Long) crit.uniqueResult();
	}

	private Criteria createOpenstaandeUploadBeeldenVerzoekenCriteria(Instelling instelling, ScreeningOrganisatie regio)
	{
		Criteria criteria = getSession().createCriteria(MammaUploadBeeldenVerzoek.class);
		criteria
			.add(Restrictions.or(Restrictions.eq("status", MammaUploadBeeldenVerzoekStatus.WACHTEN_OP_UPLOAD), Restrictions.eq("status", MammaUploadBeeldenVerzoekStatus.ERROR)));

		if (instelling != null)
		{
			criteria.add(Restrictions.eq("ziekenhuis.id", instelling.getId()));
		}

		if (regio != null)
		{
			criteria.createAlias("gemaaktDoor", "gemaaktDoor");
			criteria.createAlias("gemaaktDoor.organisatie", "organisatie");

			criteria.add(Restrictions.eq("organisatie.id", regio.getId()));
		}

		return criteria;
	}

	@Override
	public List<MammaUploadBeeldenVerzoekDto> zoekInstellingenMetOpenstaandeUploadVerzoeken(ScreeningOrganisatie regio)
	{
		Criteria criteria = createInstellingenMetOpenstaandeUploadVerzoeken(regio);
		criteria.setResultTransformer(Transformers.aliasToBean(MammaUploadBeeldenVerzoekDto.class));
		return criteria.list();
	}

	private Criteria createInstellingenMetOpenstaandeUploadVerzoeken(ScreeningOrganisatie regio)
	{
		Criteria criteria = createOpenstaandeUploadBeeldenVerzoekenCriteria(null, regio);
		criteria.createAlias("ziekenhuis", "ziekenhuis");

		criteria.setProjection(Projections.projectionList()
			.add(Projections.groupProperty("ziekenhuis.id").as("ziekenhuisId"))
			.add(Projections.groupProperty("ziekenhuis.naam").as("instellingNaam"))
			.add(Projections.count("id").as("aantalOpenstaand"))
			.add(Projections.groupProperty("ziekenhuis.telefoon").as("telefoon"))
			.add(Projections.groupProperty("ziekenhuis.telefoon2").as("telefoon2")));

		return criteria;
	}

}
