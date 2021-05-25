package nl.rivm.screenit.dao.mamma.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import nl.rivm.screenit.dao.mamma.MammaBaseOnderzoekDao;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;
import nl.rivm.screenit.model.mamma.enums.MammaMammografieIlmStatus;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;
import org.hibernate.Criteria;
import org.hibernate.criterion.Restrictions;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@Repository
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class MammaBaseOnderzoekDaoImpl extends AbstractAutowiredDao implements MammaBaseOnderzoekDao
{

	@Override
	public List<MammaOnderzoek> getOnderzoekenVanUitnodigingsNummerMetBeeldenTeVerwijderen(Long uitnodigingsNummer)
	{
		Criteria criteria = getSession().createCriteria(MammaOnderzoek.class, "onderzoek");
		criteria.createAlias("onderzoek.afspraak", "afspraak");
		criteria.createAlias("afspraak.uitnodiging", "uitnodiging");
		criteria.createAlias("uitnodiging.screeningRonde", "screeningRonde");
		criteria.createAlias("onderzoek.mammografie", "mammografie");

		criteria.add(Restrictions.eq("screeningRonde.uitnodigingsNr", uitnodigingsNummer));
		criteria.add(Restrictions.eq("mammografie.ilmStatus", MammaMammografieIlmStatus.TE_VERWIJDEREN));

		return criteria.list();
	}
}
