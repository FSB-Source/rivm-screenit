package nl.rivm.screenit.dao.colon.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.dao.colon.ColonUitnodigingsDao;
import nl.rivm.screenit.model.UitnodigingsGebied;
import nl.rivm.screenit.model.colon.enums.ColonUitnodigingCategorie;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.apache.commons.lang.NotImplementedException;
import org.hibernate.Criteria;
import org.hibernate.ScrollMode;
import org.hibernate.ScrollableResults;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional(propagation = Propagation.SUPPORTS)
public class ColonUitnodigingsDaoImpl extends AbstractAutowiredDao implements ColonUitnodigingsDao
{
	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Override
	public ScrollableResults getUitnodigingsCursor(ColonUitnodigingCategorie uitnodigingscategorie, UitnodigingsGebied uitnodigingsgebied, List<Integer> geboorteJaren,
		Integer minimaleLeeftijd, Integer maximaleLeeftijd, Long projectGroupId, List<Long> exclusieGroepIds, int fetchSize)
	{
		Criteria crit;

		switch (uitnodigingscategorie)
		{
		case U1:
			crit = ColonRestrictions.getQueryVooraankondigen(getSession(), uitnodigingsgebied, geboorteJaren, false, minimaleLeeftijd, maximaleLeeftijd, projectGroupId,
				exclusieGroepIds, currentDateSupplier.getLocalDate());
			break;
		case U2:
			crit = ColonRestrictions.getQueryU2(getSession(), uitnodigingsgebied, minimaleLeeftijd, maximaleLeeftijd, false, projectGroupId, exclusieGroepIds,
				currentDateSupplier.getLocalDate());
			break;
		default:
			throw new NotImplementedException("Onbekende categorie");
		}
		return crit.setFetchSize(fetchSize).scroll(ScrollMode.FORWARD_ONLY);
	}

}
