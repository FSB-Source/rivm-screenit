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

import java.util.Date;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.dao.InstellingDao;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.restrictions.NvlRestrictions;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.hibernate.criterion.Criterion;
import org.hibernate.criterion.Restrictions;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

@Repository
public class InstellingDaoImpl extends AbstractAutowiredDao implements InstellingDao
{
	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Override
	public Criterion getActieveRolCriterion(String igRolAlias, String rolAlias)
	{
		Date nu = currentDateSupplier.getDate();
		Date gisteren = DateUtil.minDagen(currentDateSupplier.getDate(), 1);

		return Restrictions.and(
			NvlRestrictions.le(igRolAlias + ".beginDatum", nu, Constants.BEGIN_OF_TIME),
			NvlRestrictions.ge(igRolAlias + ".eindDatum", gisteren, Constants.END_OF_TIME),
			Restrictions.eq(igRolAlias + ".actief", true),
			Restrictions.eq(rolAlias + ".actief", true));
	}
}
