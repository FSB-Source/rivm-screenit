package nl.rivm.screenit.dao.mamma.impl;

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

import nl.rivm.screenit.dao.mamma.MammaBaseHL7v24Dao;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.mamma.enums.MammaHL7v24ORMBerichtStatus;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.hibernate.Query;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.REQUIRED)
public class MammaBaseHL7v24DaoImpl extends AbstractAutowiredDao implements MammaBaseHL7v24Dao
{
	@Override
	public void deleteMessagesForClient(Client client, boolean verwijderAlleBerichten)
	{
		String clientId = client.getId().toString();

		Query query;
		var sql = "delete from mamma.hl7v24_message where dto_json like ('%\"clientId\":' || :clientId || '%')";
		if (verwijderAlleBerichten)
		{
			query = getSession().createSQLQuery(sql);
			query.setParameter("clientId", clientId);
		}
		else
		{
			sql += " and dto_json not like ('%\"status\":\"' || :delete || '\"%') and dto_json not like ('%\"status\":\"' || :goingToDelete || '\"%')";
			query = getSession().createSQLQuery(sql);
			query.setParameter("clientId", clientId);
			query.setParameter("delete", MammaHL7v24ORMBerichtStatus.DELETE.name());
			query.setParameter("goingToDelete", MammaHL7v24ORMBerichtStatus.GOINGTODELETE.name());
		}

		query.executeUpdate();
	}
}
