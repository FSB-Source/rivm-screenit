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

import nl.rivm.screenit.dao.ClientDao;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional(propagation = Propagation.SUPPORTS)
public class ClientDaoImpl extends AbstractAutowiredDao implements ClientDao
{

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void saveOrUpdateClient(Client client)
	{
		this.getSession().saveOrUpdate(client.getPersoon().getGbaAdres());
		this.getSession().saveOrUpdate(client.getPersoon());
		this.getSession().saveOrUpdate(client);
	}

	@Override
	public boolean heeftDossierMetRondeOfAfmelding(Client client)
	{
		var query = getSession().createNativeQuery("SELECT 1"
			+ " FROM gedeeld.pat_patient pa"
			+ "  LEFT JOIN colon.colon_dossier co ON pa.colon_dossier = co.id"
			+ "  LEFT JOIN cervix.dossier ce ON pa.cervix_dossier = ce.id"
			+ "  LEFT JOIN mamma.dossier ma ON pa.mamma_dossier = ma.id"
			+ " WHERE pa.id = :clientId"
			+ "  AND (co.laatste_screening_ronde IS NOT NULL OR co.laatste_afmelding IS NOT NULL"
			+ "   OR ce.laatste_screening_ronde IS NOT NULL OR ce.laatste_afmelding IS NOT NULL"
			+ "   OR ma.laatste_screening_ronde IS NOT NULL OR ma.laatste_afmelding IS NOT NULL);");
		query.setParameter("clientId", client.getId());
		return query.uniqueResult() != null;
	}
}
