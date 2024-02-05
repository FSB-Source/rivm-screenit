
package nl.rivm.screenit.service.impl;

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

import nl.rivm.screenit.dao.VragenlijstBaseDao;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.project.ProjectVragenlijstAntwoordenHolder;
import nl.rivm.screenit.model.project.ProjectVragenlijstStatus;
import nl.rivm.screenit.model.vragenlijsten.Vragenlijst;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.VragenlijstBaseService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS)
public class VragenlijstBaseServiceImpl implements VragenlijstBaseService
{

	private static final Logger LOG = LoggerFactory.getLogger(VragenlijstBaseServiceImpl.class);

	@Autowired
	private VragenlijstBaseDao vragenlijstBaseDao;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private LogService logService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Override
	public <T extends Vragenlijst> List<T> searchVragenlijsten(T searchObject, long first, long count, String sortProperty, boolean asc)
	{
		return vragenlijstBaseDao.searchVragenlijsten(searchObject, first, count, sortProperty, asc);
	}

	@Override
	public <T extends Vragenlijst> long countVragenlijsten(T searchObject)
	{
		return vragenlijstBaseDao.countVragenlijsten(searchObject);

	}

	@Override
	public void saveOrAfronden(ProjectVragenlijstAntwoordenHolder vragenlijstAntwoordenHolder, boolean afronden, Client client)
	{
		String melding = "Digitaal ontvangen - Barcode: " + vragenlijstAntwoordenHolder.getProjectBrief().getId().toString();

		vragenlijstAntwoordenHolder.setLaatstGewijzigd(currentDateSupplier.getDate());
		hibernateService.saveOrUpdate(vragenlijstAntwoordenHolder);
		if (afronden)
		{
			vragenlijstAntwoordenHolder.setStatus(ProjectVragenlijstStatus.AFGEROND);
			logService.logGebeurtenis(LogGebeurtenis.VRAGENLIJST_AFGEROND, client, melding);
		}
		else
		{
			vragenlijstAntwoordenHolder.setStatus(ProjectVragenlijstStatus.IN_BEWERKING);
			logService.logGebeurtenis(LogGebeurtenis.VRAGENLIJST_OPGESLAGEN, client, melding);
		}
		hibernateService.saveOrUpdate(vragenlijstAntwoordenHolder.getVragenlijstAntwoorden());
		hibernateService.saveOrUpdate(vragenlijstAntwoordenHolder);
	}
}
