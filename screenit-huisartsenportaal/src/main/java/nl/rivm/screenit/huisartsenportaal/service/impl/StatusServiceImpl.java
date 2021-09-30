package nl.rivm.screenit.huisartsenportaal.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
 * %%
 * Copyright (C) 2016 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import javax.jms.Connection;
import javax.jms.ConnectionFactory;

import nl.rivm.screenit.huisartsenportaal.dto.StatusDto;
import nl.rivm.screenit.huisartsenportaal.repository.HuisartsRepository;
import nl.rivm.screenit.huisartsenportaal.service.StatusService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS)
public class StatusServiceImpl implements StatusService
{

	private static String OK = "Akkoord";

	private static String NOK = "Niet Akkoord";

	@Value("${applicationEnvironment}")
	private String applicationEnvironment;

	@Autowired
	private HuisartsRepository huisartsRepository;

	@Autowired
	private ConnectionFactory connectionFactory;

	@Override
	public StatusDto getStatusObject()
	{
		StatusDto status = new StatusDto();
		status.setEnvironment(applicationEnvironment);
		status.setActiveMQStatus(getStatusFromActiveMQ());
		status.setDatabaseStatus(getStatusFromDatabase());
		return status;
	}

	@Override
	public String getStatusFromActiveMQ()
	{
		try
		{
			Connection connection = connectionFactory.createConnection();
			connection.close();
			return OK;
		}
		catch (Exception e)
		{
			return NOK;
		}
	}

	@Override
	public String getStatusFromDatabase()
	{
		try
		{
			Long aantal = huisartsRepository.countByHuisartsportaalId(1L);
			return OK;
		}
		catch (Exception e)
		{
			return NOK;
		}
	}
}
