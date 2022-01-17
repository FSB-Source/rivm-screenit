package nl.rivm.screenit.clientportaal.services.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.clientportaal.mappers.TijdelijkAdresMapper;
import nl.rivm.screenit.clientportaal.services.ClientGegevensService;
import nl.rivm.screenit.clientportaal.validators.TelefoonnummerValidator;
import nl.rivm.screenit.clientportaal.validators.TijdelijkAdresValidator;
import nl.rivm.screenit.dao.CoordinatenDao;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.TijdelijkAdres;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.service.ClientContactService;
import nl.rivm.screenit.service.LogService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.patientregistratie.persoonsgegevens.model.Persoon;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.REQUIRED)
public class ClientGegevensServiceImpl implements ClientGegevensService
{
	private static final Logger LOG = LoggerFactory.getLogger(ClientGegevensServiceImpl.class);

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private CoordinatenDao coordinatenDao;

	@Autowired
	private TijdelijkAdresMapper tijdelijkAdresMapper;

	@Autowired
	private ClientContactService clientContactService;

	@Autowired
	private LogService logService;

	@Override
	public Client setTelefoonnummer(String telefoonnummer1, String telefoonnummer2, Client client)
	{
		Persoon persoon = client.getPersoon();

		String getrimdeTelefoonnummer1 = StringUtils.trimToNull(telefoonnummer1);
		if (getrimdeTelefoonnummer1 == null || TelefoonnummerValidator.telefoonnummerIsCorrect(getrimdeTelefoonnummer1))
		{
			persoon.setTelefoonnummer1(getrimdeTelefoonnummer1);
		}
		else
		{
			throw new IllegalStateException("Het telefoonnummer is niet geldig");
		}

		String getrimdeTelefoonnummer2 = StringUtils.trimToNull(telefoonnummer2);
		if (getrimdeTelefoonnummer2 == null || TelefoonnummerValidator.telefoonnummerIsCorrect(getrimdeTelefoonnummer2))
		{
			persoon.setTelefoonnummer2(getrimdeTelefoonnummer2);
		}
		else
		{
			throw new IllegalStateException("Het telefoonnummer is niet geldig");
		}

		hibernateService.saveOrUpdate(persoon);

		return client;
	}

	@Override
	public Client setTijdelijkAdres(TijdelijkAdres nieuwTijdelijkAdres, Client client)
	{
		GbaPersoon persoon = client.getPersoon();

		TijdelijkAdres huidigTijdelijkAdres = persoon.getTijdelijkAdres();
		TijdelijkAdresValidator.validateTijdelijkAdres(nieuwTijdelijkAdres, huidigTijdelijkAdres);

		if (huidigTijdelijkAdres == null)
		{
			if (nieuwTijdelijkAdres.getId() != null)
			{
				throw new IllegalStateException("Het nieuwe tijdelijk adres heeft al een id");
			}
			clientContactService.saveTijdelijkAdres(client, client, nieuwTijdelijkAdres);
		}
		else
		{
			if (!huidigTijdelijkAdres.getId().equals(nieuwTijdelijkAdres.getId()))
			{
				throw new IllegalStateException("Huidige adres en nieuw adres hebben ander id");
			}
			nieuwTijdelijkAdres.setPostcodeCoordinaten(coordinatenDao.getCoordinaten(nieuwTijdelijkAdres));
			tijdelijkAdresMapper.updateTijdelijkAdres(huidigTijdelijkAdres, nieuwTijdelijkAdres);
			hibernateService.saveOrUpdate(huidigTijdelijkAdres);
			logService.logGebeurtenis(LogGebeurtenis.WIJZIG_TIJDELIJK_ADRES, client, client);
		}
		return client;
	}
}
