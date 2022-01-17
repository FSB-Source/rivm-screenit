package nl.rivm.screenit.mamma.se.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-se-rest-bk
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

import java.time.LocalDateTime;
import java.util.List;

import nl.rivm.screenit.mamma.se.SELogin;
import nl.rivm.screenit.mamma.se.service.MammaScreeningsEenheidService;
import nl.rivm.screenit.mamma.se.service.SELogService;
import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.service.LogService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.REQUIRED)
public class SELogServiceImpl implements SELogService
{

	private static final Logger LOG = LoggerFactory.getLogger(SELogServiceImpl.class);

	@Autowired
	private LogService logService;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private MammaScreeningsEenheidService screeningsEenheidService;

	@Override
	public void logWarning(LogGebeurtenis logGebeurtenis, Account account, Client client, Long screeningsEenheidId, LocalDateTime datumTijd, String melding)
	{
		MammaScreeningsEenheid screeningsEenheid = hibernateService.load(MammaScreeningsEenheid.class, screeningsEenheidId);
		LOG.warn(createLogMessage(account, logGebeurtenis, screeningsEenheid, melding));
		logService.logGebeurtenis(logGebeurtenis, screeningsEenheid, account, client, melding, datumTijd);
	}

	@Override
	public void logError(LogGebeurtenis logGebeurtenis, Account account, MammaScreeningsEenheid screeningsEenheid, LocalDateTime datumTijd, String melding)
	{
		logError(logGebeurtenis, account, null, screeningsEenheid, null, datumTijd, melding);
	}

	@Override
	public void logError(LogGebeurtenis logGebeurtenis, Account account, Client client, MammaScreeningsEenheid screeningsEenheid, List<Instelling> instellingen,
		LocalDateTime datumTijd, String melding)
	{
		LOG.error(createLogMessage(account, logGebeurtenis, screeningsEenheid, melding));
		logService.logGebeurtenis(logGebeurtenis, screeningsEenheid, instellingen, account, client, melding, datumTijd);
	}

	@Override
	public void logInfo(LogGebeurtenis logGebeurtenis, Account account, String seCode, LocalDateTime datumTijd, String message)
	{
		MammaScreeningsEenheid screeningsEenheid = screeningsEenheidService.getActieveScreeningsEenheidByCode(seCode);
		if (screeningsEenheid == null && StringUtils.isNotBlank(seCode))
		{
			if (StringUtils.isBlank(message))
			{
				message = "Onbekende SE code: " + seCode;
			}
			else
			{
				message += message + " (Onbekende SE code: " + seCode + ")";
			}
		}
		logInfo(logGebeurtenis, account, null, screeningsEenheid, datumTijd, message);
	}

	@Override
	public void logInfo(LogGebeurtenis logGebeurtenis, Account account, MammaScreeningsEenheid screeningsEenheid, LocalDateTime datumTijd, String message)
	{
		logInfo(logGebeurtenis, account, null, screeningsEenheid, datumTijd, message);
	}

	@Override
	public void logInfo(LogGebeurtenis logGebeurtenis, Account account, Client client, MammaScreeningsEenheid screeningsEenheid, LocalDateTime datumTijd, String message)
	{
		LOG.info(createLogMessage(account, logGebeurtenis, screeningsEenheid, message));
		logService.logGebeurtenis(logGebeurtenis, screeningsEenheid, account, client, message, datumTijd);
	}

	private String createLogMessage(Account account, LogGebeurtenis logGebeurtenis, MammaScreeningsEenheid se, String message)
	{
		String result = String.format("%s; %s; %s", logGebeurtenis.name(), se == null ? "SE-???" : se.getCode(), message != null ? message : "");
		return account == null ? result : String.format("%s %s", SELogin.accountIdLogTekst(account), result);
	}

}
