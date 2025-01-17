package nl.rivm.screenit.mamma.se.controller;

/*-
 * ========================LICENSE_START=================================
 * screenit-se-rest-bk
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

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import javax.servlet.http.HttpServletRequest;

import nl.rivm.screenit.mamma.se.SERequestHeader;
import nl.rivm.screenit.mamma.se.dto.ErrorDto;
import nl.rivm.screenit.mamma.se.dto.SeAutorisatieDto;
import nl.rivm.screenit.mamma.se.service.MammaScreeningsEenheidService;
import nl.rivm.screenit.mamma.se.service.SeAutorisatieService;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.util.EnvironmentUtil;
import nl.rivm.screenit.util.FoutmeldingsCodeUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.commons.lang.math.NumberUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

abstract class AuthorizedController
{
	private static final Logger LOG = LoggerFactory.getLogger(AuthorizedController.class);

	private static final int DEFAULT_THREAD_POOL_SIZE = 50;

	private static final int DEFAULT_ASYNC_REQUEST_TIMEOUT_SECONDS = 20;

	protected static final ExecutorService executorService;

	protected static final int asyncRequestTimeoutSeconds;

	static
	{
		int threadPoolSize = EnvironmentUtil.getIntegerEnvironmentVariable("SE_LONG_REQUEST_THREAD_POOL_SIZE", DEFAULT_THREAD_POOL_SIZE);
		executorService = Executors.newFixedThreadPool(threadPoolSize);
		asyncRequestTimeoutSeconds = EnvironmentUtil.getIntegerEnvironmentVariable("SE_LONG_REQUEST_TIMEOUT_SECONDS", DEFAULT_ASYNC_REQUEST_TIMEOUT_SECONDS);
		LOG.info("SE AuthorizedController executorService initialized: threadPoolSize: {}, requestTimeout: {} seconds", threadPoolSize, asyncRequestTimeoutSeconds);
	}

	@Autowired
	private SeAutorisatieService seAutorisatieService;

	@Autowired
	private MammaScreeningsEenheidService screeningsEenheidService;

	@Autowired
	private HibernateService hibernateService;

	protected static String getSeCode(HttpServletRequest request)
	{
		return request.getHeader(SERequestHeader.SE_CODE);
	}

	protected static String getAccountId(HttpServletRequest request)
	{
		return request.getHeader(SERequestHeader.ACCOUNT_ID);
	}

	final protected MammaScreeningsEenheid getScreeningsEenheid(HttpServletRequest request)
	{
		return screeningsEenheidService.getActieveScreeningsEenheidByCode(getSeCode(request));
	}

	final protected InstellingGebruiker getInstellingGebruiker(HttpServletRequest request)
	{
		String accountId = getAccountId(request);
		if (NumberUtils.isNumber(accountId))
		{
			return hibernateService.get(InstellingGebruiker.class, Long.parseLong(accountId));
		}
		return null;
	}

	final protected boolean isAuthorized(HttpServletRequest request, Recht recht)
	{
		String accountId = getAccountId(request);
		if (NumberUtils.isNumber(accountId))
		{
			return seAutorisatieService.isGeautoriseerd(Long.parseLong(accountId), recht);
		}
		return false;
	}

	final protected SeAutorisatieDto getSeRechtenGebruiker(Long accountId)
	{
		return seAutorisatieService.getSeRechten(accountId);
	}

	final protected ResponseEntity<ErrorDto> forbiddenResponse(String errorReferentie)
	{
		ErrorDto errorDto = new ErrorDto(errorReferentie);
		return ResponseEntity.status(HttpStatus.FORBIDDEN).body(errorDto);
	}

	final protected ResponseEntity<ErrorDto> createErrorResponse(Exception ex)
	{
		String referentie = FoutmeldingsCodeUtil.getFoutmeldingsCode("SE_REST");
		LOG.error(referentie + ": " + ex.toString(), ex);
		ErrorDto errorDto = new ErrorDto(referentie);
		return new ResponseEntity<>(errorDto, HttpStatus.INTERNAL_SERVER_ERROR);

	}

	final protected ResponseEntity<ErrorDto> createUnauthorizedResponse()
	{
		String referentie = FoutmeldingsCodeUtil.getFoutmeldingsCode("SE_REST");
		LOG.error(referentie + ": Gebruiker is niet geautoriseerd om de actie uit te voeren");
		return forbiddenResponse(referentie);
	}
}
