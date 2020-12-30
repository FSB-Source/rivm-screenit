package nl.rivm.screenit.mamma.se.controller;

/*-
 * ========================LICENSE_START=================================
 * screenit-se-rest-bk
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.time.LocalDate;
import java.util.List;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import javax.servlet.http.HttpServletRequest;

import nl.rivm.screenit.mamma.se.dto.AfspraakSeDto;
import nl.rivm.screenit.mamma.se.service.DaglijstService;

import nl.topicuszorg.hibernate.spring.services.impl.OpenHibernate5SessionInThread;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/daglijst")
public class DaglijstController extends AuthorizedController
{
	private static final Logger LOG = LoggerFactory.getLogger(DaglijstController.class);

	@Autowired
	private DaglijstService daglijstService;

	@RequestMapping(value = "/{datum}", method = RequestMethod.GET)
	public ResponseEntity readDaglijst(@PathVariable @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate datum, HttpServletRequest request)
	{
		String seCode = getSeCode(request);
		DaglijstOphaler daglijstOphaler = new DaglijstOphaler(datum, seCode);
		Future future = executorService.submit(daglijstOphaler);

		try
		{
			LOG.info("Daglijst ophalen start (" + seCode + ") dag: " + datum);
			future.get(ASYNC_REQUEST_TIMEOUT_MS, TimeUnit.MILLISECONDS);
			List<AfspraakSeDto> afspraken = daglijstOphaler.getAfspraken();
			LOG.info("Daglijst ophalen eind (" + seCode + ") dag: " + datum);

			return ResponseEntity.ok(afspraken);
		}
		catch (TimeoutException e)
		{
			LOG.warn("Timeout bij daglijst ophalen", e);
			return new ResponseEntity<>(HttpStatus.SERVICE_UNAVAILABLE);
		}
		catch (Exception e)
		{
			return createErrorResponse(e);
		}
	}

	private class DaglijstOphaler extends OpenHibernate5SessionInThread
	{
		private LocalDate datum;

		private String seCode;

		private List<AfspraakSeDto> afspraken;

		DaglijstOphaler(LocalDate datum, String seCode)
		{
			super(true);
			this.datum = datum;
			this.seCode = seCode;
		}

		@Override
		protected void runInternal()
		{
			afspraken = daglijstService.readDaglijst(datum, seCode);
		}

		List<AfspraakSeDto> getAfspraken()
		{
			return afspraken;
		}
	}
}
