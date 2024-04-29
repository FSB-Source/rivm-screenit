package nl.rivm.screenit.mamma.se.controller;

/*-
 * ========================LICENSE_START=================================
 * screenit-se-rest-bk
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

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import javax.servlet.http.HttpServletRequest;

import nl.rivm.screenit.mamma.se.dto.AfspraakSeDto;
import nl.rivm.screenit.mamma.se.service.DaglijstService;
import nl.rivm.screenit.mamma.se.service.MammaScreeningsEenheidService;
import nl.topicuszorg.hibernate.spring.services.impl.OpenHibernate5SessionInThread;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/daglijst")
public class DaglijstController extends AuthorizedController
{
	private static final Logger LOG = LoggerFactory.getLogger(DaglijstController.class);

	@Autowired
	private DaglijstService daglijstService;

	@Autowired
	private MammaScreeningsEenheidService screeningsEenheidService;

	@GetMapping(value = "/{datum}")
	public ResponseEntity readDaglijst(@PathVariable @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate datum, HttpServletRequest request)
	{
		var seCode = getSeCode(request);
		DaglijstOphaler daglijstOphaler = new DaglijstOphaler(datum, seCode);
		var future = executorService.submit(daglijstOphaler);

		try
		{
			LOG.info("Daglijst ophalen queued ({}) dag: {} ", seCode, datum);
			future.get(asyncRequestTimeoutSeconds, TimeUnit.SECONDS);
			var afspraken = daglijstOphaler.getAfspraken();
			LOG.info("Daglijst opgehaald ({}) dag: {} #afspraken: {}", seCode, datum, afspraken.size());
			return ResponseEntity.ok(afspraken);
		}
		catch (TimeoutException e)
		{
			LOG.warn("Timeout bij daglijst ophalen", e);
			return new ResponseEntity<>(HttpStatus.SERVICE_UNAVAILABLE);
		}
		catch (InterruptedException e)
		{
			Thread.currentThread().interrupt();
			return createErrorResponse(e);
		}
		catch (Exception e)
		{
			return createErrorResponse(e);
		}
	}

	private class DaglijstOphaler extends OpenHibernate5SessionInThread
	{
		private final LocalDate opTeHalenDatum;

		private final String seCode;

		private List<AfspraakSeDto> afspraken;

		DaglijstOphaler(LocalDate opTeHalenDatum, String seCode)
		{
			super(true);
			this.opTeHalenDatum = opTeHalenDatum;
			this.seCode = seCode;
		}

		@Override
		protected void runInternal()
		{
			if (screeningsEenheidService.magSeDaglijstInzienVanDatum(seCode, opTeHalenDatum))
			{
				afspraken = daglijstService.readDaglijst(opTeHalenDatum, seCode);
			}
			else
			{
				afspraken = new ArrayList<>();
			}
		}

		List<AfspraakSeDto> getAfspraken()
		{
			return afspraken;
		}
	}
}
