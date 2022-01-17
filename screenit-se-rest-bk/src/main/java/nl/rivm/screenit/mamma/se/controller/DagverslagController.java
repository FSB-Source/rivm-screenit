package nl.rivm.screenit.mamma.se.controller;

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

import java.time.LocalDate;
import java.util.Date;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import nl.rivm.screenit.mamma.se.dto.DagverslagDto;
import nl.rivm.screenit.mamma.se.service.DagverslagService;
import nl.rivm.screenit.util.DateUtil;
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
@RequestMapping("/dagverslag")
public class DagverslagController extends AuthorizedController
{
	private static final Logger LOG = LoggerFactory.getLogger(DagverslagController.class);

	@Autowired
	private DagverslagService dagverslagService;

	@RequestMapping(value = "/{seCode}/{datum}", method = RequestMethod.GET)
	public ResponseEntity getDagverslag(@PathVariable String seCode, @PathVariable @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate datum)
	{
		DagverslagOphaler dagverslagOphaler = new DagverslagOphaler(datum, seCode);
		Future future = executorService.submit(dagverslagOphaler);
		try
		{
			LOG.info("Dagverslag ophalen queued ({}) dag: {} ", seCode, datum);
			future.get(asyncRequestTimeoutSeconds, TimeUnit.SECONDS);
			return ResponseEntity.ok(dagverslagOphaler.getDagverslagDto());
		}
		catch (TimeoutException e)
		{
			LOG.warn("Timeout bij dagverslag ophalen", e);
			return new ResponseEntity<>(HttpStatus.SERVICE_UNAVAILABLE);
		}
		catch (Exception e)
		{
			return createErrorResponse(e);
		}
	}

	private class DagverslagOphaler extends OpenHibernate5SessionInThread
	{
		private final Date datum;

		private final String seCode;

		private DagverslagDto dagverslagDto;

		DagverslagOphaler(LocalDate datum, String seCode)
		{
			super(true);
			this.datum = DateUtil.toUtilDate(datum);
			this.seCode = seCode;
		}

		@Override
		protected void runInternal()
		{
			dagverslagDto = new DagverslagDto();
			dagverslagDto.setDagproductie(dagverslagService.getDagproductieVanSeMedewerkers(seCode, datum));
			dagverslagDto.setDagafsluiting(dagverslagService.getDoorgevoerdCountVanDag(seCode, datum));
			dagverslagDto.setDagSynchronisatie(dagverslagService.getSynchronisatieCountVanDag(seCode, datum));
			dagverslagDto.setNietAfgeslotenVanaf(dagverslagService.getDatumVanOudsteNietAfgeslotenOnderzoek(seCode));
			dagverslagDto.setDagPlanningSamenvatting(dagverslagService.getPlanningSamenvattingVanDeDag(seCode, datum));
		}

		DagverslagDto getDagverslagDto()
		{
			return dagverslagDto;
		}
	}
}
