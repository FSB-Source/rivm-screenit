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
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import nl.rivm.screenit.mamma.se.dto.DagAfsluitingDto;
import nl.rivm.screenit.mamma.se.dto.DagProductieDto;
import nl.rivm.screenit.mamma.se.dto.DagSynchronisatieDto;
import nl.rivm.screenit.mamma.se.dto.DagverslagDto;
import nl.rivm.screenit.mamma.se.service.MammaAfspraakService;
import nl.rivm.screenit.mamma.se.service.OnderzoekService;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.util.NaamUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

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
	private MammaAfspraakService mammaAfspraakService;

	@Autowired
	private OnderzoekService mammaOnderzoekService;

	@Autowired
	private HibernateService hibernateService;

	@RequestMapping(value = "/{seCode}/{datum}", method = RequestMethod.GET)
	public ResponseEntity getDagverslag(@PathVariable String seCode, @PathVariable @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) Date datum)
	{
		DagverslagOphaler dagverslagOphaler = new DagverslagOphaler(datum, seCode);
		Future future = executorService.submit(dagverslagOphaler);
		try
		{
			LOG.info("Dagverslag ophalen start (" + seCode + ")");
			future.get(ASYNC_REQUEST_TIMEOUT_MS, TimeUnit.MILLISECONDS);
			LOG.info("Dagverslag ophalen eind (" + seCode + ")");
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

	private String getDisplayName(long instellingGebruikerId)
	{
		final InstellingGebruiker instellingGebruiker = hibernateService.get(InstellingGebruiker.class, instellingGebruikerId);
		return NaamUtil.getNaamGebruiker(instellingGebruiker.getMedewerker());
	}

	private Map<String, DagProductieDto> getDagproductieVanSeMedewerkers(String seCode, Date datum)
	{
		Map<String, DagProductieDto> result = new HashMap<>();

		mammaAfspraakService.getIngeschrevenByGebruikerOpDatumVoorSe(datum, seCode).entrySet().stream()
			.filter(entry -> entry.getKey() != null)
			.forEach(entry -> getOrCreateDagproductieDto(entry.getKey(), result).setIngeschrevenCount(entry.getValue()));

		mammaOnderzoekService.getOnderzochtByGebruikerOpDatumVoorSe(datum, seCode).entrySet().stream()
			.filter(entry -> entry.getKey() != null)
			.forEach(entry -> getOrCreateDagproductieDto(entry.getKey(), result).setOnderzochtCount(entry.getValue()));

		mammaOnderzoekService.getAfgerondByGebruikerOpDatumVoorSe(datum, seCode).entrySet().stream()
			.filter(entry -> entry.getKey() != null)
			.forEach(entry -> getOrCreateDagproductieDto(entry.getKey(), result).setAfgerondCount(entry.getValue()));

		mammaOnderzoekService.getOnderbrokenByGebruikerOpDatumVoorSe(datum, seCode).entrySet().stream()
			.filter(entry -> entry.getKey() != null)
			.forEach(entry -> getOrCreateDagproductieDto(entry.getKey(), result).setOnderbrokenCount(entry.getValue()));

		mammaOnderzoekService.getOnvolledigByGebruikerOpDatumVoorSe(datum, seCode).entrySet().stream()
			.filter(entry -> entry.getKey() != null)
			.forEach(entry -> getOrCreateDagproductieDto(entry.getKey(), result).setOnvolledigCount(entry.getValue()));

		mammaOnderzoekService.getAfwijkingenByGebruikerOpDatumVoorSe(datum, seCode).entrySet().stream()
			.filter(entry -> entry.getKey() != null)
			.forEach(entry -> getOrCreateDagproductieDto(entry.getKey(), result).setAfwijkingenCount(entry.getValue()));
		return result;
	}

	private DagProductieDto getOrCreateDagproductieDto(Long instellingGebruikerId, Map<String, DagProductieDto> result)
	{
		DagProductieDto dagproductieDto = result.get(getDisplayName(instellingGebruikerId));
		if (dagproductieDto == null)
		{
			dagproductieDto = new DagProductieDto();
			result.put(getDisplayName(instellingGebruikerId), dagproductieDto);
		}
		return dagproductieDto;
	}

	private DagSynchronisatieDto getSynchronisatieCountVanDag(String seCode, Date datum)
	{
		DagSynchronisatieDto synchronisatieDto = new DagSynchronisatieDto();
		synchronisatieDto.setGemaakt(mammaOnderzoekService.getAantalOnderzoekenMetBeelden(datum, seCode));
		synchronisatieDto.setVerwerkt(mammaOnderzoekService.getAantalOnderzoekenMetBeeldenBeschikbaarInIms(datum, seCode));
		return synchronisatieDto;
	}

	private DagAfsluitingDto getDoorgevoerdCountVanDag(String seCode, Date datum)
	{
		DagAfsluitingDto dagAfsluitingDto = new DagAfsluitingDto();
		dagAfsluitingDto.setAantalDoorgevoerd(mammaOnderzoekService.getAantalDoorgevoerdVanDag(datum, seCode));
		return dagAfsluitingDto;
	}

	private LocalDate getDatumVanOudsteNietAfgeslotenOnderzoek(String seCode)
	{
		return mammaAfspraakService.getDatumVanOudsteNietAfgeslotenOnderzoek(seCode);
	}

	private class DagverslagOphaler extends OpenHibernate5SessionInThread
	{
		private Date datum;

		private String seCode;

		private DagverslagDto dagverslagDto;

		DagverslagOphaler(Date datum, String seCode)
		{
			super(true);
			this.datum = datum;
			this.seCode = seCode;
		}

		@Override
		protected void runInternal()
		{
			dagverslagDto = new DagverslagDto();
			dagverslagDto.setDagproductie(getDagproductieVanSeMedewerkers(seCode, datum));
			dagverslagDto.setDagafsluiting(getDoorgevoerdCountVanDag(seCode, datum));
			dagverslagDto.setDagSynchronisatie(getSynchronisatieCountVanDag(seCode, datum));
			dagverslagDto.setNietAfgeslotenVanaf(getDatumVanOudsteNietAfgeslotenOnderzoek(seCode));
		}

		DagverslagDto getDagverslagDto()
		{
			return dagverslagDto;
		}
	}
}
