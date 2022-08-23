package nl.rivm.screenit.service.mamma.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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
import java.time.LocalTime;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.NavigableSet;
import java.util.TreeSet;
import java.util.UUID;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import javax.annotation.PostConstruct;

import nl.rivm.screenit.dto.mamma.planning.PlanningBlokkadeDto;
import nl.rivm.screenit.dto.mamma.planning.PlanningCapaciteitBlokDto;
import nl.rivm.screenit.dto.mamma.planning.PlanningConceptMeldingenDto;
import nl.rivm.screenit.dto.mamma.planning.PlanningDagKopierenDto;
import nl.rivm.screenit.dto.mamma.planning.PlanningDto;
import nl.rivm.screenit.dto.mamma.planning.PlanningHerhalenDto;
import nl.rivm.screenit.dto.mamma.planning.PlanningPostcodeReeksDto;
import nl.rivm.screenit.dto.mamma.planning.PlanningRestConstants;
import nl.rivm.screenit.dto.mamma.planning.PlanningRouteWijzigenDto;
import nl.rivm.screenit.dto.mamma.planning.PlanningScreeningsEenheidDto;
import nl.rivm.screenit.dto.mamma.planning.PlanningScreeningsEenheidMetaDataDto;
import nl.rivm.screenit.dto.mamma.planning.PlanningScreeningsOrganisatieDto;
import nl.rivm.screenit.dto.mamma.planning.PlanningStandplaatsDto;
import nl.rivm.screenit.dto.mamma.planning.PlanningStandplaatsPeriodeDto;
import nl.rivm.screenit.dto.mamma.planning.PlanningStatusDto;
import nl.rivm.screenit.dto.mamma.planning.PlanningVerzetClientenDto;
import nl.rivm.screenit.dto.mamma.planning.PlanningWeekDto;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.mamma.MammaBlokkade;
import nl.rivm.screenit.model.mamma.MammaPostcodeReeks;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.MammaStandplaats;
import nl.rivm.screenit.model.mamma.enums.MammaPlanningStatus;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.mamma.MammaBaseConceptPlanningsApplicatie;
import nl.rivm.screenit.util.rest.RestApiFactory;

import org.apache.commons.lang3.StringUtils;
import org.joda.time.DateTime;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.client.HttpServerErrorException;
import org.springframework.web.client.ResourceAccessException;
import org.springframework.web.client.RestClientException;
import org.springframework.web.client.RestTemplate;

@Service
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class MammaBaseConceptPlanningsApplicatieImpl implements MammaBaseConceptPlanningsApplicatie
{
	private final class VerzetClientenThread extends Thread
	{
		private List<PlanningVerzetClientenDto> requestDtos = new ArrayList<>();

		private Object mutex = new Object();

		void addRequest(PlanningVerzetClientenDto verzetClientenDto)
		{
			synchronized (mutex)
			{
				requestDtos.add(verzetClientenDto);
				if (requestDtos.size() > 100) 
				{
					requestDtos.remove(0);
				}
			}
		}

		@Override
		public void run()
		{
			while (true)
			{
				PlanningVerzetClientenDto currentRequestDto = null;
				try
				{
					while (true)
					{
						synchronized (mutex)
						{
							currentRequestDto = null;
							if (!requestDtos.isEmpty())
							{
								currentRequestDto = requestDtos.get(0);
							}
						}
						if (currentRequestDto != null)
						{
							sendToPlanningApplicatie(PlanningRestConstants.C_CLIENT, currentRequestDto, false, null, Integer.valueOf(120000)); 
							synchronized (mutex)
							{
								requestDtos.remove(currentRequestDto);
								currentRequestDto = null;
							}
						}
						else
						{
							Thread.sleep(500);
						}
					}
				}
				catch (Exception e)
				{
					LOG.error("Fout bij versturen verzet clienten request ", e);

					synchronized (mutex)
					{
						requestDtos.remove(currentRequestDto);
					}
					try
					{
						Thread.sleep(2000); 
					}
					catch (InterruptedException e1)
					{
						LOG.error("Sleep interrupted ", e1);
					}
				}
			}
		}
	}

	private static final Logger LOG = LoggerFactory.getLogger(MammaBaseConceptPlanningsApplicatieImpl.class);

	@Autowired
	private LogService logService;

	@Autowired
	@Qualifier("planningBkRestUrl")
	private String planningBkRestUrl;

	private static final ExecutorService EXECUTOR_SERVICE = Executors.newSingleThreadExecutor();

	private VerzetClientenThread verzetClientenThread;

	@PostConstruct
	public void init()
	{
		if (StringUtils.isNotBlank(planningBkRestUrl) && !planningBkRestUrl.endsWith("/"))
		{
			planningBkRestUrl += "/";
		}

		verzetClientenThread = new VerzetClientenThread();
		EXECUTOR_SERVICE.submit(verzetClientenThread);
	}

	@Override
	public void sendPostcodeReeks(MammaPostcodeReeks postcodeReeks, boolean isNieuw)
	{
		PlanningPostcodeReeksDto dto = new PlanningPostcodeReeksDto();
		dto.standplaatsId = postcodeReeks.getStandplaats().getId();
		dto.totPostcode = postcodeReeks.getTotPostcode();
		dto.vanPostcode = postcodeReeks.getVanPostcode();
		dto.id = postcodeReeks.getId();

		sendToPlanningApplicatie(PlanningRestConstants.C_POSTCODEREEKS, dto, isNieuw, null);

	}

	private <T extends PlanningDto> void sendToPlanningApplicatie(String context, T dto, boolean isNieuw, InstellingGebruiker ingelogdeInstellingGebruiker)
	{
		sendToPlanningApplicatie(context, dto, isNieuw, ingelogdeInstellingGebruiker, null);
	}

	private <T extends PlanningDto> void sendToPlanningApplicatie(String context, T dto, boolean isNieuw, InstellingGebruiker ingelogdeInstellingGebruiker, Integer readTimeout)
	{
		RestTemplate restApi = RestApiFactory.create(readTimeout);
		if (isNieuw)
		{
			restApi.postForEntity(planningBkRestUrl + context, dto, String.class);
		}
		else
		{
			restApi.put(planningBkRestUrl + context, dto, String.class);
		}
		sendGewijzigdDoor(ingelogdeInstellingGebruiker, restApi);
	}

	private void sendGewijzigdDoor(InstellingGebruiker ingelogdeInstellingGebruiker, RestTemplate restApi)
	{
		if (ingelogdeInstellingGebruiker != null)
		{
			restApi.postForEntity(planningBkRestUrl + PlanningRestConstants.C_ACTIE + "/conceptGewijzigdDoor/" + ingelogdeInstellingGebruiker.getId() + "/"
				+ ingelogdeInstellingGebruiker.getOrganisatie().getId(), null, String.class);
		}
	}

	@Override
	public Long[] getConceptGewijzigdDoor(ScreeningOrganisatie screeningOrganisatie)
	{
		RestTemplate restApi = RestApiFactory.create();
		ResponseEntity<Long[]> result = restApi.getForEntity(planningBkRestUrl + PlanningRestConstants.C_ACTIE + "/conceptGewijzigdDoor/" + screeningOrganisatie.getId(),
			Long[].class);

		return result.getBody();
	}

	@Override
	public NavigableSet<String> getUncoveredPostcodes(ScreeningOrganisatie screeningOrganisatie)
	{
		RestTemplate restApi = RestApiFactory.create();
		Long screeningOrganisatieId = 0L;
		if (screeningOrganisatie != null)
		{
			screeningOrganisatieId = screeningOrganisatie.getId();
		}
		ResponseEntity<NavigableSet> result = restApi.getForEntity(planningBkRestUrl + PlanningRestConstants.C_UNCOVEREDPOSTCODES + "/" + screeningOrganisatieId,
			NavigableSet.class);
		NavigableSet resultSet = result.getBody();
		if (resultSet == null)
		{
			resultSet = new TreeSet<>();
		}
		if (resultSet.isEmpty())
		{
			resultSet.add("Geen");
		}
		return resultSet;
	}

	@Override
	public void deletePostcodeReeks(MammaPostcodeReeks postcodeReeks)
	{
		sendDeleteToPlanningApplicatie(PlanningRestConstants.C_POSTCODEREEKS + "/" + postcodeReeks.getId(), null);
	}

	private void sendDeleteToPlanningApplicatie(String payload, InstellingGebruiker ingelogdeGebruiker)
	{
		RestTemplate restApi = RestApiFactory.create();
		restApi.delete(planningBkRestUrl + payload);
		sendGewijzigdDoor(ingelogdeGebruiker, restApi);
	}

	@Override
	public void sendStandplaats(MammaStandplaats standplaats, boolean isNieuw)
	{
		if (standplaats.getActief())
		{
			PlanningStandplaatsDto dto = new PlanningStandplaatsDto();
			dto.id = standplaats.getId();
			dto.screeningsOrganisatieId = standplaats.getRegio().getId();

			sendToPlanningApplicatie(PlanningRestConstants.C_STANDPLAATS, dto, isNieuw, null);
		}
		else
		{
			sendDeleteToPlanningApplicatie(PlanningRestConstants.C_STANDPLAATS + "/" + standplaats.getId(), null);
		}
	}

	@Override
	public void sendScreeningsEenheid(MammaScreeningsEenheid screeningsEenheid, boolean isNieuw)
	{
		if (screeningsEenheid.getActief())
		{
			PlanningScreeningsEenheidDto dto = new PlanningScreeningsEenheidDto();
			dto.id = screeningsEenheid.getId();
			dto.aantalMammografen = Math.max(screeningsEenheid.getMammografen().size(), 1); 
			dto.screeningsOrganisatieId = screeningsEenheid.getBeoordelingsEenheid().getParent().getRegio().getId();

			sendToPlanningApplicatie(PlanningRestConstants.C_SCREENINGSEENHEID, dto, isNieuw, null);
		}
		else
		{
			sendDeleteToPlanningApplicatie(PlanningRestConstants.C_SCREENINGSEENHEID + "/" + screeningsEenheid.getId(), null);
		}
	}

	@Override
	public PlanningWeekDto getWeek(MammaScreeningsEenheid screeningsEenheid, DateTime start)
	{
		RestTemplate restApi = RestApiFactory.create();
		ResponseEntity<PlanningWeekDto> responseEntity = restApi.getForEntity(
			planningBkRestUrl + PlanningRestConstants.C_WEEK + "/" + screeningsEenheid.getId() + "/" + start.getMillis(),
			PlanningWeekDto.class);
		return responseEntity.getBody();
	}

	@Override
	public String getAfspraakDrempelOverzichtStandplaats(long standplaatsId)
	{
		RestTemplate restApi = RestApiFactory.create();
		return restApi.getForObject(planningBkRestUrl + PlanningRestConstants.C_STANDPLAATS + "/" + "getAfspraakDrempelOverzicht/" + standplaatsId, String.class);
	}

	@Override
	public String getAfspraakDrempelOverzichtScreeningsOrganisatie(long screeningsOrganisatieId)
	{
		RestTemplate restApi = RestApiFactory.create();
		return restApi.getForObject(planningBkRestUrl + PlanningRestConstants.C_SCREENINGS_ORGANISATIE + "/" + "getAfspraakDrempelOverzicht/" + screeningsOrganisatieId,
			String.class);
	}

	@Override
	public PlanningScreeningsEenheidMetaDataDto getScreeningsEenheidMetaData(MammaScreeningsEenheid screeningEenheid)
	{
		RestTemplate restApi = RestApiFactory.create();
		ResponseEntity<PlanningScreeningsEenheidMetaDataDto> responseEntity = restApi
			.getForEntity(planningBkRestUrl + PlanningRestConstants.C_SCREENINGSEENHEID + "/metaData/" + screeningEenheid.getId(),
				PlanningScreeningsEenheidMetaDataDto.class);
		return responseEntity.getBody();
	}

	@Override
	public void sendCapaciteitBlok(PlanningCapaciteitBlokDto blok, boolean isNieuw, InstellingGebruiker ingelogdeGebruiker)
	{
		sendToPlanningApplicatie(PlanningRestConstants.C_CAPACITEITBLOK, blok, isNieuw, ingelogdeGebruiker);
	}

	@Override
	public String deleteCapaciteitBlok(PlanningCapaciteitBlokDto blok, InstellingGebruiker ingelogdeGebruiker)
	{
		try
		{
			sendDeleteToPlanningApplicatie(PlanningRestConstants.C_CAPACITEITBLOK + "/" + blok.conceptId, ingelogdeGebruiker);
		}
		catch (HttpClientErrorException | HttpServerErrorException se)
		{
			return se.getResponseBodyAsString();
		}
		catch (RestClientException e)
		{
			return e.getMessage();
		}
		return null;
	}

	@Override
	public PlanningStandplaatsPeriodeDto[] getStandplaatsPeriodesSorted(MammaScreeningsEenheid screeningsEenheid)
	{
		RestTemplate restApi = RestApiFactory.create();
		ResponseEntity<PlanningStandplaatsPeriodeDto[]> responseEntity = restApi.getForEntity(
			planningBkRestUrl + PlanningRestConstants.C_ROUTE + "/" + screeningsEenheid.getId(), PlanningStandplaatsPeriodeDto[].class);
		return responseEntity.getBody();
	}

	@Override
	public void changeRoute(PlanningStandplaatsPeriodeDto standplaatsPeriodeDto, MammaScreeningsEenheid screeningsEenheid, InstellingGebruiker ingelogdeInstellingGebruiker)
	{
		PlanningRouteWijzigenDto wijziging = new PlanningRouteWijzigenDto();
		wijziging.screeningsEenheidId = screeningsEenheid.getId();
		wijziging.standplaatsId = standplaatsPeriodeDto.standplaatsId;
		wijziging.volgNr = standplaatsPeriodeDto.screeningsEenheidVolgNr;
		wijziging.standplaatsPeriodeConceptId = standplaatsPeriodeDto.conceptId;
		sendToPlanningApplicatie(PlanningRestConstants.C_ROUTE, wijziging, false, ingelogdeInstellingGebruiker);
	}

	@Override
	public void splitsStandplaatsPeriode(PlanningStandplaatsPeriodeDto standplaatsPeriodeDto, InstellingGebruiker ingelogdeInstellingGebruiker)
	{
		sendToPlanningApplicatie(PlanningRestConstants.C_ROUTE + "/splitsStandplaatsPeriode/" + standplaatsPeriodeDto.conceptId, null, false, ingelogdeInstellingGebruiker);
	}

	@Override
	public void sendAfspraakDrempelStandplaatsPeriode(PlanningStandplaatsPeriodeDto standplaatsPeriodeDto, InstellingGebruiker ingelogdeInstellingGebruiker)
	{
		sendToPlanningApplicatie(PlanningRestConstants.C_STANDPLAATS_PERIODE, standplaatsPeriodeDto, false, ingelogdeInstellingGebruiker);
	}

	@Override
	public Long[] getStandplaatsenZonderRoute(ScreeningOrganisatie screeningOrganisatie)
	{
		RestTemplate restApi = RestApiFactory.create();
		ResponseEntity<Long[]> result = restApi.getForEntity(planningBkRestUrl + PlanningRestConstants.C_STANDPLAATS + "/zonderRoute/" + screeningOrganisatie.getId(),
			Long[].class);
		return result.getBody();
	}

	@Override
	public Long[] getStandplaatsenMetRoute(ScreeningOrganisatie screeningOrganisatie)
	{
		RestTemplate restApi = RestApiFactory.create();
		ResponseEntity<Long[]> result = restApi.getForEntity(planningBkRestUrl + PlanningRestConstants.C_STANDPLAATS + "/metRoute/" + screeningOrganisatie.getId(),
			Long[].class);
		return result.getBody();
	}

	@Override
	public PlanningConceptMeldingenDto saveConcept(InstellingGebruiker ingelogdeInstellingGebruiker, boolean runDry)
	{
		RestTemplate restApi = RestApiFactory.create();
		ResponseEntity<PlanningConceptMeldingenDto> result = restApi.getForEntity(
			planningBkRestUrl + PlanningRestConstants.C_ACTIE + "/conceptOpslaan/" + ingelogdeInstellingGebruiker.getOrganisatie().getId() + "/" + runDry,
			PlanningConceptMeldingenDto.class);
		if (!runDry)
		{
			logService.logGebeurtenis(LogGebeurtenis.MAMMA_PLANNING_CONCEPT_OPGESLAGEN, ingelogdeInstellingGebruiker, Bevolkingsonderzoek.MAMMA);
		}
		return result.getBody();
	}

	@Override
	public void conceptAnnuleren(InstellingGebruiker ingelogdeInstellingGebruiker)
	{
		RestTemplate restApi = RestApiFactory.create();
		restApi.postForEntity(planningBkRestUrl + PlanningRestConstants.C_ACTIE + "/conceptAnnuleren/" + ingelogdeInstellingGebruiker.getOrganisatie().getId(), null,
			String.class);
	}

	@Override
	public void sendBlokkade(MammaBlokkade blokkade, boolean isNieuw)
	{
		if (blokkade.getId() == null || Boolean.TRUE.equals(blokkade.getActief()))
		{
			PlanningBlokkadeDto blokkadeDto = new PlanningBlokkadeDto();
			blokkadeDto.id = blokkade.getId();
			blokkadeDto.blokkadeType = blokkade.getType();
			if (blokkade.getScreeningsEenheid() != null)
			{
				blokkadeDto.screeningsEenheidId = blokkade.getScreeningsEenheid().getId();
			}
			else if (blokkade.getRegio() != null)
			{
				blokkadeDto.screeningsOrganisatieId = blokkade.getRegio().getId();
			}
			else if (blokkade.getStandplaats() != null)
			{
				blokkadeDto.standplaatsId = blokkade.getStandplaats().getId();
			}
			blokkadeDto.totEnMet = blokkade.getTotEnMet();
			blokkadeDto.vanaf = blokkade.getVanaf();
			sendToPlanningApplicatie(PlanningRestConstants.C_BLOKKADE, blokkadeDto, isNieuw, null);
		}
		else if (Boolean.FALSE.equals(blokkade.getActief()))
		{
			sendDeleteToPlanningApplicatie(PlanningRestConstants.C_BLOKKADE + "/" + blokkade.getId(), null);
		}
	}

	@Override
	public int getAantalAfsprakenOpBlok(PlanningCapaciteitBlokDto blokDto, boolean toDelete)
	{
		RestTemplate restApi = RestApiFactory.create();
		ResponseEntity<Integer> responseEntity = restApi.getForEntity(
			planningBkRestUrl + PlanningRestConstants.C_CAPACITEITBLOK + "/aantalAfsprakenOpBlok/" + (blokDto.conceptId != null ? blokDto.conceptId : UUID.randomUUID())
				+ "/" + blokDto.blokType + "/" + blokDto.vanaf.getTime()
				+ "/" + blokDto.tot.getTime() + "/" + toDelete,
			Integer.class);
		return responseEntity.getBody();
	}

	@Override
	public void herhaalWeek(MammaScreeningsEenheid screeningsEenheidVan, MammaScreeningsEenheid screeningsEenheidNaar, LocalDate teHerhalenWeek, LocalDate herhalenVanafWeek,
		LocalDate herhalenTotEnMetWeek, InstellingGebruiker ingelogdeInstellingGebruiker)
	{
		PlanningHerhalenDto dto = new PlanningHerhalenDto();
		dto.screeningsEenheidIdVan = screeningsEenheidVan.getId();
		dto.screeningsEenheidIdNaar = screeningsEenheidNaar.getId();
		dto.teHerhalenWeek = teHerhalenWeek;
		dto.herhalenVanaf = herhalenVanafWeek;
		dto.herhalenTotEnMet = herhalenTotEnMetWeek;

		sendToPlanningApplicatie(PlanningRestConstants.C_WEEK, dto, false, ingelogdeInstellingGebruiker);
	}

	@Override
	public void updateScreeningsOrganisatie(PlanningScreeningsOrganisatieDto organisatieDto)
	{
		try
		{
			sendToPlanningApplicatie(PlanningRestConstants.C_SCREENINGS_ORGANISATIE, organisatieDto, false, null);
		}
		catch (Exception e)
		{
			LOG.error(
				"Fout bij opslaan van SO wijzigingen tbv. planning. Of de planning-bk applicatie draait niet of er is een nieuwe SO aangemaakt zonder na aanmaken de reset gedraaid te hebben.",
				e);
		}
	}

	@Override
	public Date getPlannenTotEnMetDatum()
	{
		RestTemplate restApi = RestApiFactory.create();
		ResponseEntity<Date> responseEntity = restApi.getForEntity(
			planningBkRestUrl + PlanningRestConstants.C_WEEK + "/plannenTotEnMetDatum", Date.class);
		return responseEntity.getBody();
	}

	@Override
	public void verzetClienten(PlanningVerzetClientenDto verzetClientenDto)
	{
		verzetClientenThread.addRequest(verzetClientenDto);
	}

	@Override
	public PlanningStatusDto getStatus()
	{
		RestTemplate restApi = RestApiFactory.create();
		try
		{
			ResponseEntity<PlanningStatusDto> responseEntity = restApi.getForEntity(
				planningBkRestUrl + PlanningRestConstants.C_STATUS, PlanningStatusDto.class);
			return responseEntity.getBody();
		}
		catch (ResourceAccessException e)
		{

		}
		return new PlanningStatusDto(MammaPlanningStatus.OFFLINE, null);
	}

	@Override
	public void kopieerDag(MammaScreeningsEenheid bronScreeningsEenheid, MammaScreeningsEenheid doelScreeningsEenheid, LocalDate bronDag, LocalTime bronVanTijd,
		LocalTime bronTotTijd, LocalDate doelDag, InstellingGebruiker ingelogdeInstellingGebruiker)
	{
		var dto = new PlanningDagKopierenDto();
		dto.bronScreeningsEenheidId = bronScreeningsEenheid.getId();
		dto.doelScreeningsEenheidId = doelScreeningsEenheid.getId();
		dto.bronDatum = bronDag;
		dto.bronVanTijd = bronVanTijd;
		dto.bronTotTijd = bronTotTijd;
		dto.doelDatum = doelDag;

		sendToPlanningApplicatie(PlanningRestConstants.C_DAG_KOPIEREN, dto, false, ingelogdeInstellingGebruiker);
	}
}
