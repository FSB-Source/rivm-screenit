package nl.rivm.screenit.mamma.se.proxy.services.impl;

/*-
 * ========================LICENSE_START=================================
 * se-proxy
 * %%
 * Copyright (C) 2017 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.LinkedBlockingDeque;
import java.util.stream.Collectors;

import nl.rivm.screenit.mamma.se.proxy.model.RequestTypeCentraal;
import nl.rivm.screenit.mamma.se.proxy.model.SeConfiguratieKey;
import nl.rivm.screenit.mamma.se.proxy.services.AchtergrondRequestService;
import nl.rivm.screenit.mamma.se.proxy.services.CleanUpService;
import nl.rivm.screenit.mamma.se.proxy.services.ConfiguratieService;
import nl.rivm.screenit.mamma.se.proxy.services.ProxyService;
import nl.rivm.screenit.mamma.se.proxy.services.SeDaglijstService;
import nl.rivm.screenit.mamma.se.proxy.services.TransactionQueueService;
import nl.rivm.screenit.mamma.se.proxy.util.DateUtil;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.RequestEntity;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

@Service
public class SeDaglijstServiceImpl implements SeDaglijstService
{
	private static final Logger LOG = LoggerFactory.getLogger(SeDaglijstService.class);

	private final ConcurrentHashMap<LocalDate, String> daglijstCache = new ConcurrentHashMap<>();

	private final ConcurrentHashMap<LocalDate, LinkedBlockingDeque<String>> transactiesVerwerktDoorCentraalNaCachenDaglijst = new ConcurrentHashMap<>();

	private final Object daglijstEnTransactieLock = new Object();

	private final Object cleanupLock = new Object();

	private LocalDate laatsteUpdateDag;

	@Autowired
	private TransactionQueueService transactionQueueService;

	@Autowired
	private ProxyService proxyService;

	@Autowired
	private ConfiguratieService configuratieService;

	@Autowired
	private CleanUpService cleanUpService;

	@Autowired
	private AchtergrondRequestService achtergrondRequestService;

	@Override
	public String getDaglijst(LocalDate datum)
	{
		startOfDayCleanupUitvoerenIndienNodig();

		String daglijst = daglijstCache.get(datum);
		if (daglijst == null)
		{
			getDaglijstGeforceerd(datum);
		}
		return daglijstCache.get(datum);
	}

	@Override
	public String getDaglijstGeforceerd(LocalDate opTeHalenDag)
	{
		synchronized (daglijstEnTransactieLock)
		{
			ResponseEntity<String> responseEntity = daglijstRequest(opTeHalenDag);
			if (responseEntity != null && HttpStatus.OK.equals(responseEntity.getStatusCode()))
			{
				LOG.info("Daglijst van [" + opTeHalenDag + "] succesvol binnengehaald van SE-REST-BK");
				String daglijst = responseEntity.getBody();
				cacheDaglijst(opTeHalenDag, daglijst);
				return daglijstCache.get(opTeHalenDag);
			}
			else
			{
				LOG.warn("Kon daglijst van [" + opTeHalenDag + "] niet ophalen van SE-REST-BK");
				if (responseEntity == null || responseEntity.getStatusCode().equals(HttpStatus.FORBIDDEN))
				{
					achtergrondRequestService.queueDaglijstRequest(opTeHalenDag);
				}
				return null;
			}
		}
	}

	private ResponseEntity<String> daglijstRequest(LocalDate datum)
	{
		RequestEntity.BodyBuilder requestBuilder = proxyService
			.getProxyRequestEntity(RequestTypeCentraal.GET_DAGLIJST.getPathPostfix() + "/" + datum.format(DateTimeFormatter.ISO_DATE), HttpMethod.GET);
		return proxyService.sendUncheckedProxyRequest(requestBuilder.build(), String.class);
	}

	private void cacheDaglijst(LocalDate datum, String afspraakDtos)
	{
		if (afspraakDtos != null)
		{
			daglijstCache.put(datum, afspraakDtos);
			transactiesVerwerktDoorCentraalNaCachenDaglijst.getOrDefault(datum, new LinkedBlockingDeque<>()).clear();
		}
	}

	@Override
	public void voegVerwerkteTransactionDtoToe(LocalDate dag, String transactionDto)
	{
		LinkedBlockingDeque<String> dagVerwerkteTransactionDtos = transactiesVerwerktDoorCentraalNaCachenDaglijst.getOrDefault(dag, new LinkedBlockingDeque<>());
		dagVerwerkteTransactionDtos.add(transactionDto);
		transactiesVerwerktDoorCentraalNaCachenDaglijst.put(dag, dagVerwerkteTransactionDtos);
	}

	@Override
	public List<String> getDaglijstMutaties(LocalDate datum)
	{
		List<String> nogTeVerwerkenTransacties = transactionQueueService.getPendingTransactionDtos(datum);
		List<String> verwerktEnTeVerwerkenTransacties = new ArrayList<>(transactiesVerwerktDoorCentraalNaCachenDaglijst.getOrDefault(datum, new LinkedBlockingDeque<>()));
		verwerktEnTeVerwerkenTransacties.addAll(nogTeVerwerkenTransacties);
		return verwerktEnTeVerwerkenTransacties;
	}

	@Override
	public void queueDaglijstOphalenVanDag(LocalDate dag)
	{
		achtergrondRequestService.queueDaglijstRequest(dag);
		achtergrondRequestService.ensureRunning();
	}

	private void startOfDayCleanupUitvoerenIndienNodig()
	{
		synchronized (cleanupLock)
		{
			LocalDate vandaag = DateUtil.getCurrentDateTime().toLocalDate();
			if (laatsteUpdateDag == null || vandaag.isAfter(laatsteUpdateDag))
			{
				laatsteUpdateDag = vandaag;
				cleanUpService.startOfDayCleanup();
			}
		}
	}

	@Override
	public void haalDaglijstenOp()
	{
		Integer daglijstOphalenVoorDagen = configuratieService.getConfiguratieIntegerValue(SeConfiguratieKey.SE_DAGLIJST_OPHALEN_VOOR_DAGEN);
		LOG.info("Schoon cache van daglijsten en verwerkte transacties op en haal daglijst voor gisteren, vandaag en " + daglijstOphalenVoorDagen
			+ " dag(en) in de toekomst op van SE-REST-BK");
		achtergrondRequestService.verwijderAlleOpTeHalenDaglijsten();

		List<LocalDate> teVerversenDagen = teVerversenDaglijsten(daglijstOphalenVoorDagen);

		daglijstCache.entrySet().removeIf(entry -> !teVerversenDagen.contains(entry.getKey()));
		transactiesVerwerktDoorCentraalNaCachenDaglijst.entrySet().removeIf(entry -> !teVerversenDagen.contains(entry.getKey()));

		teVerversenDagen.forEach(achtergrondRequestService::queueDaglijstRequest);
		achtergrondRequestService.ensureRunning();
	}

	private List<LocalDate> teVerversenDaglijsten(Integer daglijstOphalenVoorDagen)
	{
		List<LocalDate> teVerversenDagen = new ArrayList<>();
		LocalDate vandaag = DateUtil.getCurrentDateTime().toLocalDate();
		LocalDate gisteren = vandaag.minusDays(1);

		teVerversenDagen.add(gisteren);
		for (int i = 0; i <= daglijstOphalenVoorDagen; i++)
		{
			teVerversenDagen.add(vandaag.plusDays(i));
		}
		return teVerversenDagen;
	}

	@Override
	public Object getDaglijstEnTransactieLock()
	{
		return daglijstEnTransactieLock;
	}

	@Override
	public String dagenInCache()
	{
		return daglijstCache.keySet().stream()
			.sorted()
			.map(d -> d.format(DateTimeFormatter.ISO_DATE))
			.collect(Collectors.joining(", "));
	}
}
