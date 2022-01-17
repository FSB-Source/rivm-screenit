package nl.rivm.screenit.mamma.se.proxy.services.impl;

/*-
 * ========================LICENSE_START=================================
 * se-proxy
 * %%
 * Copyright (C) 2017 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.io.IOException;
import java.time.Duration;
import java.time.LocalDateTime;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import nl.rivm.screenit.mamma.se.proxy.model.AutorisatieDto;
import nl.rivm.screenit.mamma.se.proxy.model.LogischeSessie;
import nl.rivm.screenit.mamma.se.proxy.model.NavigatieDto;
import nl.rivm.screenit.mamma.se.proxy.services.LogischeSessieService;
import nl.rivm.screenit.mamma.se.proxy.util.DateUtil;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;

@Service
public class LogischeSessieServiceImpl implements LogischeSessieService
{
	private Map<String, LogischeSessie> logischeSessies = new ConcurrentHashMap<>();

	private static final int HALF_UUR = 30 * 60 * 1000;

	private final ObjectMapper objectMapper = new ObjectMapper();

	private static final Logger LOG = LoggerFactory.getLogger(LogischeSessieService.class);

	@Value("${DISABLE_NFC_AUTHENTICATION:#{false}}")
	private boolean disableNFCAuthentication;

	@Override
	public void addLogischeSessie(LogischeSessie logischeSessie)
	{
		logischeSessies.put(logischeSessie.getYubikeyIdentificatie(), logischeSessie);
		logLogischeSessieWijziging("add", logischeSessie);
	}

	@Override
	public LogischeSessie getLogischeSessieMetIdentificatie(String yubikeyIdentificatie)
	{
		return logischeSessies.get(yubikeyIdentificatie);
	}

	@Override
	public void updateLogischeSessie(LogischeSessie logischeSessie, NavigatieDto navigatie)
	{
		logischeSessie.setNavigatie(navigatie);
		putNavigatieInLoginAntwoord(logischeSessie, navigatie);
		logischeSessie.setLaatsteUpdate(DateUtil.getCurrentDateTime());
	}

	@Override
	public void verwijderLogischeSessie(LogischeSessie logischeSessie)
	{
		if (logischeSessie == null)
		{
			return;
		}
		logLogischeSessieWijziging("delete", logischeSessie);
		logischeSessies.remove(logischeSessie.getYubikeyIdentificatie());
	}

	@Override
	public boolean isVerlopen(LogischeSessie logischeSessie)
	{
		LocalDateTime laatsteUpdate = logischeSessie.getLaatsteUpdate();
		Duration duration = Duration.between(laatsteUpdate, DateUtil.getCurrentDateTime());
		return duration.toMillis() > HALF_UUR || duration.toMillis() < 0;
	}

	@Override
	public boolean geldigeYubikey(String yubikeyIdentificatie)
	{
		LogischeSessie logischeSessie = getLogischeSessieMetIdentificatie(yubikeyIdentificatie);
		return disableNFCAuthentication || (logischeSessie != null && !isVerlopen(logischeSessie));
	}

	@Override
	public boolean zijnErNietVerlopenSessies()
	{
		return logischeSessies.entrySet().stream().anyMatch(entry -> !isVerlopen(entry.getValue()));
	}

	private void putNavigatieInLoginAntwoord(LogischeSessie logischeSessie, NavigatieDto navigatie)
	{
		ResponseEntity loginAntwoord = logischeSessie.getLoginAntwoord();
		try
		{
			AutorisatieDto autorisatieDto = objectMapper.readValue(logischeSessie.getLoginAntwoord().getBody(), AutorisatieDto.class);
			autorisatieDto.setNavigatie(objectMapper.writeValueAsString(navigatie));
			String newAutorisatieDto = objectMapper.writeValueAsString(autorisatieDto).replace("navigatieType", "type");
			logischeSessie.setLoginAntwoord(ResponseEntity.status(loginAntwoord.getStatusCode()).headers(loginAntwoord.getHeaders()).body(newAutorisatieDto));
		}
		catch (IOException ex)
		{
			LOG.warn(ex.getMessage());
			LOG.warn("Kon oorsponkelijk login antwoord niet parsen: " + logischeSessie.getLoginAntwoord().getBody());
		}
	}

	private void logLogischeSessieWijziging(String methode, LogischeSessie logischeSessie)
	{
		String loginAntwoordHttpStatusCode = logischeSessie.getLoginAntwoord() != null ? String.valueOf(logischeSessie.getLoginAntwoord().getStatusCodeValue()) : "null";
		LOG.info("logische sessie {}: {}, {}", methode, logischeSessie.getYubikeyIdentificatie(), loginAntwoordHttpStatusCode);
	}
}
