package nl.rivm.screenit.batch.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-alg
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

import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.Base64;
import java.util.Objects;
import java.util.zip.ZipInputStream;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.dto.PostcodeNlDto;
import nl.rivm.screenit.model.PostcodeNlProductCode;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.rest.RestApiFactory;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.apache.http.entity.ContentType;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;

@Service
@Slf4j
public class PostcodeNlRestService
{
	@Autowired
	private SimplePreferenceService preferenceService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	public InputStream getDelivery(PostcodeNlProductCode productCode) throws IOException
	{
		String currentTarget = getTarget(productCode);
		if (currentTarget == null || currentTarget.isEmpty() || currentTarget.equalsIgnoreCase("null"))
		{
			LocalDate date = currentDateSupplier.getLocalDate().minusDays(31);
			currentTarget = date.format(DateTimeFormatter.ofPattern(Constants.DATE_FORMAT_YYYYMMDD));
		}

		String uri = String.format("%s://%s%s?deliveryType=complete&productCode=%s&after=%s",
			preferenceService.getString(PreferenceKey.POSTCODE_NL_API_SCHEME.name()),
			preferenceService.getString(PreferenceKey.POSTCODE_NL_API_HOST.name()),
			preferenceService.getString(PreferenceKey.POSTCODE_NL_API_DELIVERYPATH.name()),
			productCode.getCode(),
			currentTarget);
		RestTemplate restApi = RestApiFactory.create();
		LOG.info("Calling {}", uri);
		ResponseEntity<PostcodeNlDto[]> response = restApi.exchange(uri, HttpMethod.GET, getPostcodeNlRequest(), PostcodeNlDto[].class);

		if (response.getStatusCode() == HttpStatus.OK)
		{
			LOG.info("{} from server at {}", response.getStatusCode(), uri);
			if (Objects.requireNonNull(response.getBody()).length > 0 && !response.getBody()[0].getDeliveryTarget().equals(getTarget(productCode)))
			{
				PostcodeNlDto postcodeNlDto = response.getBody()[0];
				LOG.info("{} delivery beschikbaar met target = {}. Huidig target = {}", productCode.getCode(), postcodeNlDto.getDeliveryTarget(), getTarget(productCode));
				setTarget(productCode, postcodeNlDto);
				return openStream(postcodeNlDto.getDownloadUrl());
			}
			else
			{
				LOG.info("Geen nieuwe delivery beschikbaar.");
			}
		}
		else
		{
			LOG.warn("{} from server at {}", response.getStatusCode(), uri);
		}
		return null;
	}

	private String getTarget(PostcodeNlProductCode productCode)
	{
		switch (productCode)
		{
		case WPL:
		{
			return preferenceService.getString(PreferenceKey.POSTCODE_NL_API_TARGET_WPL.name());
		}
		case NUM:
		{
			return preferenceService.getString(PreferenceKey.POSTCODE_NL_API_TARGET_NUM.name());
		}
		default:
		{
			LOG.error("Kon geen target ophalen, invalide productcode: {}", productCode);
			return null;
		}
		}
	}

	private void setTarget(PostcodeNlProductCode productCode, PostcodeNlDto download)
	{
		switch (productCode)
		{
		case WPL:
		{
			preferenceService.putString(PreferenceKey.POSTCODE_NL_API_TARGET_WPL.name(), download.getDeliveryTarget());
			break;
		}
		case NUM:
		{
			preferenceService.putString(PreferenceKey.POSTCODE_NL_API_TARGET_NUM.name(), download.getDeliveryTarget());
			break;
		}
		default:
			LOG.error("Kon geen target zetten, invalide productcode: {}", productCode);
			break;
		}
	}

	private InputStream openStream(String url) throws IOException
	{
		InputStream stream = new URL(url).openStream();
		ZipInputStream zipStream = new ZipInputStream(stream);
		zipStream.getNextEntry();
		return zipStream;
	}

	String getAuthorization()
	{
		String key = preferenceService.getString(PreferenceKey.POSTCODE_NL_API_KEY.name());
		String secret = preferenceService.getString(PreferenceKey.POSTCODE_NL_API_SECRET.name());
		String authorization = Base64.getEncoder().encodeToString((key + ":" + secret).getBytes());
		return authorization;
	}

	private HttpEntity<String> getPostcodeNlRequest()
	{
		HttpHeaders headers = new HttpHeaders();
		headers.add("Authorization", "Basic " + getAuthorization());
		headers.add("Content-Type", ContentType.APPLICATION_JSON.getMimeType());
		return new HttpEntity<>(headers);
	}
}
