package nl.rivm.screenit.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.OrganisatieParameter;
import nl.rivm.screenit.model.OrganisatieParameterKey;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.service.InstellingService;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.OrganisatieParameterService;
import nl.rivm.screenit.util.BigDecimalUtil;
import nl.rivm.screenit.util.EntityAuditUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.commons.lang.StringUtils;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import ca.uhn.hl7v2.util.StringUtil;

@Slf4j
@Service
@AllArgsConstructor
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class OrganisatieParameterServiceImpl implements OrganisatieParameterService
{
	private static final String GEEN_WAARDE = "(geen waarde)";

	private InstellingService instellingService;

	private HibernateService hibernateService;

	private LogService logService;

	private final Map<String, Long> paramKeyIds = new ConcurrentHashMap<>();

	@Override
	public <T> T getOrganisatieParameter(Instelling organisatie, OrganisatieParameterKey parameterKey)
	{
		return getOrganisatieParameter(organisatie, parameterKey, null);
	}

	@Override
	public <T> T getOrganisatieParameter(Instelling organisatie, OrganisatieParameterKey parameterKey, T defaultValue)
	{
		var key = maakParamKey(organisatie, parameterKey);
		var keyId = paramKeyIds.get(key);
		if (keyId == null)
		{
			LOG.debug("Id for key {} not in cache", key);
			return getParamAndCacheId(organisatie, parameterKey, defaultValue);
		}

		return getParamViaCache(organisatie, parameterKey, defaultValue);
	}

	private OrganisatieParameter getParameter(Instelling organisatie, OrganisatieParameterKey parameterKey)
	{
		Map<String, Object> queryParams = new HashMap<>();
		queryParams.put("key", parameterKey);

		if (organisatie == null)
		{
			List<Instelling> organisaties = instellingService.getInstellingByOrganisatieTypes(List.of(parameterKey.getOrganisatieType()));
			if (!organisaties.isEmpty())
			{
				organisatie = organisaties.get(0);
			}
		}
		if (organisatie != null)
		{
			queryParams.put("organisatie", organisatie.getId());
		}

		return hibernateService.getUniqueByParameters(OrganisatieParameter.class, queryParams);
	}

	private static <T> T getValueFromParam(OrganisatieParameterKey parameterKey, T defaultValue, OrganisatieParameter orgParam)
	{
		T value = null;
		if (orgParam != null && orgParam.getValue() != null)
		{
			String orgParamValue = orgParam.getValue();
			Class<?> valueType = parameterKey.getValueType();
			if (valueType.equals(Integer.class) && StringUtils.isNumeric(orgParamValue))
			{
				value = (T) Integer.valueOf(orgParamValue);
			}
			else if (valueType.equals(BigDecimal.class))
			{
				value = (T) BigDecimalUtil.stringToBigDecimal(orgParamValue, Constants.LOCALE_NL);
			}
			else if (valueType.equals(String.class))
			{
				value = (T) orgParamValue;
			}
			else if (valueType.equals(Boolean.class))
			{
				value = (T) Boolean.valueOf(orgParamValue);
			}
			else
			{
				throw new IllegalArgumentException("Type " + valueType + " not supported");
			}
		}
		if (value == null)
		{
			value = defaultValue;
		}
		return value;
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void saveOrUpdateOrganisatieParameters(List<OrganisatieParameter> parameters, InstellingGebruiker loggedInInstellingGebruiker)
	{
		Set<Instelling> instellingenToSave = new HashSet<>();
		Set<Bevolkingsonderzoek> bvos = new HashSet<>();
		List<String> nieuweValues = new ArrayList<>();
		for (OrganisatieParameter parameter : parameters)
		{
			String value = parameter.getValue();
			if (parameter.getId() == null)
			{
				value = nieuwValue(instellingenToSave, parameter, value);
			}
			else
			{
				value = updateBestaandeValue(parameter, value);
			}
			if (value != null)
			{
				nieuweValues.add("'" + parameter.getParameterNaam() + "' -> '" + value + "' voor '" + parameter.getOrganisatie().getNaam() + "'");
			}
			bvos.addAll(Arrays.asList(parameter.getKey().getBevolkingsonderzoeken()));

		}
		hibernateService.saveOrUpdateAll(parameters);
		hibernateService.saveOrUpdateAll(instellingenToSave);
		if (!nieuweValues.isEmpty())
		{
			logService.logGebeurtenis(LogGebeurtenis.PARAMETERISATIE_WIJZIG, loggedInInstellingGebruiker, "Nieuwe waarde(n): " + StringUtils.join(nieuweValues, ", "),
				bvos.toArray(new Bevolkingsonderzoek[bvos.size()]));
		}
	}

	private String updateBestaandeValue(OrganisatieParameter parameter, String value)
	{
		String diffFieldToLatestVersion = EntityAuditUtil.getDiffFieldsToLatestVersion(parameter, hibernateService.getHibernateSession(), "value");
		if (StringUtil.isBlank(diffFieldToLatestVersion))
		{
			value = null;
		}
		else if (StringUtils.isBlank(value))
		{
			value = GEEN_WAARDE;
		}
		return value;
	}

	private String nieuwValue(Set<Instelling> instellingenToSave, OrganisatieParameter parameter, String value)
	{
		parameter.getOrganisatie().getParameters().add(parameter);
		instellingenToSave.add(parameter.getOrganisatie());
		if (StringUtils.isBlank(value))
		{
			value = GEEN_WAARDE;
		}
		return value;
	}

	private String maakParamKey(Instelling organisatie, OrganisatieParameterKey parameterKey)
	{
		return (organisatie != null ? organisatie.getId() : "") + "_" + parameterKey;
	}

	private <T> T getParamAndCacheId(Instelling organisatie, OrganisatieParameterKey parameterKey, T defaultValue)
	{
		var parameter = getParameter(organisatie, parameterKey);
		if (parameter != null && parameter.getId() != null)
		{
			paramKeyIds.put(maakParamKey(organisatie, parameterKey), parameter.getId());
		}
		return getValueFromParam(parameterKey, defaultValue, parameter);
	}

	private <T> T getParamViaCache(Instelling organisatie, OrganisatieParameterKey parameterKey, T defaultValue)
	{
		String key = maakParamKey(organisatie, parameterKey);
		var keyId = paramKeyIds.get(key);
		LOG.debug("Use id {} voor Key {} uit cache", keyId, key);
		var parameter = hibernateService.get(OrganisatieParameter.class, keyId); 
		if (parameter == null)
		{
			LOG.debug("Cached Id {} for key {} not in database (anymore)", keyId, key);
			return getParamAndCacheId(organisatie, parameterKey, defaultValue);
		}
		return getValueFromParam(parameterKey, defaultValue, parameter);
	}
}
