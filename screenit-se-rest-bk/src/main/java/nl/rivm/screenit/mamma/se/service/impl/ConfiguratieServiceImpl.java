package nl.rivm.screenit.mamma.se.service.impl;

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

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.mamma.se.dto.SeAutorisatieDto;
import nl.rivm.screenit.mamma.se.dto.SeConfiguratieKey;
import nl.rivm.screenit.mamma.se.service.ConfiguratieService;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.util.HashMap;
import java.util.Map;

@Service
@Transactional(propagation = Propagation.SUPPORTS)
public class ConfiguratieServiceImpl implements ConfiguratieService
{

	@Autowired
	private SimplePreferenceService preferenceService;

	@Override
	public void voegParametersToe(SeAutorisatieDto result)
	{
		Integer seMaxOfflineInlogPeriode = preferenceService.getInteger(PreferenceKey.MAMMA_SE_MAX_OFFLINE_INLOGPERIODE.name(), 0);

		result.setSeMaxOfflineInlogPeriode(seMaxOfflineInlogPeriode);

		Map<SeConfiguratieKey, String> seParameters = new HashMap<>();
		seParameters.put(SeConfiguratieKey.SE_MAX_OFFLINE_INLOG_PERIODE, Integer.toString(seMaxOfflineInlogPeriode));
		seParameters.put(SeConfiguratieKey.SE_DAGLIJST_OPHALEN_VOOR_DAGEN, Integer.toString(preferenceService.getInteger(PreferenceKey.MAMMA_SE_DAGLIJST_OPHALEN_DAGEN.name(), 1)));
		seParameters.put(SeConfiguratieKey.SE_INFORMATIE_OPHALEN_CRON, preferenceService.getString(PreferenceKey.INTERNAL_MAMMA_SE_INFORMATIE_OPHALEN_CRON.name(), "0 0 2 * * ?"));

		result.setSeParameters(seParameters);
	}

}
