package nl.rivm.screenit.main.service.cervix.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.main.service.cervix.CervixDossierService;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.OrganisatieParameterKey;
import nl.rivm.screenit.model.dashboard.DashboardStatus;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.logging.LogRegel;
import nl.rivm.screenit.service.DashboardService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.OrganisatieParameterService;
import nl.rivm.screenit.service.cervix.CervixBaseMonsterService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import static nl.rivm.screenit.Constants.MAX_AANTAL_DAGEN_TERUGKIJKEN_CONTROLE_MISSENDE_UITSLAGEN;

@Service
@AllArgsConstructor
@Slf4j
public class CervixDossierServiceImpl implements CervixDossierService
{

	private final HibernateService hibernateService;

	private final ICurrentDateSupplier currentDateSupplier;

	private final DashboardService dashboardService;

	private final OrganisatieParameterService organisatieParameterService;

	private final CervixBaseMonsterService monsterService;

	private final LogService logService;

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public boolean setUitslagenGecontroleerdEnUpdateDashboard(LogRegel logRegel, InstellingGebruiker medewerker, DashboardStatus dashboardStatus)
	{
		var dossier = logRegel.getClient().getCervixDossier();
		var nu = currentDateSupplier.getLocalDate();
		var signalerenVanaf = nu.minusDays(MAX_AANTAL_DAGEN_TERUGKIJKEN_CONTROLE_MISSENDE_UITSLAGEN);
		var minimaleSignaleringsDatum = nu.minusDays(
			organisatieParameterService.getOrganisatieParameter(null, OrganisatieParameterKey.CERVIX_SIGNALERINGSTERMIJN_MISSENDE_UITSLAGEN, 30));
		var laatsteMonsterZonderUitslag = monsterService.getLaatsteMonsterMetMissendeUitslagVanDossier(dossier, signalerenVanaf, minimaleSignaleringsDatum).orElse(null);
		var status = dashboardService.updateLogRegelMetDashboardStatus(logRegel, medewerker.getMedewerker().getGebruikersnaam(), dashboardStatus);

		logService.logGebeurtenis(LogGebeurtenis.CERVIX_CONTROLE_MISSENDE_UITSLAGEN_MATCH_GECONTROLEERD, medewerker, dossier.getClient(), Bevolkingsonderzoek.CERVIX);

		if (laatsteMonsterZonderUitslag == null)
		{
			LOG.warn("Er zijn geen monsters zonder uitslag gevonden voor dossier {}", dossier.getId());
		}
		else
		{
			dossier.setDatumLaatstGecontroleerdeSignalering(laatsteMonsterZonderUitslag.getOntvangstdatum());
			hibernateService.saveOrUpdate(dossier);
		}
		return status;
	}
}
