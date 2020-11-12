package nl.rivm.screenit.service.colon.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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
import java.util.HashSet;
import java.util.Set;
import java.util.stream.Collectors;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.dao.BaseBriefDao;
import nl.rivm.screenit.dao.colon.ColonUitnodigingsDao;
import nl.rivm.screenit.model.BriefDefinitie;
import nl.rivm.screenit.model.ProjectParameterKey;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.colon.ColonUitnodiging;
import nl.rivm.screenit.model.colon.IFOBTTest;
import nl.rivm.screenit.model.colon.UitnodigingCohort;
import nl.rivm.screenit.model.colon.UitnodigingCohortGeboortejaren;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.project.ProjectClient;
import nl.rivm.screenit.service.BaseUitnodigingService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.colon.ColonScreeningsrondeService;
import nl.rivm.screenit.service.colon.ColonUitnodigingService;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.ProjectUtil;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.joda.time.DateTime;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class ColonUitnodigingServiceImpl implements ColonUitnodigingService
{

	private static final int EERSTE_COHORT_JAAR_DK = 2013;

	private static final Logger LOG = LoggerFactory.getLogger(ColonUitnodigingServiceImpl.class);

	@Autowired
	private BaseBriefDao briefDao;

	@Autowired
	private SimplePreferenceService simplePreferenceService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private ColonScreeningsrondeService screeningsrondeService;

	@Autowired
	private BaseUitnodigingService baseUitnodigingService;

	@Autowired
	private ColonUitnodigingsDao uitnodigingsDao;

	@Override
	public BriefDefinitie getBriefType(ColonUitnodiging colonUitnodiging)
	{
		BriefType briefType = null;
		switch (colonUitnodiging.getColonUitnodigingCategorie())
		{
		case U1:
		case U2:
		case U4:
		case U4_2:
			briefType = BriefType.COLON_UITNODIGING;
			break;
		case U3:
		case U6:
			briefType = BriefType.COLON_UITNODIGING_WANT_GEEN_MONSTER;
			break;
		default:
			throw new IllegalStateException("Onbekende of lege uitnodigingscategorie: " + colonUitnodiging.getColonUitnodigingCategorie());
		}

		return briefDao.getNieuwsteBriefDefinitie(briefType);
	}

	@Override
	public DateTime getGeprognotiseerdeIntakeDatum(boolean vooraankondigen)
	{
		Integer ifobtRetourPeriode = simplePreferenceService.getInteger(PreferenceKey.IFOBTRETOURPERIODE.name());
		Integer ifobtAnalysePeriode = simplePreferenceService.getInteger(PreferenceKey.IFOBTANALYSEPERIODE.name());
		Integer intakeAfspraakPeriode = simplePreferenceService.getInteger(PreferenceKey.INTAKEAFSPRAAKPERIODE.name());
		Integer vooraankondigingsPeriode = simplePreferenceService.getInteger(PreferenceKey.VOORAANKONDIGINSPERIODE.name());

		DateTime geprognotiseerdeIntakeDatum = currentDateSupplier.getDateTime().plusDays(ifobtRetourPeriode).plusDays(ifobtAnalysePeriode).plusDays(intakeAfspraakPeriode);

		if (vooraankondigen)
		{
			geprognotiseerdeIntakeDatum = geprognotiseerdeIntakeDatum.plusDays(vooraankondigingsPeriode);
		}
		return geprognotiseerdeIntakeDatum;
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public ColonUitnodiging cloneUitnodiging(ColonUitnodiging uitnodiging, boolean checkAlleenUitslagGecommuniceerd)
	{
		ColonScreeningRonde screeningRonde = uitnodiging.getScreeningRonde();
		ColonUitnodiging nieuweUitnodiging = null;
		if (screeningsrondeService.heeftUitslag(uitnodiging, checkAlleenUitslagGecommuniceerd))
		{
			LOG.warn("Geen nieuwe uitnodiging aangemaakt, omdat er al een uitslag in de ronde aanwezig is. UID: " + uitnodiging.getUitnodigingsId());
		}
		else if (baseUitnodigingService.heeftAlEenNieuwereUitnodiging(uitnodiging))
		{
			LOG.warn("Geen nieuwe uitnodiging aangemaakt, omdat er al een nieuwere uitnodiging in de ronde aanwezig is. UID: " + uitnodiging.getUitnodigingsId());
		}
		else if (screeningsrondeService.isRondeStatusBuitenDoelgroep(uitnodiging.getScreeningRonde()))
		{
			LOG.warn("Geen nieuwe uitnodiging aangemaakt, omdat de client al (inmiddels) buiten de doelgroep is gevallen. UID: " + uitnodiging.getUitnodigingsId());
		}
		else
		{
			LOG.info("Maak een nieuwe uitnodiging nav uitnodiging met uitnodigingsId: " + uitnodiging.getUitnodigingsId());
			nieuweUitnodiging = screeningsrondeService.createNieuweUitnodiging(screeningRonde, uitnodiging.getColonUitnodigingCategorie());
		}
		return nieuweUitnodiging;
	}

	@Override
	public Set<Integer> getAlleGeboortejarenTotMetHuidigJaar()
	{
		Set<Integer> geboortejaren = new HashSet<>();
		for (int jaar = EERSTE_COHORT_JAAR_DK; jaar <= currentDateSupplier.getLocalDate().getYear(); jaar++)
		{
			UitnodigingCohort uitnodigingCohort = uitnodigingsDao.getUitnodigingCohort(jaar);
			if (uitnodigingCohort != null)
			{
				geboortejaren.addAll(uitnodigingCohort.getGeboortejaren().stream().map(UitnodigingCohortGeboortejaren::getGeboortejaren)
					.collect(Collectors.toList()));
			}
		}
		return geboortejaren;
	}

	@Override
	public void berekenEnSetUitgesteldeUitslagDatum(ColonUitnodiging uitnodiging)
	{
		ProjectClient projectClient = ProjectUtil.getHuidigeProjectClient(uitnodiging.getScreeningRonde().getDossier().getClient(), currentDateSupplier.getDate());

		if (ProjectUtil.hasParameterSet(projectClient, ProjectParameterKey.COLON_WACHTTIJD_UITSLAG_STUDIETEST))
		{
			IFOBTTest gekoppeldeTest = uitnodiging.getGekoppeldeTest();
			int wachttijd = Integer.parseInt(ProjectUtil.getParameter(projectClient.getProject(), ProjectParameterKey.COLON_WACHTTIJD_UITSLAG_STUDIETEST));
			LocalDate analysedatum = DateUtil.toLocalDate(gekoppeldeTest.getAnalyseDatum());
			uitnodiging.setUitgesteldeUitslagDatum(DateUtil.toUtilDate(DateUtil.plusWerkdagen(analysedatum, wachttijd)));
		}
	}

	@Override
	public void verwijderUitgesteldeUitslagDatum(ColonUitnodiging uitnodiging)
	{
		uitnodiging.setUitgesteldeUitslagDatum(null);
	}
}
