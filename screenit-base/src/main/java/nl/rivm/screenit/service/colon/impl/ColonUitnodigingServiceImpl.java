package nl.rivm.screenit.service.colon.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.time.LocalDate;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.model.BriefDefinitie;
import nl.rivm.screenit.model.ProjectParameterKey;
import nl.rivm.screenit.model.TablePerClassHibernateObject_;
import nl.rivm.screenit.model.colon.ColonUitnodiging;
import nl.rivm.screenit.model.colon.UitnodigingCohort;
import nl.rivm.screenit.model.colon.UitnodigingCohortGeboortejaren;
import nl.rivm.screenit.model.colon.UitnodigingCohort_;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.repository.colon.ColonUitnodigingRepository;
import nl.rivm.screenit.repository.colon.UitnodigingCohortRepository;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.BaseUitnodigingService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.colon.ColonScreeningsrondeService;
import nl.rivm.screenit.service.colon.ColonUitnodigingService;
import nl.rivm.screenit.specification.algemeen.UitnodigingCohortSpecification;
import nl.rivm.screenit.specification.colon.ColonUitnodigingSpecification;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.ProjectUtil;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.hibernate.SessionFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
@Slf4j
public class ColonUitnodigingServiceImpl implements ColonUitnodigingService
{

	private static final int EERSTE_COHORT_JAAR_DK = 2013;

	@Autowired
	private BaseBriefService briefService;

	@Autowired
	private SimplePreferenceService simplePreferenceService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	@Lazy
	private ColonScreeningsrondeService screeningsrondeService;

	@Autowired
	@Lazy
	private BaseUitnodigingService baseUitnodigingService;

	@Autowired
	private SessionFactory sessionFactory;

	@Autowired
	private UitnodigingCohortRepository uitnodigingCohortRepository;

	@Autowired
	private ColonUitnodigingRepository uitnodigingRepository;

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

		return briefService.getNieuwsteBriefDefinitie(briefType);
	}

	@Override
	public LocalDate getGeprognotiseerdeIntakeDatum(boolean vooraankondigen)
	{
		var ifobtRetourPeriode = simplePreferenceService.getInteger(PreferenceKey.IFOBTRETOURPERIODE.name());
		var ifobtAnalysePeriode = simplePreferenceService.getInteger(PreferenceKey.IFOBTANALYSEPERIODE.name());
		var intakeAfspraakPeriode = simplePreferenceService.getInteger(PreferenceKey.INTAKEAFSPRAAKPERIODE.name());
		var vooraankondigingsPeriode = simplePreferenceService.getInteger(PreferenceKey.VOORAANKONDIGINSPERIODE.name());

		var geprognotiseerdeIntakeDatum = currentDateSupplier.getLocalDate().plusDays(ifobtRetourPeriode).plusDays(ifobtAnalysePeriode).plusDays(intakeAfspraakPeriode);

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
		var screeningRonde = uitnodiging.getScreeningRonde();
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
		for (var jaar = EERSTE_COHORT_JAAR_DK; jaar <= currentDateSupplier.getLocalDate().getYear(); jaar++)
		{
			var uitnodigingCohort = getUitnodigingCohort(jaar);
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
		var projectClient = ProjectUtil.getHuidigeProjectClient(uitnodiging.getScreeningRonde().getDossier().getClient(), currentDateSupplier.getDate());

		if (ProjectUtil.hasParameterSet(projectClient, ProjectParameterKey.COLON_WACHTTIJD_UITSLAG_STUDIETEST))
		{
			var gekoppeldeTest = uitnodiging.getGekoppeldeTest();
			var wachttijd = Integer.parseInt(ProjectUtil.getParameter(projectClient.getProject(), ProjectParameterKey.COLON_WACHTTIJD_UITSLAG_STUDIETEST));
			var analysedatum = DateUtil.toLocalDate(gekoppeldeTest.getAnalyseDatum());
			uitnodiging.setUitgesteldeUitslagDatum(DateUtil.toUtilDate(DateUtil.plusWerkdagen(analysedatum, wachttijd)));
		}
	}

	@Override
	public List<Long> getTeVersturenUitnodigingen()
	{
		var specification = ColonUitnodigingSpecification.heeftActieveClient()
			.and(ColonUitnodigingSpecification.heeftGeenVerstuurdDatum())
			.and(ColonUitnodigingSpecification.heeftUitnodigingsDatumVoorDatum(currentDateSupplier.getDate()))
			.and(ColonUitnodigingSpecification.heeftGeenBriefTypes(List.of(BriefType.COLON_UITNODIGING_INTAKE, BriefType.COLON_GUNSTIGE_UITSLAG)));

		return uitnodigingRepository.findWith(specification, Long.class, q -> q
			.projection((cb, r) -> r.get(TablePerClassHibernateObject_.id))
			.all());
	}

	@Override
	public void verwijderUitgesteldeUitslagDatum(ColonUitnodiging uitnodiging)
	{
		uitnodiging.setUitgesteldeUitslagDatum(null);
	}

	@Override
	public List<Integer> getUitnodigingCohorten()
	{
		var currentSession = sessionFactory.getCurrentSession();
		var cb = currentSession.getCriteriaBuilder();
		var q = cb.createQuery(Integer.class);
		var r = q.from(UitnodigingCohort.class);
		var specification = UitnodigingCohortSpecification.heeftJaarMinderOfGelijkAan(currentDateSupplier.getLocalDate().getYear());
		var query = q.select(r.get(UitnodigingCohort_.jaar)).where(specification.toPredicate(r, q, cb)).orderBy(cb.asc(r.get(UitnodigingCohort_.jaar)));
		return currentSession.createQuery(query).getResultList();
	}

	@Override
	public UitnodigingCohort getUitnodigingCohort(int jaar)
	{
		return uitnodigingCohortRepository.findOne(UitnodigingCohortSpecification.heeftJaar(jaar)).orElse(null);
	}
}
