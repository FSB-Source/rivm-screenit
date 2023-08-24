package nl.rivm.screenit.main.service.mamma.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.dto.mamma.afspraken.IMammaAfspraakWijzigenFilter;
import nl.rivm.screenit.dto.mamma.afspraken.MammaStandplaatsPeriodeMetAfstandDto;
import nl.rivm.screenit.dto.mamma.planning.PlanningStandplaatsPeriodeDto;
import nl.rivm.screenit.main.dao.mamma.MammaStandplaatsPeriodeDao;
import nl.rivm.screenit.main.service.mamma.MammaStandplaatsPeriodeService;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.MammaStandplaats;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode;
import nl.rivm.screenit.model.mamma.MammaStandplaatsRonde;
import nl.rivm.screenit.model.verwerkingverslag.mamma.MammaStandplaatsRondeUitnodigenRapportage;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.mamma.MammaBaseConceptPlanningsApplicatie;
import nl.rivm.screenit.service.mamma.MammaBaseStandplaatsService;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.REQUIRED)
public class MammaStandplaatsPeriodeServiceImpl implements MammaStandplaatsPeriodeService
{
	@Autowired
	private MammaBaseConceptPlanningsApplicatie baseConceptPlanningsApplicatie;

	@Autowired
	private MammaStandplaatsPeriodeDao standplaatsPeriodeDao;

	@Autowired
	private MammaBaseStandplaatsService baseStandplaatsService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private SimplePreferenceService simplePreferenceService;

	@Autowired
	private HibernateService hibernateService;

	@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
	@Override
	public List<PlanningStandplaatsPeriodeDto> getStandplaatsPeriodesSorted(MammaScreeningsEenheid screeningsEenheid)
	{
		return new ArrayList<>(Arrays.asList(baseConceptPlanningsApplicatie.getStandplaatsPeriodesSorted(screeningsEenheid)));
	}

	@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
	@Override
	public List<MammaStandplaatsPeriode> getStandplaatsPeriodesVoorBulkVerzetten(ScreeningOrganisatie regio)
	{
		Date verzettenVanaf = DateUtil.plusWerkdagen(currentDateSupplier.getDateMidnight(),
			simplePreferenceService.getInteger(PreferenceKey.MAMMA_AFSPRAAK_VERZETTEN_ZONDER_CLIENT_CONTACT_VANAF_AANTAL_WERKDAGEN.name()));

		return standplaatsPeriodeDao.getStandplaatsPeriodesVoorBulkVerzetten(regio, verzettenVanaf);
	}

	@Override
	public void splitsStandplaatsPeriode(PlanningStandplaatsPeriodeDto standplaatsPeriode, InstellingGebruiker ingelogdeInstellingGebruiker)
	{
		baseConceptPlanningsApplicatie.splitsStandplaatsPeriode(standplaatsPeriode, ingelogdeInstellingGebruiker);
	}

	@Override
	public void updateSortList(int nieuwVolgnummer, PlanningStandplaatsPeriodeDto item, MammaScreeningsEenheid screeningsEenheid, InstellingGebruiker ingelogdeInstellingGebruiker)
	{
		item.screeningsEenheidVolgNr = nieuwVolgnummer;
		baseConceptPlanningsApplicatie.changeRoute(item, screeningsEenheid, ingelogdeInstellingGebruiker);
	}

	@Override
	public boolean saveOrUpdateStandplaatsPeriode(PlanningStandplaatsPeriodeDto standplaatsPeriode, InstellingGebruiker ingelogdeInstellingGebruiker)
	{
		baseConceptPlanningsApplicatie.sendAfspraakDrempelStandplaatsPeriode(standplaatsPeriode, ingelogdeInstellingGebruiker);
		return true; 
	}

	@Override
	@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
	public MammaStandplaatsRondeUitnodigenRapportage getStandplaatsRondeUitnodigenRapportage(MammaStandplaatsRonde standplaatsRonde)
	{
		return standplaatsPeriodeDao.getStandplaatsRondeUitnodigenRapportage(standplaatsRonde);
	}

	@Override
	@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
	public List<MammaStandplaats> getStandplaatsenBuitenRegio(IMammaAfspraakWijzigenFilter filter, boolean uitstellen)
	{
		List<MammaStandplaatsPeriodeMetAfstandDto> standplaatsPeriodeMetAfstandDtos = baseStandplaatsService.getStandplaatsPeriodeMetAfstandDtos(filter, uitstellen);

		Set<MammaStandplaats> standplaatsenBuitenRegio = new HashSet<>();
		for (MammaStandplaatsPeriodeMetAfstandDto standplaatsPeriodeMetAfstandDto : standplaatsPeriodeMetAfstandDtos)
		{
			MammaStandplaatsPeriode standplaatsPeriode = hibernateService.load(MammaStandplaatsPeriode.class, standplaatsPeriodeMetAfstandDto.getStandplaatsPeriodeId());

			LocalDate vrijgegevenTotEnMetDatum = DateUtil.toLocalDate(standplaatsPeriode.getScreeningsEenheid().getVrijgegevenTotEnMet());
			if (vrijgegevenTotEnMetDatum != null || uitstellen)
			{
				standplaatsenBuitenRegio.add(standplaatsPeriode.getStandplaatsRonde().getStandplaats());
			}
		}
		return new ArrayList<>(standplaatsenBuitenRegio);
	}

	@Override
	@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
	public List<MammaScreeningsEenheid> getScreeningEenhedenBuitenRegio(IMammaAfspraakWijzigenFilter filter, boolean uitstellen)
	{
		List<MammaStandplaatsPeriodeMetAfstandDto> standplaatsPeriodeMetAfstandDtos = baseStandplaatsService.getStandplaatsPeriodeMetAfstandDtos(filter, uitstellen);

		Set<MammaScreeningsEenheid> standplaatsenBuitenRegio = new HashSet<>();
		for (MammaStandplaatsPeriodeMetAfstandDto standplaatsPeriodeMetAfstandDto : standplaatsPeriodeMetAfstandDtos)
		{
			MammaStandplaatsPeriode standplaatsPeriode = hibernateService.load(MammaStandplaatsPeriode.class, standplaatsPeriodeMetAfstandDto.getStandplaatsPeriodeId());

			LocalDate vrijgegevenTotEnMetDatum = DateUtil.toLocalDate(standplaatsPeriode.getScreeningsEenheid().getVrijgegevenTotEnMet());
			if (vrijgegevenTotEnMetDatum != null || uitstellen)
			{
				standplaatsenBuitenRegio.add(standplaatsPeriode.getScreeningsEenheid());
			}
		}
		return new ArrayList<>(standplaatsenBuitenRegio);
	}
}
