package nl.rivm.screenit.main.service.mamma.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.dto.mamma.afspraken.IMammaAfspraakWijzigenFilter;
import nl.rivm.screenit.dto.mamma.afspraken.MammaStandplaatsPeriodeMetAfstandDto;
import nl.rivm.screenit.dto.mamma.planning.PlanningStandplaatsPeriodeDto;
import nl.rivm.screenit.main.dao.mamma.MammaStandplaatsPeriodeDao;
import nl.rivm.screenit.main.service.mamma.MammaStandplaatsPeriodeService;
import nl.rivm.screenit.model.Client;
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

import org.apache.commons.lang3.StringUtils;
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
		List<PlanningStandplaatsPeriodeDto> standplaatsPeriodes = new ArrayList<>(Arrays.asList(baseConceptPlanningsApplicatie.getStandplaatsPeriodesSorted(screeningsEenheid)));
		return standplaatsPeriodes;
	}

	@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
	@Override
	public List<MammaStandplaatsPeriode> getStandplaatsPeriodesVoorBulkVerzetten(ScreeningOrganisatie regio)
	{
		Date verzettenVanaf = DateUtil.plusWerkdagen(currentDateSupplier.getDateTimeMidnight(),
			simplePreferenceService.getInteger(PreferenceKey.MAMMA_AFSPRAAK_VERZETTEN_ZONDER_CLIENT_CONTACT_VANAF_AANTAL_WERKDAGEN.name())).toDate();

		return standplaatsPeriodeDao.getStandplaatsPeriodesVoorBulkVerzetten(regio, verzettenVanaf);
	}

	@Override
	public void splitsStandplaatsPeriode(PlanningStandplaatsPeriodeDto standplaatsPeriode, InstellingGebruiker ingelogdeInstellingGebruiker)
	{
		baseConceptPlanningsApplicatie.splitsStandplaatsPeriode(standplaatsPeriode, ingelogdeInstellingGebruiker);
	}

	@Override
	public void updateSortList(int index, PlanningStandplaatsPeriodeDto item, MammaScreeningsEenheid screeningsEenheid, InstellingGebruiker ingelogdeInstellingGebruiker)
	{
		item.screeningsEenheidVolgNr = index;
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
	public List<String> getStandplaatsPlaatsenVanActivePeriodes(IMammaAfspraakWijzigenFilter filter, boolean verzetten)
	{
		Set<String> plaatsen = new HashSet<>();
		List<MammaStandplaatsPeriodeMetAfstandDto> standplaatsPeriodeMetAfstandDtos = getStandplaatsPeriodeMetAfstandDtos(filter, verzetten);
		for (MammaStandplaatsPeriodeMetAfstandDto standplaatsPeriodeMetAfstandDto : standplaatsPeriodeMetAfstandDtos)
		{
			MammaStandplaatsPeriode standplaatsPeriode = hibernateService.load(MammaStandplaatsPeriode.class, standplaatsPeriodeMetAfstandDto.getStandplaatsPeriodeId());

			LocalDate vrijgegevenTotEnMetDatum = DateUtil.toLocalDate(standplaatsPeriode.getScreeningsEenheid().getVrijgegevenTotEnMet());
			if (vrijgegevenTotEnMetDatum != null || !verzetten)
			{
				MammaStandplaats standplaats = standplaatsPeriode.getStandplaatsRonde().getStandplaats();
				plaatsen.add(standplaats.getLocatie().getPlaats());
			}
		}
		List<String> plaatsenList = plaatsen.stream().filter(plaatsnaam -> StringUtils.isNotBlank(plaatsnaam)).collect(Collectors.toList());
		Collections.sort(plaatsenList);
		return plaatsenList;
	}

	@Override
	@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
	public List<MammaStandplaats> getStandplaatsenBuitenRegio(IMammaAfspraakWijzigenFilter filter, boolean verzetten)
	{
		List<MammaStandplaatsPeriodeMetAfstandDto> standplaatsPeriodeMetAfstandDtos = getStandplaatsPeriodeMetAfstandDtos(filter, verzetten);

		Set<MammaStandplaats> standplaatsenBuitenRegio = new HashSet<>();
		for (MammaStandplaatsPeriodeMetAfstandDto standplaatsPeriodeMetAfstandDto : standplaatsPeriodeMetAfstandDtos)
		{
			MammaStandplaatsPeriode standplaatsPeriode = hibernateService.load(MammaStandplaatsPeriode.class, standplaatsPeriodeMetAfstandDto.getStandplaatsPeriodeId());

			LocalDate vrijgegevenTotEnMetDatum = DateUtil.toLocalDate(standplaatsPeriode.getScreeningsEenheid().getVrijgegevenTotEnMet());
			if (vrijgegevenTotEnMetDatum != null || !verzetten)
			{
				standplaatsenBuitenRegio.add(standplaatsPeriode.getStandplaatsRonde().getStandplaats());
			}
		}
		return new ArrayList<>(standplaatsenBuitenRegio);
	}

	@Override
	@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
	public List<MammaScreeningsEenheid> getScreeningEenhedenBuitenRegio(IMammaAfspraakWijzigenFilter filter, boolean verzetten)
	{
		List<MammaStandplaatsPeriodeMetAfstandDto> standplaatsPeriodeMetAfstandDtos = getStandplaatsPeriodeMetAfstandDtos(filter, verzetten);

		Set<MammaScreeningsEenheid> standplaatsenBuitenRegio = new HashSet<>();
		for (MammaStandplaatsPeriodeMetAfstandDto standplaatsPeriodeMetAfstandDto : standplaatsPeriodeMetAfstandDtos)
		{
			MammaStandplaatsPeriode standplaatsPeriode = hibernateService.load(MammaStandplaatsPeriode.class, standplaatsPeriodeMetAfstandDto.getStandplaatsPeriodeId());

			LocalDate vrijgegevenTotEnMetDatum = DateUtil.toLocalDate(standplaatsPeriode.getScreeningsEenheid().getVrijgegevenTotEnMet());
			if (vrijgegevenTotEnMetDatum != null || !verzetten)
			{
				standplaatsenBuitenRegio.add(standplaatsPeriode.getScreeningsEenheid());
			}
		}
		return new ArrayList<>(standplaatsenBuitenRegio);
	}

	private List<MammaStandplaatsPeriodeMetAfstandDto> getStandplaatsPeriodeMetAfstandDtos(IMammaAfspraakWijzigenFilter filter, boolean verzetten)
	{
		Client client = filter.getClient();

		String savedPlaats = filter.getPlaats();
		filter.setPlaats(null);
		if (!verzetten)
		{
			filter.setTotEnMet(filter.getVanaf());
		}
		List<MammaStandplaatsPeriodeMetAfstandDto> standplaatsPeriodeMetAfstandDtos = baseStandplaatsService.getStandplaatsPeriodeMetAfstandDtos(client, filter);
		filter.setPlaats(savedPlaats);
		return standplaatsPeriodeMetAfstandDtos;
	}
}
