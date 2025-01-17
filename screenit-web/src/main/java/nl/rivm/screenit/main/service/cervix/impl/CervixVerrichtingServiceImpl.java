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

import java.math.BigDecimal;
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.dto.cervix.facturatie.CervixVerrichtingenZoekObject;
import nl.rivm.screenit.main.service.cervix.CervixVerrichtingService;
import nl.rivm.screenit.model.BMHKLaboratorium;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.cervix.CervixHuisarts;
import nl.rivm.screenit.model.cervix.CervixHuisartsLocatie;
import nl.rivm.screenit.model.cervix.enums.CervixTariefType;
import nl.rivm.screenit.model.cervix.facturatie.CervixBetaalopdracht;
import nl.rivm.screenit.model.cervix.facturatie.CervixBetaalopdrachtRegel;
import nl.rivm.screenit.model.cervix.facturatie.CervixBetaalopdrachtRegelSpecificatie;
import nl.rivm.screenit.model.cervix.facturatie.CervixBoekRegel;
import nl.rivm.screenit.model.cervix.facturatie.CervixBoekRegel_;
import nl.rivm.screenit.model.cervix.facturatie.CervixHuisartsTarief;
import nl.rivm.screenit.model.cervix.facturatie.CervixHuisartsTarief_;
import nl.rivm.screenit.model.cervix.facturatie.CervixLabTarief;
import nl.rivm.screenit.model.enums.BestandStatus;
import nl.rivm.screenit.repository.cervix.CervixBoekRegelRepository;
import nl.rivm.screenit.repository.cervix.CervixHuisartsTariefRepository;
import nl.rivm.screenit.repository.cervix.CervixLabTariefRepository;
import nl.rivm.screenit.util.cervix.CervixTariefUtil;

import org.hibernate.Hibernate;
import org.hibernate.SessionFactory;
import org.jetbrains.annotations.NotNull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;

import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.cervix.CervixTariefSpecification.heeftGeldigVanafdatumVoorHa;
import static nl.rivm.screenit.specification.cervix.CervixTariefSpecification.heeftGeldigVanafdatumVoorLab;
import static nl.rivm.screenit.specification.cervix.CervixTariefSpecification.heeftLabVoorTarief;
import static nl.rivm.screenit.specification.cervix.CervixTariefSpecification.isHuisartsTariefActief;
import static nl.rivm.screenit.specification.cervix.CervixTariefSpecification.isLabTariefActief;
import static nl.rivm.screenit.specification.cervix.CervixVerrichtingSpecification.filterVerrichtingType;

@Service
@Slf4j
public class CervixVerrichtingServiceImpl implements CervixVerrichtingService
{
	@Autowired
	private SessionFactory sessionFactory;

	@Autowired
	@Qualifier("cervixLabBoekregelsDataProviderService")
	private CervixLabBoekregelsDataProviderServiceImpl labBoekregelsDataProviderService;

	@Autowired
	@Qualifier("cervixHuisartsBoekregelsDataProviderService")
	private CervixHuisartsBoekregelsDataProviderServiceImpl huisartsBoekregelsDataProviderService;

	@Autowired
	@Qualifier("cervixLaboratoriumTarievenDataProviderService")
	private CervixLaboratoriumTarievenDataProviderServiceImpl laboratoriumTarievenDataProviderService;

	@Autowired
	@Qualifier("cervixHuisartsTarievenDataProviderService")
	private CervixHuisartsTarievenDataProviderServiceImpl huisartsTarievenDataProviderService;

	@Autowired
	private CervixLabTariefRepository labTariefRepository;

	@Autowired
	private CervixHuisartsTariefRepository haTariefRepository;

	@Autowired
	private CervixBoekRegelRepository boekRegelRepository;

	@Override
	public List<CervixBoekRegel> getLabVerrichtingen(CervixVerrichtingenZoekObject verrichtingenZoekObject, ScreeningOrganisatie screeningOrganisatie,
		BMHKLaboratorium laboratorium, Sort sort, long first, long count)
	{
		var completedZoekObject = completeZoekObject(verrichtingenZoekObject, screeningOrganisatie, null, null, laboratorium);
		return labBoekregelsDataProviderService.findPage(first, count, completedZoekObject, sort);
	}

	@Override
	public long countLabVerrichtingen(CervixVerrichtingenZoekObject verrichtingenZoekObject, ScreeningOrganisatie screeningOrganisatie, BMHKLaboratorium laboratorium)
	{
		var completedZoekObject = completeZoekObject(verrichtingenZoekObject, screeningOrganisatie, null, null, laboratorium);
		return labBoekregelsDataProviderService.size(completedZoekObject);
	}

	@Override
	public BigDecimal getLaboratoriumTotaalBedrag(CervixVerrichtingenZoekObject verrichtingenZoekObject, ScreeningOrganisatie screeningOrganisatie,
		BMHKLaboratorium laboratorium,
		CervixTariefType tariefType)
	{
		return getTotaalBedrag(verrichtingenZoekObject, screeningOrganisatie, null, null, laboratorium, tariefType);
	}

	@Override
	public BigDecimal getHuisartsTotaalBedrag(CervixVerrichtingenZoekObject verrichtingenZoekObject, ScreeningOrganisatie screeningOrganisatie, CervixHuisarts huisarts,
		CervixHuisartsLocatie huisartsLocatie)
	{
		return getTotaalBedrag(verrichtingenZoekObject, screeningOrganisatie, huisarts, huisartsLocatie, null, CervixTariefType.HUISARTS_UITSTRIJKJE);
	}

	@Override
	public List<CervixBoekRegel> getHuisartsVerrichtingen(CervixVerrichtingenZoekObject verrichtingenZoekObject, ScreeningOrganisatie screeningOrganisatie, CervixHuisarts huisarts,
		CervixHuisartsLocatie huisartsLocatie, Sort sort, long first, long count)
	{
		var completedCriteria = completeZoekObject(verrichtingenZoekObject, screeningOrganisatie, huisarts, huisartsLocatie, null);
		return huisartsBoekregelsDataProviderService.findPage(first, count, completedCriteria, sort);
	}

	@Override
	public long countHuisartsVerrichtingen(CervixVerrichtingenZoekObject verrichtingenZoekObject, ScreeningOrganisatie screeningOrganisatie, CervixHuisarts huisarts,
		CervixHuisartsLocatie huisartsLocatie)
	{
		var completedCriteria = completeZoekObject(verrichtingenZoekObject, screeningOrganisatie, huisarts, huisartsLocatie, null);
		return huisartsBoekregelsDataProviderService.size(completedCriteria);
	}

	@Override
	public CervixBetaalopdracht createBetaalOpdracht(ScreeningOrganisatie screeningOrganisatie, List<CervixBoekRegel> boekregels)
	{
		var opdracht = new CervixBetaalopdracht();
		opdracht.setScreeningOrganisatie(screeningOrganisatie);
		opdracht.setStatus(BestandStatus.NOG_TE_VERWERKEN);
		opdracht.setVanIban(screeningOrganisatie.getIban().toUpperCase());
		opdracht.setVanTenaamstelling(screeningOrganisatie.getIbanTenaamstelling());

		for (CervixBoekRegel boekregel : boekregels)
		{
			var regel = findOrCreateCervixBetaalOpdrachtRegel(opdracht, boekregel);
			var specificatie = findOrCreateCervixBetaalopdrachtRegelSpecificatie(regel, boekregel);
			updateSpecificatieMetVerrichting(specificatie, boekregel);
		}

		for (var bor : regelsNietPlusBedrag(opdracht))
		{
			opdracht.getBetaalopdrachtRegels().remove(bor);
		}
		return opdracht;
	}

	@Override
	public List<CervixLabTarief> getLabTarieven(BMHKLaboratorium laboratorium, long first, long count, Sort sort)
	{
		return laboratoriumTarievenDataProviderService.findPage(first, count, laboratorium, sort);
	}

	@Override
	public Long countLabTarieven(BMHKLaboratorium laboratorium)
	{
		return laboratoriumTarievenDataProviderService.size(laboratorium);
	}

	@Override
	public boolean heeftLaboratoriumTarief(CervixLabTarief tarief)
	{
		return labTariefRepository.exists(isLabTariefActief()
			.and(heeftLabVoorTarief(tarief.getBmhkLaboratorium()))
			.and(heeftGeldigVanafdatumVoorLab(tarief.getGeldigVanafDatum())));
	}

	@Override
	public boolean heeftHuisartsTarief(Date datum)
	{
		return haTariefRepository.exists(isHuisartsTariefActief()
			.and(heeftGeldigVanafdatumVoorHa(datum)));
	}

	@Override
	public List<CervixHuisartsTarief> getHuisartsTarieven(long first, long count, Sort sort)
	{
		return huisartsTarievenDataProviderService.findPage(first, count, null, sort);
	}

	@Override
	public Long countHuisartsTarieven()
	{
		return huisartsTarievenDataProviderService.size(null);
	}

	@Override
	public CervixLabTarief getLatestLabTarief(BMHKLaboratorium lab)
	{
		return labTariefRepository.findByGeldigTotenmetDatumIsNullAndActiefIsTrueAndBmhkLaboratorium(lab).orElse(null);
	}

	@Override
	public CervixHuisartsTarief getLatestHuisartsTarief()
	{
		return haTariefRepository.findByGeldigTotenmetDatumIsNullAndActiefIsTrue().orElse(null);
	}

	private List<CervixBetaalopdrachtRegel> regelsNietPlusBedrag(CervixBetaalopdracht opdracht)
	{
		return opdracht.getBetaalopdrachtRegels().stream().filter(regel -> regel.getBedrag().compareTo(BigDecimal.ZERO) <= 0).collect(Collectors.toList());
	}

	private BigDecimal getTeBetalenBedrag(CervixBoekRegel boekRegel)
	{
		var totaalBedrag = BigDecimal.ZERO;
		for (var boekregel : boekRegel.getVerrichting().getBoekRegels())
		{
			if (boekregel.getSpecificatie() == null)
			{
				var tariefBedrag = CervixTariefUtil.getTariefBedrag(boekregel);
				if (boekregel.getDebet())
				{
					totaalBedrag = totaalBedrag.subtract(tariefBedrag);
				}
				else
				{
					totaalBedrag = totaalBedrag.add(tariefBedrag);
				}
			}
		}
		return totaalBedrag;
	}

	private void updateSpecificatieMetVerrichting(CervixBetaalopdrachtRegelSpecificatie specificatie, CervixBoekRegel boekRegel)
	{
		var tariefBedrag = CervixTariefUtil.getTariefBedrag(boekRegel);
		if (BigDecimal.ZERO.compareTo(specificatie.getTariefBedrag()) == 0)
		{
			specificatie.setTariefBedrag(tariefBedrag);
		}
		var bedrag = getTeBetalenBedrag(boekRegel);
		specificatie.setBedrag(specificatie.getBedrag().add(bedrag));
		var debets = specificatie.getDebets();
		for (var nogNietGekoppeldeBoekRegel : boekRegel.getVerrichting().getBoekRegels())
		{
			if (nogNietGekoppeldeBoekRegel.getSpecificatie() == null)
			{
				if (nogNietGekoppeldeBoekRegel.getDebet())
				{
					debets = debets.add(CervixTariefUtil.getTariefBedrag(nogNietGekoppeldeBoekRegel));
				}
				specificatie.getBoekRegels().add(nogNietGekoppeldeBoekRegel);
			}
		}
		specificatie.setDebets(debets);
		specificatie.setAantalBetaalRegels(specificatie.getAantalBetaalRegels() + 1);

		var regel = specificatie.getBetaalopdrachtRegel();
		regel.setBedrag(regel.getBedrag().add(bedrag));
	}

	private CervixBetaalopdrachtRegelSpecificatie findOrCreateCervixBetaalopdrachtRegelSpecificatie(CervixBetaalopdrachtRegel regel, CervixBoekRegel boekregel)
	{
		var specificatie = getSpecificatieFromBetaalopdrachtRegel(regel, boekregel);
		if (specificatie == null)
		{
			specificatie = new CervixBetaalopdrachtRegelSpecificatie();
			specificatie.setTariefType(boekregel.getVerrichting().getType());
			specificatie.setTarief(boekregel.getTarief());
			specificatie.setBedrag(BigDecimal.ZERO);
			specificatie.setTariefBedrag(BigDecimal.ZERO);
			specificatie.setBetaalopdrachtRegel(regel);
			specificatie.setAantalBetaalRegels(0);
			regel.getSpecificaties().add(specificatie);
		}
		return specificatie;
	}

	private CervixBetaalopdrachtRegelSpecificatie getSpecificatieFromBetaalopdrachtRegel(CervixBetaalopdrachtRegel regel, CervixBoekRegel boekRegel)
	{
		var type = boekRegel.getVerrichting().getType();
		var tarief = boekRegel.getTarief();
		for (CervixBetaalopdrachtRegelSpecificatie spec : regel.getSpecificaties())
		{
			if (tarief.equals(spec.getTarief()) && type.equals(spec.getTariefType()))
			{
				return spec;
			}
		}
		return null;
	}

	private CervixBetaalopdrachtRegel findOrCreateCervixBetaalOpdrachtRegel(CervixBetaalopdracht opdracht, CervixBoekRegel boekregel)
	{
		CervixHuisartsLocatie locatie = null;
		BMHKLaboratorium lab = null;
		CervixBetaalopdrachtRegel regel;
		if (CervixTariefUtil.hoortVerrichtingBijHuisarts(boekregel))
		{
			locatie = getHuisartsLocatie(boekregel);
			regel = getCervixBetaalOpdrachtRegelFrom(opdracht, locatie, null);
		}
		else
		{
			lab = getBMHKLabVanVerrichting(boekregel);
			regel = getCervixBetaalOpdrachtRegelFrom(opdracht, null, lab);
		}
		if (regel == null)
		{
			regel = new CervixBetaalopdrachtRegel();
			if (locatie != null)
			{
				regel.setHuisartsLocatie(locatie);
				regel.setNaarIban(locatie.getIban().toUpperCase());
				regel.setNaarTenaamstelling(locatie.getIbanTenaamstelling());
			}
			if (lab != null)
			{
				regel.setLaboratorium(lab);
				regel.setNaarIban(lab.getIban().toUpperCase());
				regel.setNaarTenaamstelling(lab.getIbanTenaamstelling());
			}
			opdracht.getBetaalopdrachtRegels().add(regel);
			regel.setBetaalopdracht(opdracht);
			regel.setBedrag(BigDecimal.ZERO);
		}
		return regel;
	}

	private CervixBetaalopdrachtRegel getCervixBetaalOpdrachtRegelFrom(CervixBetaalopdracht opdracht, CervixHuisartsLocatie locatie, BMHKLaboratorium laboratorium)
	{
		return opdracht.getBetaalopdrachtRegels().stream().
			filter(regel -> regel.getLaboratorium() != null && laboratorium != null && laboratorium.equals(regel.getLaboratorium())
				|| regel.getHuisartsLocatie() != null && locatie != null && locatie.equals(regel.getHuisartsLocatie()))
			.findFirst()
			.orElse(null);
	}

	private BMHKLaboratorium getBMHKLabVanVerrichting(CervixBoekRegel boekregel)
	{
		return ((CervixLabTarief) Hibernate.unproxy(boekregel.getTarief())).getBmhkLaboratorium();
	}

	private CervixHuisartsLocatie getHuisartsLocatie(CervixBoekRegel boekregel)
	{
		return boekregel.getVerrichting().getHuisartsLocatie();
	}

	private @NotNull BigDecimal getTotaalBedrag(CervixVerrichtingenZoekObject verrichtingenZoekObject, ScreeningOrganisatie screeningOrganisatie, CervixHuisarts huisarts,
		CervixHuisartsLocatie huisartsLocatie, BMHKLaboratorium laboratorium,
		CervixTariefType tariefType)
	{
		var completedZoekObject = completeZoekObject(verrichtingenZoekObject, screeningOrganisatie, huisarts, huisartsLocatie, laboratorium);
		var totaalBedragCredit = getTotaalBedrag(completedZoekObject, tariefType, false);
		var totaalBedragDebet = getTotaalBedrag(completedZoekObject, tariefType, true);
		return totaalBedragCredit.subtract(totaalBedragDebet);
	}

	private @NotNull BigDecimal getTotaalBedrag(CervixVerrichtingenZoekObject verrichtingenZoekObject, CervixTariefType tariefType, boolean debet)
	{
		verrichtingenZoekObject.setDebet(debet);
		if (tariefType == CervixTariefType.HUISARTS_UITSTRIJKJE)
		{
			return boekRegelRepository.findWith(huisartsBoekregelsDataProviderService.getSpecification(verrichtingenZoekObject, Sort.unsorted())
				.and(filterVerrichtingType(tariefType)), BigDecimal.class, q -> q.projection((cb, r) ->
			{
				var huisartsTarief = cb.treat(join(r, CervixBoekRegel_.tarief), CervixHuisartsTarief.class);
				return cb.sum(huisartsTarief.get(CervixHuisartsTarief_.tarief));
			})).one().orElse(BigDecimal.ZERO);
		}
		else
		{
			return boekRegelRepository.findWith(labBoekregelsDataProviderService.getSpecification(verrichtingenZoekObject, Sort.unsorted())
				.and(filterVerrichtingType(tariefType)), BigDecimal.class, q -> q.projection((cb, r) ->
			{
				var labTarief = cb.treat(join(r, CervixBoekRegel_.tarief), CervixLabTarief.class);
				return cb.sum(labTarief.get(tariefType.getBedragProperty()));
			})).one().orElse(BigDecimal.ZERO);
		}
	}

	private static CervixVerrichtingenZoekObject completeZoekObject(CervixVerrichtingenZoekObject verrichtingenZoekObject, ScreeningOrganisatie screeningOrganisatie,
		CervixHuisarts huisarts, CervixHuisartsLocatie huisartsLocatie, BMHKLaboratorium laboratorium)
	{
		CervixVerrichtingenZoekObject clonedZoekObject;
		try
		{
			clonedZoekObject = verrichtingenZoekObject.clone();
			clonedZoekObject.setScreeningOrganisatie(screeningOrganisatie);
			clonedZoekObject.setHuisarts(huisarts);
			clonedZoekObject.setHuisartsLocatie(huisartsLocatie);
			clonedZoekObject.setBmhkLaboratorium(laboratorium);
		}
		catch (CloneNotSupportedException e)
		{
			throw new RuntimeException(e);
		}
		return clonedZoekObject;
	}
}
