package nl.rivm.screenit.service.cervix.impl;

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
import java.util.Date;
import java.util.List;

import nl.rivm.screenit.dao.cervix.CervixVerrichtingDao;
import nl.rivm.screenit.dto.cervix.facturatie.CervixVerrichtingenZoekObject;
import nl.rivm.screenit.model.BMHKLaboratorium;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.SortState;
import nl.rivm.screenit.model.cervix.CervixHuisarts;
import nl.rivm.screenit.model.cervix.CervixHuisartsLocatie;
import nl.rivm.screenit.model.cervix.enums.CervixTariefType;
import nl.rivm.screenit.model.cervix.facturatie.CervixBetaalopdracht;
import nl.rivm.screenit.model.cervix.facturatie.CervixBetaalopdrachtRegel;
import nl.rivm.screenit.model.cervix.facturatie.CervixBetaalopdrachtRegelSpecificatie;
import nl.rivm.screenit.model.cervix.facturatie.CervixBoekRegel;
import nl.rivm.screenit.model.cervix.facturatie.CervixLabTarief;
import nl.rivm.screenit.model.cervix.facturatie.CervixTarief;
import nl.rivm.screenit.model.enums.BestandStatus;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.cervix.CervixVerrichtingService;
import nl.rivm.screenit.util.cervix.CervixTariefUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.hibernate.Hibernate;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS)
public class CervixVerrichtingServiceImpl implements CervixVerrichtingService
{

	private static final Logger LOG = LoggerFactory.getLogger(CervixVerrichtingServiceImpl.class);

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private CervixVerrichtingDao verrichtingDao;

	@Autowired
	private LogService logService;

	@Override
	public CervixTarief getTariefVoorDatum(CervixTariefType tariefType, Date verrichtingsDatum, BMHKLaboratorium laboratorium)
	{
		return verrichtingDao.getGeldigForDatumTarief(tariefType, verrichtingsDatum, laboratorium);
	}

	@Override
	public List<CervixBoekRegel> getLabVerrichtingen(CervixVerrichtingenZoekObject verrichtingenCriteria, ScreeningOrganisatie screeningOrganisatie,
		BMHKLaboratorium bmhkLaboratorium, SortState<String> sortState,
		long first,
		long count)
	{
		return verrichtingDao.getLabVerrichtingen(verrichtingenCriteria, screeningOrganisatie, bmhkLaboratorium, sortState, first, count);
	}

	@Override
	public long countLabVerrichtingen(CervixVerrichtingenZoekObject verrichtingenCriteria, ScreeningOrganisatie screeningOrganisatie, BMHKLaboratorium bmhkLaboratorium)
	{
		return verrichtingDao.countLabVerrichtingen(verrichtingenCriteria, screeningOrganisatie, bmhkLaboratorium);
	}

	@Override
	public BigDecimal getLaboratoriumTotaalBedrag(CervixVerrichtingenZoekObject verrichtingenCriteria, ScreeningOrganisatie screeningOrganisatie, BMHKLaboratorium bmhkLaboratorium,
		CervixTariefType tariefType)
	{
		verrichtingenCriteria.setDebet(false);
		BigDecimal totaalBedragCredit = verrichtingDao.getLaboratoriumTotaalBedrag(verrichtingenCriteria, screeningOrganisatie, bmhkLaboratorium, tariefType);
		if (totaalBedragCredit == null)
		{
			totaalBedragCredit = BigDecimal.ZERO;
		}
		verrichtingenCriteria.setDebet(true);
		BigDecimal totaalBedragDebet = verrichtingDao.getLaboratoriumTotaalBedrag(verrichtingenCriteria, screeningOrganisatie, bmhkLaboratorium, tariefType);
		if (totaalBedragDebet == null)
		{
			totaalBedragDebet = BigDecimal.ZERO;
		}
		verrichtingenCriteria.setDebet(null);
		return totaalBedragCredit.subtract(totaalBedragDebet);
	}

	@Override
	public BigDecimal getHuisartsTotaalBedrag(CervixVerrichtingenZoekObject verrichtingenCriteria, ScreeningOrganisatie screeningOrganisatie, CervixHuisarts huisarts,
		CervixHuisartsLocatie huisartsLocatie)
	{
		verrichtingenCriteria.setDebet(false);
		BigDecimal totaalBedragCredit = verrichtingDao.getHuisartsTotaalBedrag(verrichtingenCriteria, screeningOrganisatie, huisarts, huisartsLocatie);
		if (totaalBedragCredit == null)
		{
			totaalBedragCredit = BigDecimal.ZERO;
		}
		verrichtingenCriteria.setDebet(true);
		BigDecimal totaalBedragDebet = verrichtingDao.getHuisartsTotaalBedrag(verrichtingenCriteria, screeningOrganisatie, huisarts, huisartsLocatie);
		if (totaalBedragDebet == null)
		{
			totaalBedragDebet = BigDecimal.ZERO;
		}
		verrichtingenCriteria.setDebet(null);
		return totaalBedragCredit.subtract(totaalBedragDebet);
	}

	@Override
	public List<CervixBoekRegel> getHuisartsVerrichtingen(CervixVerrichtingenZoekObject verrichtingenCriteria, ScreeningOrganisatie screeningOrganisatie, CervixHuisarts huisarts,
		CervixHuisartsLocatie huisartsLocatie, SortState<String> sortState, long first, long count)
	{
		return verrichtingDao.getHuisartsVerrichtingen(verrichtingenCriteria, screeningOrganisatie, huisarts, huisartsLocatie, sortState, first, count);
	}

	@Override
	public long countHuisartsVerrichtingen(CervixVerrichtingenZoekObject verrichtingenCriteria, ScreeningOrganisatie screeningOrganisatie, CervixHuisarts huisarts,
		CervixHuisartsLocatie huisartsLocatie)
	{
		return verrichtingDao.countHuisartsVerrichtingen(verrichtingenCriteria, screeningOrganisatie, huisarts, huisartsLocatie);
	}

	@Override
	public CervixBetaalopdracht createBetaalOpdracht(ScreeningOrganisatie screeningOrganisatie, List<CervixBoekRegel> boekregels)
	{
		CervixBetaalopdracht opdracht = new CervixBetaalopdracht();
		opdracht.setScreeningOrganisatie(screeningOrganisatie);
		opdracht.setStatus(BestandStatus.NOG_TE_VERWERKEN);
		opdracht.setVanIban(screeningOrganisatie.getIban().toUpperCase());
		opdracht.setVanTenaamstelling(screeningOrganisatie.getIbanTenaamstelling());

		for (CervixBoekRegel boekregel : boekregels)
		{
			CervixBetaalopdrachtRegel regel = findOrCreateCervixBetaalOpdrachtRegel(opdracht, boekregel);
			CervixBetaalopdrachtRegelSpecificatie specificatie = findOrCreateCervixBetaalopdrachtRegelSpecificatie(regel, boekregel);
			updateSpecificatieMetVerrichting(specificatie, boekregel);
		}

		for (CervixBetaalopdrachtRegel bor : regelsNietPlusBedrag(opdracht))
		{
			opdracht.getBetaalopdrachtRegels().remove(bor);
		}
		return opdracht;
	}

	private List<CervixBetaalopdrachtRegel> regelsNietPlusBedrag(CervixBetaalopdracht opdracht)
	{
		List<CervixBetaalopdrachtRegel> verwijderRegels = new ArrayList<>();
		for (CervixBetaalopdrachtRegel bor : opdracht.getBetaalopdrachtRegels())
		{
			if (bor.getBedrag().compareTo(BigDecimal.ZERO) <= 0)
			{
				verwijderRegels.add(bor);
			}
		}
		return verwijderRegels;
	}

	private BigDecimal getTeBetalenBedrag(CervixBoekRegel boekRegel)
	{
		BigDecimal totaalBedrag = BigDecimal.ZERO;
		for (CervixBoekRegel boekregel : boekRegel.getVerrichting().getBoekRegels())
		{
			if (boekregel.getSpecificatie() == null)
			{
				BigDecimal tariefBedrag = CervixTariefUtil.getTariefBedrag(boekregel);
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
		BigDecimal tariefBedrag = CervixTariefUtil.getTariefBedrag(boekRegel);
		if (BigDecimal.ZERO.compareTo(specificatie.getTariefBedrag()) == 0)
		{
			specificatie.setTariefBedrag(tariefBedrag);
		}
		BigDecimal bedrag = getTeBetalenBedrag(boekRegel);
		specificatie.setBedrag(specificatie.getBedrag().add(bedrag));
		BigDecimal debets = specificatie.getDebets();
		for (CervixBoekRegel nogNietGekoppeldeBoekRegel : boekRegel.getVerrichting().getBoekRegels())
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

		CervixBetaalopdrachtRegel regel = specificatie.getBetaalopdrachtRegel();
		regel.setBedrag(regel.getBedrag().add(bedrag));
	}

	private CervixBetaalopdrachtRegelSpecificatie findOrCreateCervixBetaalopdrachtRegelSpecificatie(CervixBetaalopdrachtRegel regel, CervixBoekRegel boekregel)
	{
		CervixBetaalopdrachtRegelSpecificatie specificatie = getSpecificatieFromBetaalopdrachtRegel(regel, boekregel);
		if (specificatie == null)
		{
			specificatie = new CervixBetaalopdrachtRegelSpecificatie();
			specificatie.setTariefType(boekregel.getVerrichting().getType());
			specificatie.setTarief(boekregel.getTarief());
			specificatie.setBedrag(BigDecimal.ZERO);
			specificatie.setTariefBedrag(BigDecimal.ZERO);
			specificatie.setBetaalopdrachtRegel(regel);
			specificatie.setAantalBetaalRegels(Integer.valueOf(0));
			regel.getSpecificaties().add(specificatie);
		}
		return specificatie;
	}

	private CervixBetaalopdrachtRegelSpecificatie getSpecificatieFromBetaalopdrachtRegel(CervixBetaalopdrachtRegel regel, CervixBoekRegel boekRegel)
	{
		CervixTariefType type = boekRegel.getVerrichting().getType();
		CervixTarief tarief = boekRegel.getTarief();
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
		CervixBetaalopdrachtRegel regel = null;
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
		for (CervixBetaalopdrachtRegel opdrachtRegel : opdracht.getBetaalopdrachtRegels())
		{
			if (opdrachtRegel.getLaboratorium() != null && laboratorium != null && laboratorium.equals(opdrachtRegel.getLaboratorium())
				|| opdrachtRegel.getHuisartsLocatie() != null && locatie != null && locatie.equals(opdrachtRegel.getHuisartsLocatie()))
			{
				return opdrachtRegel;
			}
		}
		return null;
	}

	private BMHKLaboratorium getBMHKLabVanVerrichting(CervixBoekRegel boekregel)
	{
		return ((CervixLabTarief) Hibernate.unproxy(boekregel.getTarief())).getBmhkLaboratorium();
	}

	private CervixHuisartsLocatie getHuisartsLocatie(CervixBoekRegel boekregel)
	{
		return boekregel.getVerrichting().getHuisartsLocatie();
	}
}
