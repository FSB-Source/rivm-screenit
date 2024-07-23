package nl.rivm.screenit.service.mamma.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.time.Duration;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import nl.rivm.screenit.dao.mamma.MammaBaseBeoordelingDao;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.enums.MammaBeLezerSoort;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.mamma.MammaBaseBeoordelingReserveringService;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
public class MammaBaseBeoordelingReserveringServiceImpl implements MammaBaseBeoordelingReserveringService
{
	private static final Logger LOG = LoggerFactory.getLogger(MammaBaseBeoordelingReserveringServiceImpl.class);

	private static final Duration MAX_RESERVERINGSTIJD =
		Duration.ofMinutes(30); 

	@Autowired
	private MammaBaseBeoordelingDao beoordelingDao;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Override
	@Transactional(propagation = Propagation.REQUIRES_NEW) 
	public List<Long> reserveerBeoordelingen(Long startBeoordelingId, List<Long> beoordelingenIds, InstellingGebruiker ingelogdeGebruiker, MammaBeLezerSoort lezerSoort)
	{
		List<Long> komendeGereserveerdeIds = new ArrayList<>();
		List<Long> overgeslagenIds = new ArrayList<>();
		int startIndex = Math.max(beoordelingenIds.indexOf(startBeoordelingId), 0); 
		for (int teReserverenIndex = startIndex; teReserverenIndex < beoordelingenIds.size()
			&& komendeGereserveerdeIds.size() < getMaximumAantalTeReserverenBeoordelingen(lezerSoort); teReserverenIndex++)
		{
			MammaBeoordeling beoordeling = hibernateService.get(MammaBeoordeling.class, beoordelingenIds.get(teReserverenIndex));
			hibernateService.reload(beoordeling); 
			if (!gereserveerdDoorIemandAnders(ingelogdeGebruiker, beoordeling) && statusToegestaan(beoordeling, lezerSoort, ingelogdeGebruiker))
			{
				beoordeling.setReserveringhouder(ingelogdeGebruiker);
				beoordeling.setReserveringsmoment(currentDateSupplier.getDate());
				komendeGereserveerdeIds.add(beoordeling.getId());
			}
			else
			{
				overgeslagenIds.add(beoordeling.getId());
			}
		}

		beoordelingenIds.removeAll(overgeslagenIds);
		LOG.info("Resultaat reserveren: komende gereserveerde beoordeling id's: {} Beoordeling id's overgeslagen: {}", komendeGereserveerdeIds, overgeslagenIds);
		return komendeGereserveerdeIds;
	}

	private int getMaximumAantalTeReserverenBeoordelingen(MammaBeLezerSoort lezerSoort)
	{

		return lezerSoort == MammaBeLezerSoort.EERSTE_OF_TWEEDE_LEZER ? 3 : 1;
	}

	private boolean statusToegestaan(MammaBeoordeling beoordeling, MammaBeLezerSoort lezerSoort, InstellingGebruiker ingelogdeGebruiker)
	{
		if (beoordeling.getStatus() == MammaBeoordelingStatus.VERSLAG_GEREED && !beoordeling.getVerslagLezing().getBeoordelaar().equals(ingelogdeGebruiker))
		{
			return false;
		}

		if (List.of(MammaBeoordelingStatus.TWEEDE_LEZING, MammaBeoordelingStatus.TWEEDE_LEZING_OPGESLAGEN).contains(beoordeling.getStatus())
			&& ingelogdeGebruiker.equals(beoordeling.getEersteLezing().getBeoordelaar()))
		{
			LOG.warn("1e lezer probeert beoordeling '{}' te openenen voor 2e lezing", beoordeling.getId());
			return false;
		}

		return getToegestaneStatussen(lezerSoort).contains(beoordeling.getStatus());
	}

	private List<MammaBeoordelingStatus> getToegestaneStatussen(MammaBeLezerSoort lezerSoort)
	{
		switch (lezerSoort)
		{
		case EERSTE_OF_TWEEDE_LEZER:
			return Arrays.asList(MammaBeoordelingStatus.EERSTE_LEZING, MammaBeoordelingStatus.TWEEDE_LEZING, MammaBeoordelingStatus.EERSTE_LEZING_OPGESLAGEN,
				MammaBeoordelingStatus.TWEEDE_LEZING_OPGESLAGEN);
		case DISCREPANTIE_LEZER:

			return Arrays.asList(MammaBeoordelingStatus.DISCREPANTIE, MammaBeoordelingStatus.ARBITRAGE, MammaBeoordelingStatus.VERSLAG_MAKEN,
				MammaBeoordelingStatus.UITSLAG_GUNSTIG);
		case ARBITRAGE_LEZER:

			return Arrays.asList(MammaBeoordelingStatus.ARBITRAGE, MammaBeoordelingStatus.VERSLAG_MAKEN, MammaBeoordelingStatus.UITSLAG_GUNSTIG);
		case VERSLAG_MAKER:
			return Arrays.asList(MammaBeoordelingStatus.VERSLAG_MAKEN, MammaBeoordelingStatus.VERSLAG_GEREED, MammaBeoordelingStatus.VERSLAG_AFGEKEURD);
		case FOTOBESPREKING_VISITATIE:
		case CONCLUSIE_REVIEW:
			return MammaBeoordelingStatus.eindStatussen();
		default:
			throw new IllegalArgumentException("Onbekende lezersoort: " + lezerSoort);
		}
	}

	@Override
	public boolean gereserveerdDoorIemandAnders(InstellingGebruiker ingelogdeGebruiker, MammaBeoordeling beoordeling)
	{
		return beoordeling.getReserveringhouder() != null && !ingelogdeGebruiker.equals(beoordeling.getReserveringhouder()) && !heeftVerlopenReservering(beoordeling);
	}

	private boolean heeftVerlopenReservering(MammaBeoordeling beoordeling)
	{
		if (beoordeling.getStatus() == MammaBeoordelingStatus.EERSTE_LEZING_OPGESLAGEN || beoordeling.getStatus() == MammaBeoordelingStatus.TWEEDE_LEZING_OPGESLAGEN)
		{
			return false; 
		}

		if (beoordeling.getReserveringsmoment() == null)
		{
			return true;
		}

		LocalDateTime reserveringstijd = DateUtil.toLocalDateTime(beoordeling.getReserveringsmoment());
		LocalDateTime reserveringVerlopenTijd = reserveringstijd.plus(MAX_RESERVERINGSTIJD).plusMinutes(1); 

		LocalDateTime nu = currentDateSupplier.getLocalDateTime();
		return nu.isAfter(reserveringVerlopenTijd);
	}

	@Override
	public boolean gereserveerdVoorGebruiker(long beoordelingId, InstellingGebruiker ingelogdeGebruiker, MammaBeLezerSoort lezerSoort)
	{
		if (MammaBeLezerSoort.CONCLUSIE_REVIEW.equals(lezerSoort))
		{
			return true; 
		}

		var beoordeling = hibernateService.get(MammaBeoordeling.class, beoordelingId); 
		hibernateService.reload(beoordeling); 
		return ingelogdeGebruiker.equals(beoordeling.getReserveringhouder()) && !heeftVerlopenReservering(beoordeling);
	}

	@Transactional
	@Override
	public void reserveringenVrijgeven(InstellingGebruiker ingelogdeGebruiker)
	{
		List<MammaBeoordeling> vrijTeGevenBeoordelingen = beoordelingDao.getVrijTeGevenBeoordelingen(ingelogdeGebruiker);
		vrijTeGevenBeoordelingen.forEach(this::geefBeoordelingVrij);
		LOG.info("Vrijgegeven beoordeling id's: {} ", vrijTeGevenBeoordelingen.stream().map(b -> b.getId().toString()).collect(Collectors.joining(",")));
	}

	@Transactional
	@Override
	public void geefBeoordelingVrij(MammaBeoordeling beoordeling)
	{
		beoordeling.setReserveringhouder(null);
		beoordeling.setReserveringsmoment(null);
		hibernateService.saveOrUpdate(beoordeling);
	}
}
