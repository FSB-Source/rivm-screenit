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

import java.math.BigDecimal;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Optional;

import nl.rivm.screenit.dao.mamma.MammaBaseCapaciteitsBlokDao;
import nl.rivm.screenit.dto.mamma.afspraken.MammaCapaciteitBlokDto;
import nl.rivm.screenit.dto.mamma.planning.PlanningCapaciteitBlokDto;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.mamma.MammaCapaciteitBlok;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode;
import nl.rivm.screenit.model.mamma.enums.MammaCapaciteitBlokType;
import nl.rivm.screenit.service.mamma.MammaBaseAfspraakService;
import nl.rivm.screenit.service.mamma.MammaBaseCapaciteitsBlokService;
import nl.rivm.screenit.service.mamma.MammaBaseConceptPlanningsApplicatie;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.client.HttpServerErrorException;
import org.springframework.web.client.RestClientException;

@Service
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class MammaBaseCapaciteitsBlokServiceImpl implements MammaBaseCapaciteitsBlokService
{
	private static final Logger LOG = LoggerFactory.getLogger(MammaBaseCapaciteitsBlokServiceImpl.class);

	@Autowired
	private MammaBaseCapaciteitsBlokDao capaciteitsBlokDao;

	@Autowired(required = false)
	private MammaBaseConceptPlanningsApplicatie conceptPlanningsApplicatie;

	@Autowired
	private MammaBaseAfspraakService afspraakService;

	@Override
	public String saveOrUpdate(PlanningCapaciteitBlokDto blok, InstellingGebruiker ingelogdeGebruiker)
	{
		if (MammaCapaciteitBlokType.GEEN_SCREENING.equals(blok.blokType))
		{
			blok.aantalOnderzoeken = 0;
			blok.minderValideAfspraakMogelijk = false;
		}

		boolean isNieuw = blok.conceptId == null;
		try
		{
			conceptPlanningsApplicatie.sendCapaciteitBlok(blok, isNieuw, ingelogdeGebruiker);
		}
		catch (HttpClientErrorException | HttpServerErrorException se)
		{
			var responseBody = se.getResponseBodyAsString();
			LOG.error("Er ging iets mis bij het opslaan van de capaciteitsblok", se);
			return responseBody;
		}
		catch (RestClientException e)
		{
			return e.getMessage();
		}
		return null;
	}

	@Override
	public String delete(PlanningCapaciteitBlokDto blok, InstellingGebruiker loggedInInstellingGebruiker)
	{
		return conceptPlanningsApplicatie.deleteCapaciteitBlok(blok, loggedInInstellingGebruiker);
	}

	@Override
	public int getAantalAfsprakenOpBlok(PlanningCapaciteitBlokDto blokDto, boolean toDelete)
	{
		return conceptPlanningsApplicatie.getAantalAfsprakenOpBlok(blokDto, toDelete);
	}

	@Override
	public List<MammaCapaciteitBlok> getCapaciteitsBlokken(MammaScreeningsEenheid screeningsEenheid, Date start, Date end, boolean bepaalCapaciteit,
		Collection<MammaCapaciteitBlokType> blokTypes)
	{
		List<MammaCapaciteitBlok> capaciteitsBlokken = capaciteitsBlokDao.getCapaciteitsBlokken(screeningsEenheid, start, end, blokTypes);
		if (bepaalCapaciteit)
		{
			bepaalCapaciteit(capaciteitsBlokken, screeningsEenheid);
		}
		return capaciteitsBlokken;
	}

	private void bepaalCapaciteit(List<MammaCapaciteitBlok> capaciteitsBlokken, MammaScreeningsEenheid screeningsEenheid)
	{
		ScreeningOrganisatie screeningOrganisatie = (ScreeningOrganisatie) HibernateHelper.deproxy(screeningsEenheid.getBeoordelingsEenheid().getParent().getRegio());
		for (MammaCapaciteitBlok capaciteitBlok : capaciteitsBlokken)
		{
			BigDecimal aantalOnderzoeken = new BigDecimal(capaciteitBlok.getAantalOnderzoeken());
			capaciteitBlok.setBeschikbareCapaciteit(aantalOnderzoeken.multiply(capaciteitBlok.getBlokType().getFactorType().getFactor(screeningOrganisatie)));

			afspraakService.bepaalBenodigdeCapaciteit(capaciteitBlok.getAfspraken(), screeningsEenheid);
			BigDecimal benodigdeCapaciteit = afspraakService.getBenodigdeCapaciteit(capaciteitBlok.getAfspraken());
			capaciteitBlok.setVrijeCapaciteit(capaciteitBlok.getBeschikbareCapaciteit().subtract(benodigdeCapaciteit));
		}
	}

	@Override
	public Collection<MammaCapaciteitBlokDto> getNietGeblokkeerdeCapaciteitsBlokDtos(MammaStandplaatsPeriode standplaatsPeriode, Date vanaf, Date totEnMet,
		Collection<MammaCapaciteitBlokType> blokTypes)
	{
		return capaciteitsBlokDao.getNietGeblokkeerdeCapaciteitsBlokDtos(standplaatsPeriode, vanaf, totEnMet, blokTypes);
	}

	@Override
	public MammaCapaciteit getCapaciteit(Collection<MammaCapaciteitBlokDto> capaciteitBlokDtos)
	{
		MammaCapaciteit capaciteit = new MammaCapaciteit();
		HashMap<MammaCapaciteit.BlokTypeCapaciteit, HashMap<String, BigDecimal>> blokTypeVrijeCapaciteitPerDag = new HashMap<>();
		for (MammaCapaciteitBlokDto capaciteitBlokDto : capaciteitBlokDtos)
		{
			MammaCapaciteit.BlokTypeCapaciteit blokTypeCapaciteit = capaciteit.capaciteitMap.get(capaciteitBlokDto.blokType);
			blokTypeCapaciteit.beschikbareCapaciteit = blokTypeCapaciteit.beschikbareCapaciteit.add(capaciteitBlokDto.beschikbareCapaciteit);
			blokTypeCapaciteit.vrijeCapaciteit = blokTypeCapaciteit.vrijeCapaciteit.add(capaciteitBlokDto.vrijeCapaciteit);

			if (!blokTypeVrijeCapaciteitPerDag.containsKey(blokTypeCapaciteit))
			{
				blokTypeVrijeCapaciteitPerDag.put(blokTypeCapaciteit, new HashMap<>());
			}

			String standplaatsPeriodeDag = capaciteitBlokDto.vanaf.toLocalDate().format(DateUtil.LOCAL_DATE_FORMAT) + "-" + capaciteitBlokDto.standplaatsPeriode;
			BigDecimal vrijeCapaciteitStandplaatsPeriodeDag = BigDecimal.ZERO;

			HashMap<String, BigDecimal> vrijeCapaciteitPerDag = blokTypeVrijeCapaciteitPerDag.get(blokTypeCapaciteit);
			if (vrijeCapaciteitPerDag.containsKey(standplaatsPeriodeDag))
			{
				vrijeCapaciteitStandplaatsPeriodeDag = vrijeCapaciteitPerDag.get(standplaatsPeriodeDag);
			}
			vrijeCapaciteitStandplaatsPeriodeDag = vrijeCapaciteitStandplaatsPeriodeDag.add(capaciteitBlokDto.vrijeCapaciteit);
			vrijeCapaciteitPerDag.put(standplaatsPeriodeDag, vrijeCapaciteitStandplaatsPeriodeDag);
		}

		for (MammaCapaciteit.BlokTypeCapaciteit blokTypeCapaciteit : capaciteit.capaciteitMap.values())
		{
			if (blokTypeVrijeCapaciteitPerDag.containsKey(blokTypeCapaciteit))
			{
				for (BigDecimal vrijeDagCapaciteit : blokTypeVrijeCapaciteitPerDag.get(blokTypeCapaciteit).values())
				{
					if (vrijeDagCapaciteit.compareTo(BigDecimal.ZERO) < 0)
					{
						blokTypeCapaciteit.negatieveVrijeCapaciteit = blokTypeCapaciteit.negatieveVrijeCapaciteit.add(vrijeDagCapaciteit.multiply(BigDecimal.valueOf(-1)));
					}
				}
			}
			blokTypeCapaciteit.benutteCapaciteit = blokTypeCapaciteit.beschikbareCapaciteit.subtract(blokTypeCapaciteit.vrijeCapaciteit);
		}
		return capaciteit;
	}

	@Override
	public MammaCapaciteitBlok getCapaciteitsBlokOpTijdstipVoorSe(Client client, MammaScreeningsEenheid screeningsEenheid, Date nu)
	{

		List<MammaCapaciteitBlok> capaciteitBlokkenNu = getCapaciteitsBlokken(screeningsEenheid, nu, nu, false,
			Arrays.asList(MammaCapaciteitBlokType.TEHUIS, MammaCapaciteitBlokType.REGULIER));
		if (client.getMammaDossier().getTehuis() != null)
		{
			return getMammaCapaciteitBlokVoorType(capaciteitBlokkenNu, Collections.singletonList(MammaCapaciteitBlokType.TEHUIS));
		}
		else
		{
			return getMammaCapaciteitBlokVoorType(capaciteitBlokkenNu, Collections.singletonList(MammaCapaciteitBlokType.REGULIER));
		}
	}

	private MammaCapaciteitBlok getMammaCapaciteitBlokVoorType(List<MammaCapaciteitBlok> capaciteitBlokkenNu, List<MammaCapaciteitBlokType> types)
	{
		if (capaciteitBlokkenNu.size() >= 1)
		{
			Optional<MammaCapaciteitBlok> mogelijkCapaciteitsBlok = capaciteitBlokkenNu.stream().filter(mammaCapaciteitBlok -> types.contains(mammaCapaciteitBlok.getBlokType()))
				.findAny();
			if (mogelijkCapaciteitsBlok.isPresent())
			{
				return mogelijkCapaciteitsBlok.get();
			}
		}
		return null;
	}

}
