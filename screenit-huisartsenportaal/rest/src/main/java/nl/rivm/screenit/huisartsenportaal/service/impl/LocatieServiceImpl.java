package nl.rivm.screenit.huisartsenportaal.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
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

import java.util.ArrayList;
import java.util.List;

import nl.rivm.screenit.huisartsenportaal.dto.LocatieDto;
import nl.rivm.screenit.huisartsenportaal.dto.VerificatieLocatieDto;
import nl.rivm.screenit.huisartsenportaal.dto.locatie.LocatieResultDto;
import nl.rivm.screenit.huisartsenportaal.dto.locatie.LocatieSearchDto;
import nl.rivm.screenit.huisartsenportaal.enums.CervixLocatieStatus;
import nl.rivm.screenit.huisartsenportaal.model.Huisarts;
import nl.rivm.screenit.huisartsenportaal.model.Locatie;
import nl.rivm.screenit.huisartsenportaal.repository.LocatieCriteriaRepository;
import nl.rivm.screenit.huisartsenportaal.repository.LocatieRepository;
import nl.rivm.screenit.huisartsenportaal.service.AdresService;
import nl.rivm.screenit.huisartsenportaal.service.LabformulierService;
import nl.rivm.screenit.huisartsenportaal.service.LocatieService;
import nl.rivm.screenit.huisartsenportaal.service.LocatieVerificatieService;
import nl.rivm.screenit.huisartsenportaal.service.SynchronisatieService;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS)
public class LocatieServiceImpl implements LocatieService
{
	@Autowired
	private ModelMapper modelMapper;

	@Autowired
	private LocatieRepository locatieRepository;

	@Autowired
	private LocatieCriteriaRepository locatieCriteriaRepository;

	@Autowired
	private LabformulierService labformulierService;

	@Autowired
	private AdresService adresService;

	@Autowired
	private LocatieVerificatieService locatieVerificatieService;

	@Autowired
	private SynchronisatieService synchronisatieService;

	@Override
	public List<LocatieDto> getAllLocatiesFromHuisartsInDto(Huisarts huisarts)
	{
		var locatieDtos = new ArrayList<LocatieDto>();
		var locaties = locatieRepository.findByHuisarts(huisarts);
		if (!CollectionUtils.isEmpty(locaties))
		{
			locaties.forEach(locatie -> {
				locatieDtos.add(getLocatieDto(locatie));
			});
		}
		removeEmptyFromDto(locatieDtos);
		return locatieDtos;
	}

	private void removeEmptyFromDto(List<LocatieDto> locatieDtos)
	{
		for (LocatieDto dto : locatieDtos)
		{
			if (LocatieDto.EMPTY_VALUE.equalsIgnoreCase(dto.getIban()))
			{
				dto.setIban("");
			}
			if (LocatieDto.EMPTY_VALUE.equalsIgnoreCase(dto.getIbanTenaamstelling()))
			{
				dto.setIbanTenaamstelling("");
			}
			if (LocatieDto.EMPTY_VALUE.equalsIgnoreCase(dto.getNaam()))
			{
				dto.setNaam("");
			}
		}
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public Locatie updateAndGetLocatie(Huisarts huisarts, LocatieDto locatieDto)
	{
		Locatie locatie = getLocatie(locatieDto);
		if (locatie == null)
		{
			locatie = new Locatie();
		}
		if (locatie.getScreenitId() == null)
		{
			locatie.setScreenitId(locatieDto.getScreenitId());
		}
		locatie.setNaam(locatieDto.getNaam());
		locatie.setIban(locatieDto.getIban());
		locatie.setIbanTenaamstelling(locatieDto.getIbanTenaamstelling());

		if (!CervixLocatieStatus.valueOf(locatieDto.getStatus()).equals(CervixLocatieStatus.INACTIEF)
			&& !StringUtils.equals(locatieDto.getZorgmailklantnummer(), locatie.getZorgmailklantnummer()))
		{
			locatie.setStatus(CervixLocatieStatus.KLANTNUMMER_NIET_GEVERIFIEERD);
			locatie = locatieVerificatieService.setVerificatiePincode(huisarts, locatie);
		}

		else if (!CervixLocatieStatus.INACTIEF.equals(CervixLocatieStatus.valueOf(locatieDto.getStatus())) && CervixLocatieStatus.INACTIEF.equals(locatie.getStatus()))
		{
			if (Boolean.TRUE.equals(locatie.getMoetVerifierenVoorActivatie()))
			{
				locatie = locatieVerificatieService.setVerificatiePincode(huisarts, locatie);
				locatie.setStatus(CervixLocatieStatus.KLANTNUMMER_NIET_GEVERIFIEERD);
				locatie.setMoetVerifierenVoorActivatie(null);
				locatieDto.setHerzendVerificatieMail(true);
			}
			else
			{
				locatie.setStatus(CervixLocatieStatus.ACTIEF);
			}
		}

		else if (CervixLocatieStatus.INACTIEF.equals(CervixLocatieStatus.valueOf(locatieDto.getStatus())))
		{
			if (CervixLocatieStatus.KLANTNUMMER_NIET_GEVERIFIEERD.equals(locatie.getStatus()))
			{
				locatie.setMoetVerifierenVoorActivatie(true);
			}
			locatie.setStatus(CervixLocatieStatus.INACTIEF);
		}

		if (locatieDto.getHerzendVerificatieMail() == null && CervixLocatieStatus.KLANTNUMMER_NIET_GEVERIFIEERD.equals(locatie.getStatus())
			&& locatie.getHuisartsportaalId() != null && StringUtils.equals(locatieDto.getZorgmailklantnummer(), locatie.getZorgmailklantnummer()))
		{
			locatieDto.setHerzendVerificatieMail(false);
		}

		locatie.setZorgmailklantnummer(locatieDto.getZorgmailklantnummer());
		locatie.setHuisarts(huisarts);

		if (locatieDto.getLocatieAdres() != null)
		{
			locatie.setLocatieAdres(adresService.updateAndGetAdres(locatieDto.getLocatieAdres()));
		}
		locatieRepository.save(locatie);

		return locatie;
	}

	@Override
	public Locatie getLocatie(LocatieDto locatieDto)
	{
		Locatie locatie = null;
		if (locatieDto.getHuisartsportaalId() != null)
		{
			locatie = locatieRepository.findByHuisartsportaalId(locatieDto.getHuisartsportaalId());
		}
		if (locatie == null && locatieDto.getScreenitId() != null)
		{
			locatie = locatieRepository.findByScreenitId(locatieDto.getScreenitId());
		}
		return locatie;
	}

	@Override
	public LocatieDto getLocatieDto(Locatie locatie)
	{
		LocatieDto dto = new LocatieDto();
		modelMapper.map(locatie, dto);
		return dto;
	}

	@Override
	public LocatieResultDto getLocatieResultDto(Huisarts huisarts, LocatieSearchDto locatieSearchDto)
	{
		LocatieResultDto resultDto = new LocatieResultDto();
		List<Locatie> locaties = locatieCriteriaRepository.getLocaties(huisarts, locatieSearchDto);
		resultDto.setAantalLocaties(locatieCriteriaRepository.countLocaties(huisarts, locatieSearchDto));
		for (Locatie locatie : locaties)
		{
			resultDto.getLocaties().add(getLocatieDto(locatie));
		}
		return resultDto;
	}

	@Override
	public boolean isLocatieIdVanHuisarts(Huisarts huisarts, long locatieId)
	{
		boolean isLocatieIdVanHuisarts = false;
		for (Locatie locatie : huisarts.getLocaties())
		{
			if (locatieId == locatie.getHuisartsportaalId())
			{
				isLocatieIdVanHuisarts = true;
			}
		}
		return isLocatieIdVanHuisarts;
	}

	@Override
	public void herzendVerificatieMail(Huisarts huisarts, VerificatieLocatieDto locatieDto)
	{
		Locatie locatie = locatieRepository.findByHuisartsportaalId(Long.valueOf(locatieDto.getHuisartsportaalId()));

		if (locatie.getVerificatieCode().length() > 4)
		{
			locatie = locatieVerificatieService.setVerificatiePincode(huisarts, locatie);
		}
		locatieRepository.save(locatie);
		synchronisatieService.herzendVerificatieMail(huisarts, locatie);
	}

	@Override
	public void nietVerstuurdeLabformulierenVerwijderen(LocatieDto locatieDto)
	{
		Locatie locatie = getLocatie(locatieDto);
		if (locatie != null && locatie.getStatus().equals(CervixLocatieStatus.INACTIEF))
		{
			labformulierService.verwijderNogNietVerstuurdeLabformulierenVanLocatie(locatie);
		}
	}
}
