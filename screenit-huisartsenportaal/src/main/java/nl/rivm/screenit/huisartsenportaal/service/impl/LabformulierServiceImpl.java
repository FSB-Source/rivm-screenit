package nl.rivm.screenit.huisartsenportaal.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
 * %%
 * Copyright (C) 2016 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.util.Date;
import java.util.List;

import nl.rivm.screenit.huisartsenportaal.dto.AanvraagDto;
import nl.rivm.screenit.huisartsenportaal.dto.AanvraagStatistiekenDto;
import nl.rivm.screenit.huisartsenportaal.dto.AanvraagTotalenDto;
import nl.rivm.screenit.huisartsenportaal.dto.TableResultOptionsDto;
import nl.rivm.screenit.huisartsenportaal.model.Huisarts;
import nl.rivm.screenit.huisartsenportaal.model.LabformulierAanvraag;
import nl.rivm.screenit.huisartsenportaal.model.Locatie;
import nl.rivm.screenit.huisartsenportaal.model.enums.AanvraagStatus;
import nl.rivm.screenit.huisartsenportaal.repository.AanvraagCriteriaRepository;
import nl.rivm.screenit.huisartsenportaal.repository.AanvraagRepository;
import nl.rivm.screenit.huisartsenportaal.repository.LocatieRepository;
import nl.rivm.screenit.huisartsenportaal.service.LabformulierService;
import nl.rivm.screenit.huisartsenportaal.service.SynchronisatieService;

import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jms.IllegalStateException;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS)
public class LabformulierServiceImpl implements LabformulierService
{
	@Autowired
	private AanvraagRepository aanvragenRepository;

	@Autowired
	private AanvraagCriteriaRepository aanvragenCriteriaRepository;

	@Autowired
	private LocatieRepository locatieRepository;

	@Autowired
	private SynchronisatieService synchronisatieService;

	@Autowired
	private ModelMapper modelMapper;

	@Override
	public AanvraagTotalenDto getAanvragenHuisarts(Huisarts huisarts, TableResultOptionsDto tableResultOptionsDto)
	{
		List<LabformulierAanvraag> aanvragen = aanvragenCriteriaRepository.findByHuisarts(huisarts, tableResultOptionsDto);

		List<AanvraagDto> dtos = new ArrayList<>();
		for (LabformulierAanvraag aanvraag : aanvragen)
		{
			dtos.add(convertToDto(aanvraag));
		}
		AanvraagTotalenDto aanvraagTotalenDto = new AanvraagTotalenDto();
		aanvraagTotalenDto.setAanvragen(dtos);
		aanvraagTotalenDto.setAantalAanvragen(aanvragenCriteriaRepository.countAanvragen(huisarts));
		return aanvraagTotalenDto;
	}

	@Override
	public LabformulierAanvraag saveAanvraag(Huisarts huisarts, AanvraagDto aanvraagDto) throws IllegalStateException
	{
		Locatie locatie = null;
		if (aanvraagDto.getLocatie() != null && aanvraagDto.getLocatie().getHuisartsportaalId() != null)
		{
			locatie = locatieRepository.findByHuisartsportaalId(aanvraagDto.getLocatie().getHuisartsportaalId());
		}
		if (locatie == null && aanvraagDto.getLocatie() != null && aanvraagDto.getLocatie().getScreenitId() != null)
		{
			locatie = locatieRepository.findByScreenitId(aanvraagDto.getLocatie().getScreenitId());
		}

		LabformulierAanvraag aanvraag = new LabformulierAanvraag();
		aanvraag.setHuisarts(huisarts);
		aanvraag.setLocatie(locatie);
		aanvraag.setAantal(aanvraagDto.getAantal());
		aanvraag.setAanvraagDatum(new Date());
		aanvraag.setAangevraagdDoor(huisarts.getAchternaam());
		aanvraag.setStatus(AanvraagStatus.AANGEVRAAGD);
		aanvraag.setStatusDatum(new Date());
		aanvragenRepository.save(aanvraag);

		return aanvraag;
	}

	@Override
	public LabformulierAanvraag saveScreenITAanvraag(AanvraagDto aanvraagDto)
	{

		LabformulierAanvraag aanvraag = null;
		if (aanvraagDto.getHuisartsportaalId() != null)
		{
			aanvraag = aanvragenRepository.findByHuisartsportaalId(aanvraagDto.getHuisartsportaalId());
		}
		if (aanvraag == null && aanvraagDto.getScreenitId() != null)
		{
			aanvraag = aanvragenRepository.findByScreenitId(aanvraagDto.getScreenitId());
		}
		if (aanvraag == null)
		{
			aanvraag = new LabformulierAanvraag();
		}
		if(aanvraag.getScreenitId() == null)
		{
			aanvraag.setScreenitId(aanvraagDto.getScreenitId());
		}
		aanvraag.setAantal(aanvraagDto.getAantal());
		aanvraag.setAangevraagdDoor(aanvraagDto.getAangevraagdDoor());
		aanvraag.setAanvraagDatum(aanvraagDto.getAanvraagDatum());
		aanvraag.setStatusDatum(aanvraagDto.getStatusDatum());
		aanvraag.setStatus(AanvraagStatus.getAanvraagStatusByName(aanvraagDto.getStatus()));
		Locatie locatie = null;
		if (aanvraagDto.getLocatie().getHuisartsportaalId() != null)
		{
			locatie = locatieRepository.findByHuisartsportaalId(aanvraagDto.getLocatie().getHuisartsportaalId());
		}
		if (locatie == null && aanvraagDto.getLocatie().getScreenitId() != null)
		{
			locatie = locatieRepository.findByScreenitId(aanvraagDto.getLocatie().getScreenitId());
		}
		aanvraag.setLocatie(locatie);
		aanvraag.setHuisarts(locatie.getHuisarts());
		aanvragenRepository.save(aanvraag);

		return aanvraag;
	}

	@Override
	public AanvraagStatistiekenDto getAanvraagStatistiekenLocatie(Long locatiePortaalId)
	{
		Integer nogTeVersturen = 0;
		Integer verstuurd = 0;
		Locatie locatie = locatieRepository.findByHuisartsportaalId(locatiePortaalId);
		List<LabformulierAanvraag> aanvragen = aanvragenCriteriaRepository.findByLocatie(locatie);
		for (LabformulierAanvraag aanvraag : aanvragen)
		{
			Integer aantalFormulieren = aanvraag.getAantal();
			if (AanvraagStatus.AFGEDRUKT_EN_VERSTUURD == aanvraag.getStatus())
			{
				verstuurd += aantalFormulieren;
			}
			else
			{
				nogTeVersturen += aantalFormulieren;
			}
		}

		AanvraagStatistiekenDto aanvraagStatistiekenDto = new AanvraagStatistiekenDto();
		aanvraagStatistiekenDto.setAantalNogTeVersturen(nogTeVersturen);
		aanvraagStatistiekenDto.setAantalVerstuurd(verstuurd);
		aanvraagStatistiekenDto.setAantalTotaal(aanvragen.size());
		return aanvraagStatistiekenDto;
	}

	@Override
	public AanvraagDto convertToDto(LabformulierAanvraag aanvraag)
	{
		AanvraagDto aanvraagDto = new AanvraagDto();
		modelMapper.map(aanvraag, aanvraagDto);

		return aanvraagDto;
	}

	@Override
	public LabformulierAanvraag convertToEntity(AanvraagDto aanvraagDto)
	{

		LabformulierAanvraag aanvraag = new LabformulierAanvraag();
		if (aanvraagDto.getScreenitId() == null)
		{
			aanvragenRepository.findByScreenitId(aanvraagDto.getScreenitId());
		}
		modelMapper.map(aanvraagDto, aanvraag);
		return aanvraag;
	}

	public void setModelMapper(ModelMapper modelMapper)
	{
		this.modelMapper = modelMapper;
	}

	@Override
	public void verwijderNogNietVerstuurdeLabformulierenVanLocatie(Locatie locatie)
	{
		List<LabformulierAanvraag> nogNietVerstuurdeLabformulieren = aanvragenRepository.findByLocatieAndStatus(locatie, AanvraagStatus.AANGEVRAAGD);
		nogNietVerstuurdeLabformulieren.forEach(aanvraag -> {
			aanvraag.setStatus(AanvraagStatus.VERWIJDERD);
			aanvraag.setStatusDatum(new Date());
			aanvragenRepository.save(aanvraag);
			synchronisatieService.syncAanvraag(aanvraag);
		});
	}
}
