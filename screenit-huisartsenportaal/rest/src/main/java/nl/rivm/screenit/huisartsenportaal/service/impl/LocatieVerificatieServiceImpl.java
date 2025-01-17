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

import java.security.SecureRandom;
import java.util.ArrayList;
import java.util.List;

import nl.rivm.screenit.huisartsenportaal.dto.VerificatieLocatieDto;
import nl.rivm.screenit.huisartsenportaal.dto.VerificatieStatusDto;
import nl.rivm.screenit.huisartsenportaal.enums.CervixLocatieStatus;
import nl.rivm.screenit.huisartsenportaal.model.Huisarts;
import nl.rivm.screenit.huisartsenportaal.model.Locatie;
import nl.rivm.screenit.huisartsenportaal.repository.LocatieRepository;
import nl.rivm.screenit.huisartsenportaal.service.LocatieVerificatieService;
import nl.rivm.screenit.huisartsenportaal.service.SynchronisatieService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS)
public class LocatieVerificatieServiceImpl implements LocatieVerificatieService
{
	@Autowired
	private LocatieRepository locatieRepository;

	@Autowired
	private SynchronisatieService synchronisatieService;

	@Override
	public List<VerificatieLocatieDto> getTeVerifierenLocaties(Huisarts huisarts)
	{
		List<Locatie> locaties = locatieRepository.findByStatusAndHuisarts(CervixLocatieStatus.KLANTNUMMER_NIET_GEVERIFIEERD, huisarts);

		List<VerificatieLocatieDto> teVerifierenLocaties = new ArrayList<>();

		for (Locatie locatie : locaties)
		{
			var locatieDto = new VerificatieLocatieDto();
			locatieDto.setLocatieNaam(locatie.getNaam());
			locatieDto.setZorgmailKlantnummer(locatie.getZorgmailklantnummer());
			locatieDto.setHuisartsportaalId(String.valueOf(locatie.getHuisartsportaalId()));

			teVerifierenLocaties.add(locatieDto);
		}

		return teVerifierenLocaties;
	}

	@Override
	public VerificatieStatusDto verifieerLocatie(VerificatieLocatieDto locatieDto)
	{
		Locatie locatie = locatieRepository.findByHuisartsportaalId(Long.valueOf(locatieDto.getHuisartsportaalId()));

		VerificatieStatusDto statusDto = new VerificatieStatusDto();
		if (locatie != null && locatie.getVerificatieCode().equals(locatieDto.getVerificatieCode()))
		{
			statusDto.setSucces(true);
			if (!CervixLocatieStatus.KLANTNUMMER_NIET_GEVERIFIEERD.equals(locatie.getStatus()))
			{
				statusDto.setReedsGeverifieerd(true);
			}
			else
			{
				locatie.setStatus(CervixLocatieStatus.ACTIEF);
				locatieRepository.save(locatie);
				synchronisatieService.syncLocatie(locatie.getHuisarts(), locatie, null);
			}
			statusDto.setLocatieNaam(locatie.getNaam());
			statusDto.setZorgmailKlantnummer(locatie.getZorgmailklantnummer());
		}
		else
		{
			statusDto.setSucces(false);
		}

		return statusDto;
	}

	@Override
	public Locatie setVerificatiePincode(Huisarts huisarts, Locatie teVerifierenLocatie)
	{
		SecureRandom random = new SecureRandom();
		int randomNumber = random.nextInt(10000);
		String pincode = String.format("%04d", randomNumber);
		teVerifierenLocatie.setVerificatieCode(pincode);
		for (Locatie locatie : huisarts.getLocaties())
		{
			if (!locatie.equals(teVerifierenLocatie) && locatie.getVerificatieCode() != null && teVerifierenLocatie.getVerificatieCode().equals(locatie.getVerificatieCode()))
			{
				return setVerificatiePincode(huisarts, teVerifierenLocatie);
			}
		}
		return teVerifierenLocatie;
	}
}
