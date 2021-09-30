package nl.rivm.screenit.huisartsenportaal.validator;

/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
 * %%
 * Copyright (C) 2016 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.EnumSet;
import java.util.List;

import nl.rivm.screenit.huisartsenportaal.dto.AdresDto;
import nl.rivm.screenit.huisartsenportaal.dto.LocatieDto;
import nl.rivm.screenit.huisartsenportaal.enums.CervixLocatieStatus;
import nl.rivm.screenit.huisartsenportaal.model.Huisarts;
import nl.rivm.screenit.huisartsenportaal.model.Locatie;
import nl.rivm.screenit.huisartsenportaal.repository.LocatieCriteriaRepository;
import nl.rivm.screenit.huisartsenportaal.repository.LocatieRepository;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.validator.routines.IBANValidator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;

@Component
public class LocatieValidator extends BaseValidator<LocatieDto>
{
	@Autowired
	private LocatieRepository locatieRepository;

	@Autowired
	private LocatieCriteriaRepository locatieCriteriaRepository;

	private Logger LOG = LoggerFactory.getLogger(LocatieValidator.class);

	@Override
	public void validateTarget(LocatieDto dto, Errors errors)
	{
		if (!controleerLocatieOpNullVelden(dto))
		{
			errors.reject("error.locaties.valid", "Niet alle velden van de locatie zijn gevuld.");
		}

		Locatie locatieMetDezelfdeNaam = isErEenLocatieMetDezelfdeNaam(dto);
		if (locatieMetDezelfdeNaam != null)
		{
			if (locatieMetDezelfdeNaam.getStatus().equals(CervixLocatieStatus.ACTIEF)
				|| locatieMetDezelfdeNaam.getStatus().equals(CervixLocatieStatus.KLANTNUMMER_NIET_GEVERIFIEERD))
			{
				errors.reject("error.locatie.namen", "Er is al een actieve locatie met dezelfde naam.");
			}
			else
			{
				errors.reject("error.locatie.namen", "Er is al een verwijderde locatie met dezelfde naam.");
			}
		}
		if (checkOfDeLaatsteLocatieWordtVerwijderd(dto))
		{
			errors.reject("error.locatie.last", "U wilt de laatste locatie verwijderen. Dat kan alleen als er nog een locatie aanwezig is. Voeg eerst een nieuwe locatie toe.");
		}
		if (!isIbanValide(dto))
		{
			errors.reject("error.iban.invalide", "Het IBAN rekeningnummer is niet correct.");
		}
	}

	private boolean controleerLocatieOpNullVelden(LocatieDto locatieDto)
	{
		boolean isIban = locatieDto.getIban() != null;
		boolean isIbanTenaamstelling = locatieDto.getIbanTenaamstelling() != null;
		boolean isZorgmailklantnummer = locatieDto.getZorgmailklantnummer() != null;
		boolean isNaam = locatieDto.getNaam() != null;
		boolean isAdres = controleerAdresOpNullVelden(locatieDto.getLocatieAdres());
		return isIban && isIbanTenaamstelling && isZorgmailklantnummer && isNaam && isAdres;
	}

	private boolean controleerAdresOpNullVelden(AdresDto adresDto)
	{
		boolean isStraat = adresDto.getStraat() != null;
		boolean isHuisnummer = adresDto.getHuisnummer() != null;
		boolean isPostcode = adresDto.getPostcode() != null;
		boolean isPlaats = adresDto.getWoonplaats() != null;
		return isStraat && isHuisnummer && isPostcode && isPlaats;
	}

	private Locatie isErEenLocatieMetDezelfdeNaam(LocatieDto dto)
	{
		Huisarts huisarts = getIngelogdeHuisarts();
		List<Locatie> locaties = locatieRepository.findByHuisarts(huisarts);
		for (Locatie locatie : locaties)
		{
			if (StringUtils.lowerCase(locatie.getNaam()).equals(StringUtils.lowerCase(dto.getNaam())) && !locatie.getHuisartsportaalId().equals(dto.getHuisartsportaalId()))
			{
				return locatie;
			}
		}
		return null;
	}

	private boolean checkOfDeLaatsteLocatieWordtVerwijderd(LocatieDto dto)
	{
		if (CervixLocatieStatus.INACTIEF.equals(CervixLocatieStatus.valueOf(dto.getStatus())))
		{
			Huisarts huisarts = getIngelogdeHuisarts();
			List<Locatie> locaties = locatieCriteriaRepository.findByHuisartsAndStatussen(huisarts,
				EnumSet.of(CervixLocatieStatus.ACTIEF, CervixLocatieStatus.KLANTNUMMER_NIET_GEVERIFIEERD));
			return locaties.size() < 2;
		}
		return false;
	}

	private boolean isIbanValide(LocatieDto dto)
	{
		if (dto.getIban() != null)
		{
			return IBANValidator.getInstance().isValid(dto.getIban());
		}
		return false;
	}
}
