package nl.rivm.screenit.huisartsenportaal.validator;

/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
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
import java.util.List;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.huisartsenportaal.dto.AanvraagDto;
import nl.rivm.screenit.huisartsenportaal.dto.AanvragenZoekObjectDto;
import nl.rivm.screenit.huisartsenportaal.enums.CervixLocatieStatus;
import nl.rivm.screenit.huisartsenportaal.model.LabformulierAanvraag;
import nl.rivm.screenit.huisartsenportaal.model.Locatie;
import nl.rivm.screenit.huisartsenportaal.repository.AanvraagCriteriaRepository;
import nl.rivm.screenit.huisartsenportaal.repository.LocatieRepository;
import nl.rivm.screenit.huisartsenportaal.util.CervixLocatieUtil;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;

@Slf4j
@Component
public class LabformulierenAanvragenValidator extends BaseValidator<AanvraagDto>
{

	@Autowired
	private LocatieRepository locatieRepository;

	@Autowired
	private AanvraagCriteriaRepository aanvraagCriteriaRepository;

	public LabformulierenAanvragenValidator()
	{
		setExcludeClasses(AanvragenZoekObjectDto.class);
	}

	@Override
	public void validateTarget(AanvraagDto target, Errors errors)
	{
		var huisarts = getIngelogdeHuisarts();
		LOG.debug("aangekomen bij Validatie, huisarts = " + huisarts.getAgbcode());

		Locatie locatieVanDto = null;
		if (target.getLocatie() != null)
		{
			locatieVanDto = locatieRepository.findByHuisartsportaalId(target.getLocatie().getHuisartsportaalId());
		}

		if (locatieVanDto == null)
		{
			LOG.debug("error.geen.locatie");
			errors.reject("error.geen.locatie", "Voor het aanvragen dient u een locatie te selecteren.");
		}
		else
		{
			if (!CervixLocatieStatus.ACTIEF.equals(locatieVanDto.getStatus()) || !CervixLocatieUtil.isLocatieCompleet(locatieVanDto))
			{
				LOG.debug("error.geen.locatie");
				errors.reject("error.geen.locatie", "De gekozen locatie is niet actief of is niet goed ingevuld.");
			}
			if (!locatieVanDto.getHuisarts().equals(huisarts))
			{
				LOG.debug("error.locatie.onbekend");
				errors.reject("error.locatie.onbekend", "Deze locatie hoort niet bij deze huisarts.");
			}

			var beginDatumVandaag = LocalDate.now();
			var eindDatumVandaag = LocalDate.now().plusDays(1);

			List<LabformulierAanvraag> aanvragenVandaag = aanvraagCriteriaRepository.findByLocatieAndAanvraagDatumBetween(locatieVanDto, beginDatumVandaag, eindDatumVandaag);
			if (aanvragenVandaag.size() >= 2)
			{
				LOG.debug("error.max.aanvragen");
				errors.reject("error.max.aanvragen", "U mag maximaal maar 2 keer per dag labformulieren aanvragen per locatie.");
			}

			if (target.getAantal() == null)
			{
				LOG.debug("error.aantal.null");
				errors.reject("error.aantal.null", "Voor het aanvragen dient u een geldig aantal in te voeren.");
			}
			else if (target.getAantal() > 25)
			{
				LOG.debug("error.aantal.max");
				errors.reject("error.aantal.max", "De maximale waarde moet lager dan 25 zijn.");
			}
			else if (target.getAantal() < 10)
			{
				LOG.debug("error.aantal.min");
				errors.reject("error.aantal.min", "De minimale waarde moet hoger dan 10 zijn.");
			}

			if (target.getLocatie() == null)
			{
				LOG.debug("error.locatie.null");
				errors.reject("error.locatie.null", "Kies een locatie voordat u een aanvraag mag indienen.");
			}
			errors.getAllErrors().forEach(objectError ->
			{
				LOG.debug(objectError.getDefaultMessage());
			});
			LOG.debug("einde validatie");
		}
	}
}
