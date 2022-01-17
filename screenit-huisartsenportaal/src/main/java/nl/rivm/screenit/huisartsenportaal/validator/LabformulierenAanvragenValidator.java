package nl.rivm.screenit.huisartsenportaal.validator;

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

import java.util.Date;
import java.util.List;

import nl.rivm.screenit.huisartsenportaal.dto.AanvraagDto;
import nl.rivm.screenit.huisartsenportaal.dto.TableResultOptionsDto;
import nl.rivm.screenit.huisartsenportaal.enums.CervixLocatieStatus;
import nl.rivm.screenit.huisartsenportaal.model.Huisarts;
import nl.rivm.screenit.huisartsenportaal.model.LabformulierAanvraag;
import nl.rivm.screenit.huisartsenportaal.model.Locatie;
import nl.rivm.screenit.huisartsenportaal.repository.AanvraagCriteriaRepository;
import nl.rivm.screenit.huisartsenportaal.repository.LocatieRepository;

import org.joda.time.DateTime;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;

@Component
public class LabformulierenAanvragenValidator extends BaseValidator<AanvraagDto>
{

	private static final Logger LOG = LoggerFactory.getLogger(LabformulierenAanvragenValidator.class);

	@Autowired
	private LocatieRepository locatieRepository;

	@Autowired
	private AanvraagCriteriaRepository aanvraagCriteriaRepository;

	public LabformulierenAanvragenValidator()
	{
		setExcludeClasses(TableResultOptionsDto.class);
	}

	@Override
	public void validateTarget(AanvraagDto target, Errors errors)
	{
		Huisarts huisarts = getIngelogdeHuisarts();
		LOG.debug("aangekomen bij Validatie, huisarts = " + huisarts.getAgbcode());

		Locatie locatieVanDto = null;
		if (target.getLocatie() != null)
		{
			locatieVanDto = locatieRepository.findByHuisartsportaalId(target.getLocatie().getHuisartsportaalId());
		}

		if (locatieVanDto == null || !CervixLocatieStatus.ACTIEF.equals(locatieVanDto.getStatus()))
		{
			LOG.debug("error.geen.locatie");
			errors.reject("error.geen.locatie", "Voor het aanvragen dient u een locatie te selecteren.");
		}
		else
		{

			if (!locatieVanDto.getHuisarts().equals(huisarts))
			{
				LOG.debug("error.locatie.onbekend");
				errors.reject("error.locatie.onbekend", "Deze locatie hoort niet bij deze huisarts.");
			}

			Date beginDatumVandaag = new DateTime().withTimeAtStartOfDay().toDate();
			Date eindDatumVandaag = new DateTime().withTimeAtStartOfDay().plusDays(1).toDate();

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
			else if (target.getAantal() > 75)
			{
				LOG.debug("error.aantal.max");
				errors.reject("error.aantal.max", "De maximale waarde moet lager dan 75 zijn.");
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
			errors.getAllErrors().forEach(objectError -> {
				LOG.debug(objectError.getDefaultMessage());
			});
			LOG.debug("einde validatie");
		}
	}
}
