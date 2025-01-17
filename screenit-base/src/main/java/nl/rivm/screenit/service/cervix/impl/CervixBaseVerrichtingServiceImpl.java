package nl.rivm.screenit.service.cervix.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.util.Date;
import java.util.List;

import lombok.RequiredArgsConstructor;

import nl.rivm.screenit.model.BMHKLaboratorium;
import nl.rivm.screenit.model.cervix.enums.CervixTariefType;
import nl.rivm.screenit.model.cervix.facturatie.CervixTarief;
import nl.rivm.screenit.model.cervix.facturatie.CervixVerrichting;
import nl.rivm.screenit.repository.cervix.CervixHuisartsTariefRepository;
import nl.rivm.screenit.repository.cervix.CervixLabTariefRepository;
import nl.rivm.screenit.repository.cervix.CervixVerrichtingRepository;
import nl.rivm.screenit.service.cervix.CervixBaseVerrichtingService;
import nl.rivm.screenit.util.DateUtil;

import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import static nl.rivm.screenit.specification.cervix.CervixTariefSpecification.heeftLabVoorTarief;
import static nl.rivm.screenit.specification.cervix.CervixTariefSpecification.isGeldigHaTariefVoor;
import static nl.rivm.screenit.specification.cervix.CervixTariefSpecification.isGeldigLabTariefVoor;
import static nl.rivm.screenit.specification.cervix.CervixTariefSpecification.isHuisartsTariefActief;
import static nl.rivm.screenit.specification.cervix.CervixTariefSpecification.isLabTariefActief;
import static nl.rivm.screenit.specification.cervix.CervixTariefSpecification.isTariefVoorVerrichting;
import static nl.rivm.screenit.specification.cervix.CervixVerrichtingSpecification.filterVerrichtingTypeVoorVerrichting;
import static nl.rivm.screenit.specification.cervix.CervixVerrichtingSpecification.verrichtingsdatumValtTussenVoorVerrichting;

@Service
@RequiredArgsConstructor
public class CervixBaseVerrichtingServiceImpl implements CervixBaseVerrichtingService
{
	private final CervixLabTariefRepository labTariefRepository;

	private final CervixHuisartsTariefRepository huisartsTariefRepository;

	private final CervixVerrichtingRepository verrichtingRepository;

	@Override
	public CervixTarief getTariefVoorDatum(Date verrichtingsdatum, BMHKLaboratorium laboratorium)
	{
		var peildatum = DateUtil.toLocalDate(verrichtingsdatum);
		if (laboratorium == null)
		{
			return huisartsTariefRepository.findOne(isHuisartsTariefActief()
				.and(isGeldigHaTariefVoor(peildatum))).orElse(null);
		}
		return labTariefRepository.findOne(isLabTariefActief()
			.and(isGeldigLabTariefVoor(peildatum))
			.and(heeftLabVoorTarief(laboratorium))).orElse(null);
	}

	@Override
	public List<CervixVerrichting> getVerrichtingenVoorTarief(Long oudTariefId, CervixTarief nieuweTarief, CervixTariefType tariefType)
	{
		var pageable = Pageable.ofSize(Integer.getInteger("BMHK_VERRICHTINGEN_HERINDEXEREN_CHUNK_SIZE", 500));
		return verrichtingRepository.findAll(filterVerrichtingTypeVoorVerrichting(tariefType)
				.and(isTariefVoorVerrichting(oudTariefId))
				.and(verrichtingsdatumValtTussenVoorVerrichting(DateUtil.toLocalDate(nieuweTarief.getGeldigVanafDatum()), DateUtil.toLocalDate(nieuweTarief.getGeldigTotenmetDatum()))),
			pageable).toList();
	}

}
