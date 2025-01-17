package nl.rivm.screenit.service.mamma.impl;

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

import nl.rivm.screenit.dto.mamma.afspraken.IMammaAfspraakWijzigenFilter;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode_;
import nl.rivm.screenit.model.mamma.MammaStandplaatsRonde_;
import nl.rivm.screenit.model.mamma.MammaStandplaats_;
import nl.rivm.screenit.repository.mamma.MammaStandplaatsPeriodeRepository;
import nl.rivm.screenit.service.mamma.MammaBaseStandplaatsPeriodeService;
import nl.topicuszorg.organisatie.model.Adres_;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;

import com.google.common.collect.Range;

import static nl.rivm.screenit.specification.mamma.MammaStandplaatsPeriodeSpecification.filterScreeningsEenheden;
import static nl.rivm.screenit.specification.mamma.MammaStandplaatsPeriodeSpecification.heeftActieveScreeningsEenheid;
import static nl.rivm.screenit.specification.mamma.MammaStandplaatsPeriodeSpecification.heeftActieveStandplaats;
import static nl.rivm.screenit.specification.mamma.MammaStandplaatsPeriodeSpecification.heeftPlaats;
import static nl.rivm.screenit.specification.mamma.MammaStandplaatsPeriodeSpecification.heeftScreeningsEenheid;
import static nl.rivm.screenit.specification.mamma.MammaStandplaatsPeriodeSpecification.heeftStandplaats;
import static nl.rivm.screenit.specification.mamma.MammaStandplaatsPeriodeSpecification.isActiefOpDatum;
import static nl.rivm.screenit.specification.mamma.MammaStandplaatsPeriodeSpecification.isBeschikbaarVoorScreeningsOrganisatie;
import static nl.rivm.screenit.specification.mamma.MammaStandplaatsPeriodeSpecification.overlaptMetPeriode;

@Service
public class MammaBaseStandplaatsPeriodeServiceImpl implements MammaBaseStandplaatsPeriodeService
{
	@Autowired
	private MammaStandplaatsPeriodeRepository standplaatsPeriodeRepository;

	@Override
	public List<MammaStandplaatsPeriode> getStandplaatsPerioden(IMammaAfspraakWijzigenFilter filter)
	{
		return standplaatsPeriodeRepository.findAll(
			overlaptMetPeriode(Range.closed(filter.getVanaf(), filter.getTotEnMet()))
				.and(heeftActieveStandplaats())
				.and(heeftActieveScreeningsEenheid())
				.and(filterScreeningsEenheden(filter.getScreeningsEenheden()))
				.and(locatieSpecificatie(filter)),
			Sort.by(String.join(".", MammaStandplaatsPeriode_.STANDPLAATS_RONDE, MammaStandplaatsRonde_.STANDPLAATS, MammaStandplaats_.LOCATIE, Adres_.PLAATS))
		);
	}

	@Override
	public MammaStandplaatsPeriode getStandplaatsPeriodeOpDatum(MammaScreeningsEenheid screeningsEenheid, Date datum)
	{
		return standplaatsPeriodeRepository.findOne(heeftScreeningsEenheid(screeningsEenheid).and(isActiefOpDatum(datum))).orElse(null);
	}

	private Specification<MammaStandplaatsPeriode> locatieSpecificatie(IMammaAfspraakWijzigenFilter filter)
	{
		if (StringUtils.isNotBlank(filter.getPlaats()))
		{
			return heeftPlaats(filter.getPlaats());
		}
		else if (!filter.getStandplaatsen().isEmpty())
		{
			return heeftStandplaats(filter.getStandplaatsen());
		}
		else if (!filter.isBuitenRegio())
		{
			var screeningOrganisatieClient = filter.getClient().getPersoon().getGbaAdres().getGbaGemeente().getScreeningOrganisatie();
			return isBeschikbaarVoorScreeningsOrganisatie(screeningOrganisatieClient);
		}
		return null;
	}
}
