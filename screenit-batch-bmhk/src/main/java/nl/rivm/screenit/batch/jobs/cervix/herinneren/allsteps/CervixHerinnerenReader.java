package nl.rivm.screenit.batch.jobs.cervix.herinneren.allsteps;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
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

import java.time.LocalDate;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.batch.jobs.helpers.BaseSpecificationSortableScrollableResultReader;
import nl.rivm.screenit.model.OrganisatieParameterKey;
import nl.rivm.screenit.model.cervix.CervixUitnodiging;
import nl.rivm.screenit.model.cervix.enums.CervixMonsterType;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.OrganisatieParameterService;
import nl.rivm.screenit.specification.cervix.CervixUitnodigingSpecification;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.jpa.domain.Specification;

public abstract class CervixHerinnerenReader extends BaseSpecificationSortableScrollableResultReader<CervixUitnodiging, Object>
{
	@Autowired
	private ICurrentDateSupplier dateSupplier;

	@Autowired
	private SimplePreferenceService preferenceService;

	@Autowired
	private OrganisatieParameterService organisatieParameterService;

	private final OrganisatieParameterKey maxAantalParameterKey;

	private final CervixMonsterType monsterType;

	protected CervixHerinnerenReader(int fetchSize, OrganisatieParameterKey maxAantalParameterKey, CervixMonsterType monsterType)
	{
		super.setFetchSize(fetchSize);
		this.maxAantalParameterKey = maxAantalParameterKey;
		this.monsterType = monsterType;
	}

	@Override
	protected Specification<CervixUitnodiging> createSpecification()
	{
		return CervixUitnodigingSpecification.heeftClientMetIndicatieAanwezig()
			.and(CervixUitnodigingSpecification.heeftGeenVertrokkenPersoonUitNederlandDatum())
			.and(CervixUitnodigingSpecification.heeftGeenPersoonMetOverledenDatum())
			.and(CervixUitnodigingSpecification.heeftLopendeRonde())
			.and(CervixUitnodigingSpecification.heeftHerinneren(true))
			.and(CervixUitnodigingSpecification.heeftMergedBrieven())
			.and(CervixUitnodigingSpecification.heeftGeenGeanulleerdeHerinneringDatum())
			.and(CervixUitnodigingSpecification.heeftMonsterType(monsterType));
	}

	@Override
	protected int getMaxResults()
	{
		var maxAantalHerinneringen = getMaxAantalHerinneringen(maxAantalParameterKey);
		return maxAantalHerinneringen == null ? -1 : maxAantalHerinneringen;
	}

	protected LocalDate getMaxPeriodeDatum(PreferenceKey preferenceKey)
	{
		var weken = preferenceService.getInteger(preferenceKey.name());
		return dateSupplier.getLocalDate().minusWeeks(weken).plusDays(1);
	}

	protected Integer getMaxAantalHerinneringen(OrganisatieParameterKey paramKey)
	{
		return organisatieParameterService.getOrganisatieParameter(null, paramKey);
	}

	@Override
	protected Class<CervixUitnodiging> getEntityClass()
	{
		return CervixUitnodiging.class;
	}

	@Override
	protected Class<Object> getResultClass()
	{
		return Object.class;
	}
}
