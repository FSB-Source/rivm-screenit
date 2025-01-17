package nl.rivm.screenit.batch.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
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

import java.util.List;

import nl.rivm.screenit.batch.service.CervixUitnodigingService;
import nl.rivm.screenit.model.OrganisatieParameterKey;
import nl.rivm.screenit.model.TablePerClassHibernateObject_;
import nl.rivm.screenit.model.Uitnodiging_;
import nl.rivm.screenit.model.cervix.enums.CervixMonsterType;
import nl.rivm.screenit.repository.cervix.CervixUitnodigingRepository;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.OrganisatieParameterService;
import nl.rivm.screenit.specification.algemeen.InpakbareUitnodigingSpecification;
import nl.rivm.screenit.specification.cervix.CervixUitnodigingSpecification;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;

@Service
public class CervixUitnodigingServiceImpl implements CervixUitnodigingService
{
	@Autowired
	private OrganisatieParameterService organisatieParameterService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private CervixUitnodigingRepository uitnodigingRepository;

	@Override
	public List<Long> getTeVersturenZasUitnodigingen()
	{
		var specification = CervixUitnodigingSpecification.heeftActieveClient()
			.and(CervixUitnodigingSpecification.heeftGemeenteMetBmhkLaboratorium())
			.and(CervixUitnodigingSpecification.heeftLopendeRonde())
			.and(InpakbareUitnodigingSpecification.heeftGeenVerstuurdDatum())
			.and(CervixUitnodigingSpecification.heeftUitnodigingsDatumVoorDatum(currentDateSupplier.getDate()))
			.and(CervixUitnodigingSpecification.heeftMonsterType(CervixMonsterType.ZAS))
			.and(CervixUitnodigingSpecification.heeftGeenGeannuleerdDatum())
			.and(CervixUitnodigingSpecification.heeftTeVersturenZasUitnodigingen());

		var maxAantalUitnodigingen = getMaxAantalZasUitnodigingen();
		var query = uitnodigingRepository.findWith(specification, Long.class,
			q -> q.projection((cb, r) -> r.get(TablePerClassHibernateObject_.id)));
		if (maxAantalUitnodigingen != null)
		{
			return query
				.sortBy(Sort.by(Sort.Order.asc(Uitnodiging_.UITNODIGINGS_DATUM)))
				.all(0, maxAantalUitnodigingen);
		}
		return query.all();
	}

	private Integer getMaxAantalZasUitnodigingen()
	{
		return organisatieParameterService.getOrganisatieParameter(null, OrganisatieParameterKey.CERVIX_MAX_AANTAL_ZAS_NAAR_INPAKCENTRUM);
	}
}
