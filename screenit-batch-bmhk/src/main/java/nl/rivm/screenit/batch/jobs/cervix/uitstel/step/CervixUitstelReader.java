package nl.rivm.screenit.batch.jobs.cervix.uitstel.step;

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

import nl.rivm.screenit.batch.jobs.helpers.BaseSpecificationScrollableResultReader;
import nl.rivm.screenit.model.cervix.CervixUitstel;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.specification.cervix.CervixUitstelSpecification;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

@Component
public class CervixUitstelReader extends BaseSpecificationScrollableResultReader<CervixUitstel, Long>
{
	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	public CervixUitstelReader()
	{
		setFetchSize(50);
	}

	@Override
	public Specification<CervixUitstel> createSpecification()
	{
		var vandaag = currentDateSupplier.getLocalDate();
		return CervixUitstelSpecification.heeftClientMetIndicatieAanwezig()
			.and(CervixUitstelSpecification.heeftPersoonMetOverledenDatum())
			.and(CervixUitstelSpecification.heeftGeenVertrokkenPersoonUitNederlandDatum())
			.and(CervixUitstelSpecification.heeftLopendeRonde())
			.and(CervixUitstelSpecification.heeftGeenGeannuleerdDatum())
			.and(CervixUitstelSpecification.heeftUitstellenTotDatumEerderDan(vandaag));
	}
}
