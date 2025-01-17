package nl.rivm.screenit.batch.jobs.mamma.aftergba.afsprakenannuleren;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bk
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

import javax.persistence.criteria.From;
import javax.persistence.criteria.Join;

import nl.rivm.screenit.batch.jobs.helpers.BaseSpecificationScrollableResultReader;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Client_;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaDossier_;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde_;
import nl.rivm.screenit.model.mamma.MammaUitnodiging_;
import nl.rivm.screenit.model.mamma.enums.MammaAfspraakStatus;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.specification.ExtendedSpecification;
import nl.rivm.screenit.specification.SpecificationUtil;
import nl.rivm.screenit.specification.algemeen.PersoonSpecification;

import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import com.google.common.collect.Range;

import static nl.rivm.screenit.batch.jobs.mamma.aftergba.AfterGbaJobConfiguration.AFTER_GBA_JOB_READER_FETCH_SIZE;
import static nl.rivm.screenit.specification.mamma.MammaAfspraakSpecification.heeftStatus;
import static nl.rivm.screenit.specification.mamma.MammaAfspraakSpecification.valtInDatumTijdPeriode;
import static org.springframework.data.jpa.domain.Specification.not;

@Component
public class MammaAfsprakenAnnulerenReader extends BaseSpecificationScrollableResultReader<Client>
{

	private final ICurrentDateSupplier dateSupplier;

	public MammaAfsprakenAnnulerenReader(ICurrentDateSupplier dateSupplier)
	{
		super.setFetchSize(AFTER_GBA_JOB_READER_FETCH_SIZE);
		this.dateSupplier = dateSupplier;
	}

	@Override
	protected Specification<Client> createSpecification()
	{
		return overledenOfInBuitenland().and(geplandeToekomstigeAfspraak());
	}

	private Specification<Client> overledenOfInBuitenland()
	{
		return not(PersoonSpecification.isNietOverledenEnWoontInNederland().with(Client_.persoon));
	}

	private ExtendedSpecification<Client> geplandeToekomstigeAfspraak()
	{
		return heeftStatus(MammaAfspraakStatus.GEPLAND)
			.and(valtInDatumTijdPeriode(Range.greaterThan(dateSupplier.getLocalDateTime())))
			.with(r -> laatsteAfspraakJoin(r));
	}

	private static Join<?, MammaAfspraak> laatsteAfspraakJoin(From<?, ? extends Client> clientRoot)
	{
		var dossierJoin = SpecificationUtil.join(clientRoot, Client_.mammaDossier);
		var rondeJoin = SpecificationUtil.join(dossierJoin, MammaDossier_.laatsteScreeningRonde);
		var uitnodigingJoin = SpecificationUtil.join(rondeJoin, MammaScreeningRonde_.laatsteUitnodiging);
		return SpecificationUtil.join(uitnodigingJoin, MammaUitnodiging_.laatsteAfspraak);
	}
}
