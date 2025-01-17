package nl.rivm.screenit.main.service.mamma.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.util.Collections;
import java.util.List;

import nl.rivm.screenit.main.model.mamma.beoordeling.MammaCeWerklijstZoekObject;
import nl.rivm.screenit.main.service.RepositoryDataProviderService;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;
import nl.rivm.screenit.model.mamma.enums.MammaOnderzoekStatus;
import nl.rivm.screenit.repository.mamma.MammaOnderzoekRepository;

import org.apache.commons.collections.CollectionUtils;
import org.apache.wicket.extensions.markup.html.repeater.util.SortParam;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;

import static nl.rivm.screenit.specification.mamma.MammaOnderzoekSpecification.filterBriefOnderbrokenOnderzoek;
import static nl.rivm.screenit.specification.mamma.MammaOnderzoekSpecification.filterScreeningsEenheid;
import static nl.rivm.screenit.specification.mamma.MammaOnderzoekSpecification.heeftLaatsteAfspraakVanLaatsteUitnodigingVanLaatsteRonde;
import static nl.rivm.screenit.specification.mamma.MammaOnderzoekSpecification.heeftLopendeRonde;
import static nl.rivm.screenit.specification.mamma.MammaOnderzoekSpecification.heeftPersoonIsNietOverledenEnWoontInNederland;
import static nl.rivm.screenit.specification.mamma.MammaOnderzoekSpecification.heeftStatus;
import static nl.rivm.screenit.specification.mamma.MammaOnderzoekSpecification.isDoorgevoerd;

@Service("MammaCeOnderbrokenOnderzoekenDataProviderService")
public class MammaCeOnderbrokenOnderzoekenDataProviderServiceImpl extends RepositoryDataProviderService<MammaOnderzoek, MammaOnderzoekRepository, MammaCeWerklijstZoekObject>
{
	@Override
	protected Specification<MammaOnderzoek> getSpecification(MammaCeWerklijstZoekObject filter, Sort sortParam)
	{
		return heeftLaatsteAfspraakVanLaatsteUitnodigingVanLaatsteRonde().and(
				isDoorgevoerd(true)).and(heeftLopendeRonde()).and(filterScreeningsEenheid(filter.getScreeningsEenheden())).and(heeftStatus(MammaOnderzoekStatus.ONDERBROKEN))
			.and(filterBriefOnderbrokenOnderzoek(filter.getMetBriefOproepOnderbrokenOnderzoek())).and(heeftPersoonIsNietOverledenEnWoontInNederland());
	}

	@Override
	public List<MammaOnderzoek> findPage(long first, long count, MammaCeWerklijstZoekObject filter, SortParam<String> sortParam)
	{
		if (CollectionUtils.isEmpty(filter.getScreeningsEenheden()))
		{
			return Collections.emptyList();
		}
		return super.findPage(first, count, filter, sortParam);
	}

	@Override
	public long size(MammaCeWerklijstZoekObject filter)
	{
		if (CollectionUtils.isEmpty(filter.getScreeningsEenheden()))
		{
			return 0L;
		}
		return super.size(filter);
	}
}
