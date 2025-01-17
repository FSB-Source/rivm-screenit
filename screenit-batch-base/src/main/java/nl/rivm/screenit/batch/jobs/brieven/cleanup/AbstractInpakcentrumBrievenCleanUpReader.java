package nl.rivm.screenit.batch.jobs.brieven.cleanup;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-base
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

import nl.rivm.screenit.model.MergedBrieven;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.service.ICurrentDateSupplier;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.jpa.domain.Specification;

import static nl.rivm.screenit.specification.algemeen.MergedBrievenSpecification.heeftBriefTypeIn;
import static nl.rivm.screenit.specification.algemeen.MergedBrievenSpecification.heeftCreatieDatumVoorOfOp;

public abstract class AbstractInpakcentrumBrievenCleanUpReader<MB extends MergedBrieven<?>> extends AbstractBrievenCleanUpReader<MB>
{
	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	protected abstract Bevolkingsonderzoek getBevolkingsonderzoek();

	@Override
	protected Specification<MB> createSpecification()
	{
		var peilMoment = currentDateSupplier.getLocalDateTime().minusDays(getBewaarTermijnInDagen());
		var briefTypes = BriefType.getBriefTypesMetOrganisatieType(true, OrganisatieType.INPAKCENTRUM, getBevolkingsonderzoek());

		return isVerwijderd(false)
			.and(heeftCreatieDatumVoorOfOp(peilMoment))
			.and(heeftBriefTypeIn(briefTypes));
	}
}
