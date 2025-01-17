package nl.rivm.screenit.batch.jobs.ilm.applicatielogging;

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

import lombok.Setter;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.batch.jobs.helpers.BaseSpecificationScrollableResultReader;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.logging.LogRegel;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.jpa.domain.Specification;

import static nl.rivm.screenit.specification.algemeen.LogRegelSpecification.heeftAantalBevolkingsonderzoekenGelijkAan;
import static nl.rivm.screenit.specification.algemeen.LogRegelSpecification.heeftAantalBevolkingsonderzoekenNietGelijkAan;
import static nl.rivm.screenit.specification.algemeen.LogRegelSpecification.heeftBevolkingsonderzoek;
import static nl.rivm.screenit.specification.algemeen.LogRegelSpecification.heeftGebeurtenisDatumVoor;

public class IlmApplicatieLoggingVerwijderenReader extends BaseSpecificationScrollableResultReader<LogRegel>
{
	@Autowired
	private SimplePreferenceService preferenceService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Setter
	private Bevolkingsonderzoek bvo;

	@Override
	protected Specification<LogRegel> createSpecification()
	{
		var peilDatum = currentDateSupplier.getLocalDate().minusDays(preferenceService.getInteger(PreferenceKey.ILM_BEWAARTERMIJN_NIET_MEDISCH.name()));

		return heeftGebeurtenisDatumVoor(peilDatum)
			.and(bvoSpecifiekeSpecification());
	}

	private Specification<LogRegel> bvoSpecifiekeSpecification()
	{
		if (bvo == null)
		{
			return heeftAantalBevolkingsonderzoekenNietGelijkAan(1);
		}

		return heeftBevolkingsonderzoek(bvo).and(heeftAantalBevolkingsonderzoekenGelijkAan(1));
	}
}
