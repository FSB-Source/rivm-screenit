package nl.rivm.screenit.batch.jobs.mamma.beoordeling.ilm.step.beelden.upload;

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

import java.util.List;

import lombok.AllArgsConstructor;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.batch.jobs.helpers.BaseSpecificationScrollableResultReader;
import nl.rivm.screenit.model.mamma.MammaUploadBeeldenPoging;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import static nl.rivm.screenit.model.mamma.enums.MammaMammografieIlmStatus.TE_VERWIJDEREN;
import static nl.rivm.screenit.model.mamma.enums.MammaMammografieIlmStatus.VERWIJDEREN_MISLUKT;
import static nl.rivm.screenit.specification.mamma.MammaUploadBeeldenPogingSpecification.heeftIlmStatusIn;
import static nl.rivm.screenit.specification.mamma.MammaUploadBeeldenPogingSpecification.heeftStatusDatumVoor;

@Component
@AllArgsConstructor
public class MammaBeeldenStatusUploadSignalerenReader extends BaseSpecificationScrollableResultReader<MammaUploadBeeldenPoging>
{

	private final SimplePreferenceService preferenceService;

	private final ICurrentDateSupplier currentDateSupplier;

	@Override
	protected Specification<MammaUploadBeeldenPoging> createSpecification()
	{
		var signaleerTermijn = currentDateSupplier.getLocalDate().minusDays(preferenceService.getInteger(PreferenceKey.ILM_SIGNALEERTERMIJN_BEELDEN_STATUS.name())).atStartOfDay();
		return heeftStatusDatumVoor(signaleerTermijn).and(heeftIlmStatusIn(List.of(TE_VERWIJDEREN, VERWIJDEREN_MISLUKT)));
	}

}
