package nl.rivm.screenit.batch.jobs.cervix.brieven.client.genererenstep;

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

import lombok.AllArgsConstructor;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.batch.jobs.brieven.genereren.AbstractBrievenGenererenReader;
import nl.rivm.screenit.model.cervix.CervixBrief;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.GbaStatus;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import static nl.rivm.screenit.model.enums.BriefType.CERVIX_CONTROLEUITSTRIJKJE_AFWIJKING;
import static nl.rivm.screenit.model.enums.BriefType.CERVIX_CYTOLOGIE_AFWIJKING;
import static nl.rivm.screenit.model.enums.BriefType.CERVIX_VOLGEND_MONSTER_CYTOLOGIE_AFWIJKING;
import static nl.rivm.screenit.specification.algemeen.BriefSpecification.heeftBriefType;
import static nl.rivm.screenit.specification.algemeen.BriefSpecification.isAangemaaktVoorOfOp;
import static nl.rivm.screenit.specification.algemeen.ClientSpecification.heeftGbaStatus;
import static nl.rivm.screenit.specification.algemeen.GemeenteSpecification.heeftBmhkLaboratorium;
import static nl.rivm.screenit.specification.cervix.CervixBriefSpecification.heeftGeenUitnodiging;

@Component
@AllArgsConstructor
public class CervixBrievenGenererenReader extends AbstractBrievenGenererenReader<CervixBrief>
{

	private final ICurrentDateSupplier currentDateSupplier;

	private final SimplePreferenceService preferenceService;

	@Override
	protected Long getScreeningOrganisatieId()
	{
		return getStepExecutionContext().getLong(CervixBrievenGenererenPartitioner.KEY_SCREENINGORGANISATIEID);
	}

	@Override
	protected Specification<CervixBrief> createSpecification()
	{
		var briefType = BriefType.valueOf(getStepExecutionContext().getString(CervixBrievenGenererenPartitioner.KEY_BRIEFTYPE));
		var specification = super.createSpecification()
			.and(heeftGbaStatus(GbaStatus.INDICATIE_AANWEZIG).with(r -> clientJoin(r)))
			.and(heeftBriefType(briefType));
		if (List.of(CERVIX_CYTOLOGIE_AFWIJKING, CERVIX_VOLGEND_MONSTER_CYTOLOGIE_AFWIJKING, CERVIX_CONTROLEUITSTRIJKJE_AFWIJKING).contains(briefType))
		{
			specification = specification.and(isAangemaaktVoorOfOp(
				DateUtil.minDagen(currentDateSupplier.getDate(), preferenceService.getInteger(PreferenceKey.CERVIX_UITSTEL_UITSLAGBRIEF_PAP3A2_OF_HOGER.toString()))));
		}

		specification = specification.and(heeftGeenUitnodiging().or(heeftBmhkLaboratorium().with(r -> gemeenteJoin(r))));

		return specification;
	}

}
