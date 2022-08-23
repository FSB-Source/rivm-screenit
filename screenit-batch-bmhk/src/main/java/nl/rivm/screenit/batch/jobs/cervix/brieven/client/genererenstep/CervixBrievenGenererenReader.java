
package nl.rivm.screenit.batch.jobs.cervix.brieven.client.genererenstep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

import lombok.AllArgsConstructor;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.batch.jobs.brieven.genereren.AbstractBrievenGenererenReader;
import nl.rivm.screenit.model.cervix.CervixBrief;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.GbaStatus;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.hibernate.Criteria;
import org.hibernate.criterion.Restrictions;
import org.hibernate.sql.JoinType;
import org.springframework.batch.item.ExecutionContext;
import org.springframework.stereotype.Component;

@Component
@AllArgsConstructor
public class CervixBrievenGenererenReader extends AbstractBrievenGenererenReader<CervixBrief>
{

	private final ICurrentDateSupplier currentDateSupplier;

	private final SimplePreferenceService preferenceService;

	@Override
	protected Long getScreeningOrganisatieId(ExecutionContext context)
	{
		return context.getLong(CervixBrievenGenererenPartitioner.KEY_SCREENINGORGANISATIEID);
	}

	@Override
	protected Criteria additionalRestrictions(Criteria crit, ExecutionContext context)
	{
		crit.add(Restrictions.eq("client.gbaStatus", GbaStatus.INDICATIE_AANWEZIG));

		var briefType = BriefType.valueOf(context.getString(CervixBrievenGenererenPartitioner.KEY_BRIEFTYPE));
		crit.add(Restrictions.eq("briefType", briefType));

		switch (briefType)
		{
		case CERVIX_CYTOLOGIE_AFWIJKING:
		case CERVIX_VOLGEND_MONSTER_CYTOLOGIE_AFWIJKING:
		case CERVIX_CONTROLEUITSTRIJKJE_AFWIJKING:
			crit.add(Restrictions.le("creatieDatum",
				currentDateSupplier.getDateTime().minusDays(preferenceService.getInteger(PreferenceKey.CERVIX_UITSTEL_UITSLAGBRIEF_PAP3A2_OF_HOGER.toString())).toDate()));
			break;
		}

		crit.createAlias("uitnodiging", "uitnodiging", JoinType.LEFT_OUTER_JOIN);
		crit.add(Restrictions.or(Restrictions.isNull("uitnodiging.id"), Restrictions.isNotNull("gemeente.bmhkLaboratorium")));

		return crit;
	}
}
