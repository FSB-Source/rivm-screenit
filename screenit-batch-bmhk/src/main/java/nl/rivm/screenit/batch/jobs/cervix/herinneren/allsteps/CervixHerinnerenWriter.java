package nl.rivm.screenit.batch.jobs.cervix.herinneren.allsteps;

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

import nl.rivm.screenit.batch.jobs.cervix.herinneren.CervixHerinnerenConstants;
import nl.rivm.screenit.batch.jobs.helpers.BaseWriter;
import nl.rivm.screenit.model.cervix.CervixBrief;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde;
import nl.rivm.screenit.model.cervix.CervixUitnodiging;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.cervix.CervixFactory;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.beans.factory.annotation.Autowired;

public class CervixHerinnerenWriter extends BaseWriter<CervixUitnodiging>
{
	@Autowired
	private BaseBriefService briefService;

	@Autowired
	private CervixFactory factory;

	@Autowired
	private HibernateService hibernateService;

	@Override
	protected void write(CervixUitnodiging uitnodiging) throws Exception
	{
		uitnodiging.setHerinneren(false);
		hibernateService.saveOrUpdate(uitnodiging);

		CervixScreeningRonde ronde = uitnodiging.getScreeningRonde();
		CervixBrief brief = uitnodiging.getBrief();

		CervixBrief nieuweBrief = null;
		switch (uitnodiging.getMonsterType())
		{
		case UITSTRIJKJE:
			if (BriefType.getCervixUitnodigingen().contains(brief.getBriefType()))
			{
				nieuweBrief = briefService.maakBvoBrief(ronde, BriefType.CERVIX_HERINNERING_UITNODIGING);
			}
			else
			{
				nieuweBrief = briefService.maakBvoBrief(ronde, brief.getBriefType());
				nieuweBrief.setHerdruk(brief);

				CervixUitnodiging nieuweUitnodiging = factory.maakUitnodiging(ronde, nieuweBrief);
				nieuweUitnodiging.setHerinnering(true);

				hibernateService.saveOrUpdateAll(nieuweBrief, nieuweUitnodiging);
			}
			break;
		case ZAS:
			nieuweBrief = briefService.maakBvoBrief(ronde, BriefType.CERVIX_HERINNERING_ZAS_UITNODIGING);
			break;
		}

		aantallenContextOphogen(CervixHerinnerenConstants.TOTAAL_AANTAL_BRIEVEN_KEY, CervixHerinnerenConstants.BRIEF_TYPE_KEY, nieuweBrief.getBriefType());
	}
}
