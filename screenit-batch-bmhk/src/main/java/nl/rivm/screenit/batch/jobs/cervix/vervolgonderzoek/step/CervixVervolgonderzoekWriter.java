package nl.rivm.screenit.batch.jobs.cervix.vervolgonderzoek.step;

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

import nl.rivm.screenit.batch.jobs.cervix.vervolgonderzoek.CervixVervolgonderzoekConstants;
import nl.rivm.screenit.batch.jobs.helpers.BaseWriter;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde;
import nl.rivm.screenit.model.cervix.CervixUitnodiging;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.service.cervix.CervixFactory;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.beans.factory.annotation.Autowired;

public class CervixVervolgonderzoekWriter extends BaseWriter<CervixScreeningRonde>
{

	@Autowired
	private CervixFactory factory;

	@Autowired
	private HibernateService hibernateService;

	@Override
	protected void write(CervixScreeningRonde ronde) throws Exception
	{
		CervixUitnodiging uitnodiging = factory.maakUitnodiging(ronde, BriefType.CERVIX_UITNODIGING_CONTROLEUITSTRIJKJE, true, false);
		ronde.setUitnodigingVervolgonderzoek(uitnodiging);
		hibernateService.saveOrUpdate(ronde);

		aantalContextOphogen(CervixVervolgonderzoekConstants.VERVOLGONDERZOEK_AANTAL_KEY);
	}
}
