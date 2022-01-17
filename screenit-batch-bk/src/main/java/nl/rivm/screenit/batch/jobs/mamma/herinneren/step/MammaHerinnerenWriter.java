package nl.rivm.screenit.batch.jobs.mamma.herinneren.step;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bk
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

import nl.rivm.screenit.batch.jobs.helpers.BaseWriter;
import nl.rivm.screenit.batch.jobs.mamma.herinneren.MammaHerinnerenConstants;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.MammaUitnodiging;
import nl.rivm.screenit.service.BaseBriefService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.beans.factory.annotation.Autowired;

public class MammaHerinnerenWriter extends BaseWriter<MammaScreeningRonde>
{

	@Autowired
	private BaseBriefService briefService;

	@Autowired
	private HibernateService hibernateService;

	@Override
	protected void write(MammaScreeningRonde ronde) throws Exception
	{
		MammaUitnodiging laatsteUitnodiging = ronde.getLaatsteUitnodiging();
		laatsteUitnodiging.setHerinnered(true);
		hibernateService.saveOrUpdate(laatsteUitnodiging);

		briefService.maakBvoBrief(ronde, BriefType.MAMMA_HERINNERING);
		aantalContextOphogen(MammaHerinnerenConstants.TOTAAL_AANTAL_BRIEVEN_KEY);
	}

}
