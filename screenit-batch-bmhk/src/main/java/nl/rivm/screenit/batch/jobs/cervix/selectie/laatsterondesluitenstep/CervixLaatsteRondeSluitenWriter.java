package nl.rivm.screenit.batch.jobs.cervix.selectie.laatsterondesluitenstep;

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

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.batch.jobs.helpers.BaseWriter;
import nl.rivm.screenit.model.ScreeningRondeStatus;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.stereotype.Component;

@Component
@Slf4j
@AllArgsConstructor
public class CervixLaatsteRondeSluitenWriter extends BaseWriter<CervixScreeningRonde>
{
	private final ICurrentDateSupplier dateSupplier;

	private final HibernateService hibernateService;

	private final LogService logService;

	@Override
	protected void write(CervixScreeningRonde ronde) throws Exception
	{
		LOG.info("Laatste ronde sluiten voor client (id: '{}')", ronde.getDossier().getClient().getId());
		ronde.setStatus(ScreeningRondeStatus.AFGEROND);
		ronde.setStatusDatum(dateSupplier.getDate());
		hibernateService.saveOrUpdate(ronde);
		logService.logGebeurtenis(LogGebeurtenis.CERVIX_RONDE_VERLOPEN, ronde.getDossier().getClient(), Bevolkingsonderzoek.CERVIX);
	}
}
