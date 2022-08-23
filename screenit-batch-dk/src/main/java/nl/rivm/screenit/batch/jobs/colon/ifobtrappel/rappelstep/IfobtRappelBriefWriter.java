package nl.rivm.screenit.batch.jobs.colon.ifobtrappel.rappelstep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-dk
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
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.batch.jobs.colon.ifobtrappel.IfobtRappelJobConstants;
import nl.rivm.screenit.batch.jobs.helpers.BaseWriter;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.LogService;

import org.springframework.stereotype.Component;

@Component
@Slf4j
@AllArgsConstructor
public class IfobtRappelBriefWriter extends BaseWriter<Client>
{

	private final BaseBriefService briefService;

	private final LogService logService;

	@Override
	protected void write(Client client)
	{
		var csr = client.getColonDossier().getLaatsteScreeningRonde();
		var ifobtTest = csr.getLaatsteIFOBTTest();
		var herinneringsBrief = briefService.maakBvoBrief(csr, BriefType.COLON_HERINNERING);
		logService.logGebeurtenis(LogGebeurtenis.RAPPEL_VERZONDEN, client, Bevolkingsonderzoek.COLON);
		ifobtTest.setHerinnering(Boolean.TRUE);
		getHibernateService().saveOrUpdate(ifobtTest);
		herinneringsBrief.setIfobtTest(ifobtTest);
		getHibernateService().saveOrUpdate(herinneringsBrief);
		updateExecutionPlusOne();
	}

	private void updateExecutionPlusOne()
	{
		int herinnerd = getExecutionContext().getInt(IfobtRappelJobConstants.HERINNERD);
		getExecutionContext().put(IfobtRappelJobConstants.HERINNERD, herinnerd + 1);
	}
}
