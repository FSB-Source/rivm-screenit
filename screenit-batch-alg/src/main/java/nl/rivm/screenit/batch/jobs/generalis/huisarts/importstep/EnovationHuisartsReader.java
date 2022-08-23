package nl.rivm.screenit.batch.jobs.generalis.huisarts.importstep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-alg
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

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.batch.BaseCsvFileReader;
import nl.rivm.screenit.batch.jobs.generalis.huisarts.EnovationHuisartsJobListener;
import nl.rivm.screenit.service.ZorgmailImportMapping;
import nl.rivm.screenit.service.ZorgmailImportService;
import nl.rivm.screenit.service.ZorgmailImportVoortgang;

import org.springframework.beans.factory.annotation.Autowired;

@Slf4j
public class EnovationHuisartsReader extends BaseCsvFileReader<Object[]>
{
	@Autowired
	private ZorgmailImportService zorgmailImportService;

	@Override
	protected Object[] parseLine(String[] line, int regelNummer, String bestandsNaam) throws IllegalStateException
	{
		Object[] regel = new Object[] { regelNummer, line };
		if (regelNummer == 1)
		{
			ZorgmailImportMapping zorgmailImportMapping = zorgmailImportService.maakMapping(line);
			getExecutionContext().put(EnovationHuisartsJobListener.ZM_BESTAND_MAPPING, zorgmailImportMapping);
			ZorgmailImportVoortgang voortgang = new ZorgmailImportVoortgang();
			getExecutionContext().put(EnovationHuisartsJobListener.ZM_BESTAND_VOORTGANG, voortgang);
		}
		return regel;
	}

}
