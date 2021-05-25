package nl.rivm.screenit.batch.jobs.colon.selectie.afrondenstep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-dk
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.service.BaseScreeningRondeService;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;

public class ColonVerlopenRondesWriter extends BaseWriter<ColonScreeningRonde>
{

	private Logger LOG = LoggerFactory.getLogger(ColonVerlopenRondesWriter.class);

	@Autowired
	private BaseScreeningRondeService screeningRondeService;

	@Override
	protected void write(ColonScreeningRonde ronde)
	{
		LOG.info("gevonden ronde: {} wordt afgerond", ronde.getId());

		screeningRondeService.screeningRondeAfronden(ronde, "Cli\u00EBnt behoort niet meer tot de doelgroep");
	}
}
