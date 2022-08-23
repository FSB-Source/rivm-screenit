package nl.rivm.screenit.batch.jobs.colon.selectie.afrondenstep;

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

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.batch.jobs.helpers.BaseWriter;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.service.BaseScreeningRondeService;

import org.springframework.stereotype.Component;

@Component
@Slf4j
@AllArgsConstructor
public class ColonVerlopenRondesWriter extends BaseWriter<ColonScreeningRonde>
{

	private final BaseScreeningRondeService screeningRondeService;

	@Override
	protected void write(ColonScreeningRonde ronde)
	{
		LOG.info("gevonden ronde: {} wordt afgerond", ronde.getId());

		screeningRondeService.screeningRondeAfronden(ronde, Constants.RONDE_AFROND_REDEN_BUITEN_DOELGROEP);
	}
}
