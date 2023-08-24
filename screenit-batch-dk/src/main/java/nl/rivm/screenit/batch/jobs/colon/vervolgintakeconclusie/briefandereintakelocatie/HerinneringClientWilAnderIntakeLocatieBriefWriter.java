package nl.rivm.screenit.batch.jobs.colon.vervolgintakeconclusie.briefandereintakelocatie;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-dk
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.batch.jobs.helpers.BaseWriter;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.service.BaseBriefService;

import org.springframework.stereotype.Component;

@Component
@AllArgsConstructor
public class HerinneringClientWilAnderIntakeLocatieBriefWriter extends BaseWriter<ColonScreeningRonde>
{

	private final BaseBriefService briefService;

	@Override
	protected void write(ColonScreeningRonde ronde) throws Exception
	{
		briefService.maakBvoBrief(ronde, BriefType.COLON_HERINNERING_ANDERE_INTAKELOCATIE);
	}
}
