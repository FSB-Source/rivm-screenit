package nl.rivm.screenit.batch.jobs.cervix.heroverwegers;

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

import nl.rivm.screenit.batch.jobs.helpers.BaseWriter;
import nl.rivm.screenit.model.cervix.cis.CervixCISHistorie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;

import org.springframework.stereotype.Component;

@Component
@AllArgsConstructor
public class CervixHeroverwegersWriter extends BaseWriter<CervixCISHistorie>
{
	private final BaseBriefService briefService;

	private final ICurrentDateSupplier dateSupplier;

	private final LogService logService;

	@Override
	protected void write(CervixCISHistorie item) throws Exception
	{
		briefService.maakBvoBrief(item.getAfmelding(), BriefType.CERVIX_HEROVERWEGERS, dateSupplier.getDate());
		logService.logGebeurtenis(LogGebeurtenis.CERVIX_HEROVERWEGERSBRIEF_AANGEMAAKT, item.getDossier().getClient(), Bevolkingsonderzoek.CERVIX);
		aantalContextOphogen(BriefType.CERVIX_HEROVERWEGERS.name());
	}
}
