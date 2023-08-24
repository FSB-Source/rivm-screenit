package nl.rivm.screenit.batch.jobs.cervix.zaskoppelen.koppelstep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
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

import nl.rivm.screenit.batch.jobs.cervix.zaskoppelen.ZasKoppelenConstants;
import nl.rivm.screenit.batch.jobs.helpers.BaseKoppelReader;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.rivm.screenit.service.LogService;

import org.springframework.batch.item.ItemReader;
import org.springframework.stereotype.Component;

import generated.KOPPELDATA.VERZONDENUITNODIGING;

@Component
@AllArgsConstructor
public class ZasKoppelReader extends BaseKoppelReader implements ItemReader<VERZONDENUITNODIGING>
{
	private final LogService logService;

	@Override
	protected void logEindValidatie(LogEvent eindEvent)
	{
		logService.logGebeurtenis(LogGebeurtenis.CERVIX_ZAS_CONTROLE_KOPPELEN_AFGEROND, eindEvent, Bevolkingsonderzoek.CERVIX);
	}

	@Override
	protected void logKoppelFout(LogEvent logEvent)
	{
		logService.logGebeurtenis(LogGebeurtenis.CERVIX_ZAS_CONTROLE_KOPPELEN_FOUT, logEvent, Bevolkingsonderzoek.CERVIX);
	}

	@Override
	protected String getAfkortingBvo()
	{
		return Bevolkingsonderzoek.CERVIX.getAfkorting();
	}

	@Override
	protected String getKoppelenConstant()
	{
		return ZasKoppelenConstants.RAPPORTAGEKEYZASKOPPELEN;
	}
}
