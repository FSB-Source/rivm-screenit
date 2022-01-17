package nl.rivm.screenit.batch.jobs.mamma.aftergba.deelnamekansenherzien;

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
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.service.mamma.MammaBaseKansberekeningService;

import org.springframework.beans.factory.annotation.Autowired;

public class MammaDeelnamekansenHerzienWriter extends BaseWriter<MammaDossier>
{
	@Autowired
	private MammaBaseKansberekeningService baseKansberekeningService;

	@Override
	protected void write(MammaDossier dossier)
	{
		baseKansberekeningService.dossierEventHerzien(dossier);
	}
}
