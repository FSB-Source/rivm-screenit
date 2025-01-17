package nl.rivm.screenit.batch.jobs.aftergba.deelnamemodus;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-base
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

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.batch.jobs.helpers.BaseWriter;
import nl.rivm.screenit.service.TransgenderService;
import nl.rivm.screenit.model.DeelnamemodusDossier;

import org.springframework.beans.factory.annotation.Autowired;

@Slf4j
public abstract class AbstractDeelnamemodusWriter<T extends DeelnamemodusDossier> extends BaseWriter<T>
{
	@Autowired
	private TransgenderService transgenderService;

	@Override
	protected void write(T dossier) throws Exception
	{
		LOG.info("Deelnamemodus bijwerken voor client: {} ", dossier.getClient().getId());
		transgenderService.bijwerkenDeelnamemodus(dossier);
	}
}
