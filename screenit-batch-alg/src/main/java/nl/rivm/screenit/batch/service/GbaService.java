package nl.rivm.screenit.batch.service;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-alg
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

import nl.rivm.screenit.batch.jobs.generalis.gba.exception.GbaImportException;
import nl.rivm.screenit.model.gba.GbaVerwerkingsLog;
import nl.topicuszorg.gba.vertrouwdverbonden.model.Vo107Bericht;

import org.hibernate.Criteria;

public interface GbaService
{

	void importVo107Bericht(Vo107Bericht bericht, GbaVerwerkingsLog log) throws GbaImportException;

	void logGbaImportError(GbaImportException e, GbaVerwerkingsLog verwerkingLog);

	Criteria getAllAdressenZonderCoordinanten();
}
