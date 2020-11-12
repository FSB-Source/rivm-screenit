package nl.rivm.screenit.batch.jobs.colon.gunstigeuitslag.gunstigestep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-dk
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.batch.jobs.colon.gunstigeuitslag.GunstigeUitslagConstants;
import nl.rivm.screenit.batch.jobs.helpers.BaseWriter;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.colon.enums.ColonUitnodigingsintervalType;
import nl.rivm.screenit.service.colon.ColonDossierBaseService;
import nl.rivm.screenit.service.colon.ColonScreeningsrondeService;

import org.springframework.beans.factory.annotation.Autowired;

public class GunstigeUitslagBriefWriter extends BaseWriter<Client>
{

	@Autowired
	private ColonScreeningsrondeService screeningsrondeService;

	@Autowired
	private ColonDossierBaseService dossierBaseService;

	@Override
	protected void write(Client client) throws Exception
	{
		boolean gemaakt = screeningsrondeService.maakGunstigeUitslagBriefVoorLaatsteRonde(client);
		if (gemaakt)
		{
			dossierBaseService.setDatumVolgendeUitnodiging(client.getColonDossier(), ColonUitnodigingsintervalType.GUNSTIGE_UITSLAG);
			updateExecutionPlusOne();
		}
	}

	private void updateExecutionPlusOne()
	{
		int geselecteerd = getExecutionContext().getInt(GunstigeUitslagConstants.GESELECTEERD);
		getExecutionContext().put(GunstigeUitslagConstants.GESELECTEERD, geselecteerd + 1);
	}
}
