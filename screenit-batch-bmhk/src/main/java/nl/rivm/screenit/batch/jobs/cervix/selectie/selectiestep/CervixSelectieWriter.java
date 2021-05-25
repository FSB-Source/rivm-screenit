package nl.rivm.screenit.batch.jobs.cervix.selectie.selectiestep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
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

import nl.rivm.screenit.batch.jobs.cervix.selectie.CervixSelectieConstants;
import nl.rivm.screenit.batch.jobs.helpers.BaseWriter;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.cervix.CervixDossier;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.service.cervix.CervixFactory;

import org.springframework.beans.factory.annotation.Autowired;

public class CervixSelectieWriter extends BaseWriter<Client>
{

	@Autowired
	private CervixFactory factory;

	@Override
	protected void write(Client client) throws Exception
	{
		CervixDossier dossier = client.getCervixDossier();
		CervixScreeningRonde ronde = factory.maakRonde(dossier);
		factory.maakUitnodiging(ronde, BriefType.CERVIX_UITNODIGING, true, false);

		aantalContextOphogen(CervixSelectieConstants.SELECTIE_AANTAL_KEY);
	}
}
