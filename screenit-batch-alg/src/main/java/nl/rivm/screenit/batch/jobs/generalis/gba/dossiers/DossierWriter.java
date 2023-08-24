package nl.rivm.screenit.batch.jobs.generalis.gba.dossiers;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-alg
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

import java.util.Set;

import nl.rivm.screenit.batch.jobs.generalis.gba.GbaConstants;
import nl.rivm.screenit.batch.jobs.helpers.BaseWriter;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.service.DossierFactory;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class DossierWriter extends BaseWriter<Client>
{
	@Autowired
	private DossierFactory dossierFactory;

	@Override
	protected void write(Client client)
	{
		Set<Bevolkingsonderzoek> bevolkingsonderzoeken = dossierFactory.maakDossiers(client);

		if (bevolkingsonderzoeken.contains(Bevolkingsonderzoek.COLON))
		{
			aantalContextOphogen(GbaConstants.AANTAL_COLON_DOSSIERS_KEY);
		}
		if (bevolkingsonderzoeken.contains(Bevolkingsonderzoek.CERVIX))
		{
			aantalContextOphogen(GbaConstants.AANTAL_CERVIX_DOSSIERS_KEY);
		}
		if (bevolkingsonderzoeken.contains(Bevolkingsonderzoek.MAMMA))
		{
			aantalContextOphogen(GbaConstants.AANTAL_MAMMA_DOSSIERS_KEY);
		}
	}
}
