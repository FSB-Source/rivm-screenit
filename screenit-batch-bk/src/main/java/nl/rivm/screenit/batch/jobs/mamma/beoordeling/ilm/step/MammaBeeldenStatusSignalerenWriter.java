package nl.rivm.screenit.batch.jobs.mamma.beoordeling.ilm.step;

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

import java.util.Date;

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.mamma.MammaMammografie;

public class MammaBeeldenStatusSignalerenWriter extends MammaAbstractBeeldenStatusSignalerenWriter<MammaMammografie>
{
	@Override
	protected void write(MammaMammografie mammografie)
	{
		boolean isBezwaar = false;
		boolean isUploaded = false;
		long accessionNumber = mammografie.getOnderzoek().getAfspraak().getUitnodiging().getScreeningRonde().getUitnodigingsNr();
		Date statusDatum = mammografie.getIlmStatusDatum();
		Client client = mammografie.getOnderzoek().getAfspraak().getUitnodiging().getScreeningRonde().getDossier().getClient();

		registreerSignalering(isBezwaar, isUploaded, accessionNumber, statusDatum, client);
	}
}
