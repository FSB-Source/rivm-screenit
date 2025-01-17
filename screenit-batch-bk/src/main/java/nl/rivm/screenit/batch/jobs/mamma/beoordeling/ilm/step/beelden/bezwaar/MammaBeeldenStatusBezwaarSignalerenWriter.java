package nl.rivm.screenit.batch.jobs.mamma.beoordeling.ilm.step.beelden.bezwaar;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bk
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

import nl.rivm.screenit.batch.jobs.mamma.beoordeling.ilm.step.beelden.MammaAbstractBeeldenStatusSignalerenWriter;
import nl.rivm.screenit.model.mamma.MammaIlmBezwaarPoging;
import nl.rivm.screenit.util.DateUtil;

import org.springframework.stereotype.Component;

@Component
public class MammaBeeldenStatusBezwaarSignalerenWriter extends MammaAbstractBeeldenStatusSignalerenWriter<MammaIlmBezwaarPoging>
{

	@Override
	protected void write(MammaIlmBezwaarPoging bezwaarPoging)
	{
		var isBezwaar = true;
		var isUploaded = bezwaarPoging.isUploaded();
		var accessionNumber = bezwaarPoging.getAccessionNumber();
		var statusDatum = DateUtil.toUtilDate(bezwaarPoging.getStatusDatum());
		var client = bezwaarPoging.getDossier().getClient();

		registreerSignalering(isBezwaar, isUploaded, accessionNumber, statusDatum, client);
	}

}
