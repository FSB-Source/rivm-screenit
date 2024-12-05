package nl.rivm.screenit.batch.jobs.mamma.dense2.verwijderdensiteit;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bk
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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
import nl.rivm.screenit.batch.jobs.mamma.dense2.MammaDense2Constants;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;
import nl.rivm.screenit.service.mamma.MammaBaseDense2Service;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class MammaVerwijderDensiteitWriter extends BaseWriter<MammaOnderzoek>
{
	@Autowired
	private MammaBaseDense2Service dense2Service;

	@Override
	protected void write(MammaOnderzoek onderzoek)
	{
		aantalContextOphogen(MammaDense2Constants.AANTAL_ONDERZOEKEN_DENSITEIT_VERWIJDERD);
		dense2Service.verwijderDensiteit(onderzoek);
	}
}
