package nl.rivm.screenit.batch.jobs.cervix.verrichtingen.dubbelecytologiestep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
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

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.batch.jobs.cervix.verrichtingen.CervixAbstractVerrichtingenWriter;
import nl.rivm.screenit.model.cervix.CervixUitstrijkje;

import org.springframework.stereotype.Component;

@Component
@Slf4j
public class CervixDubbeleCytologieVerrichtingenWriter extends CervixAbstractVerrichtingenWriter<CervixUitstrijkje>
{

	@Override
	protected void write(CervixUitstrijkje uitstrijkje) throws Exception
	{
		try
		{
			bepaalCytologieVerrichtingen(uitstrijkje);
		}
		catch (Exception e)
		{
			LOG.error("Fout bij bepalen van verrichting, monsterId: " + uitstrijkje.getMonsterId() + ", clientId "
				+ uitstrijkje.getUitnodiging().getScreeningRonde().getDossier().getClient().getId());
			throw e;
		}
	}
}
