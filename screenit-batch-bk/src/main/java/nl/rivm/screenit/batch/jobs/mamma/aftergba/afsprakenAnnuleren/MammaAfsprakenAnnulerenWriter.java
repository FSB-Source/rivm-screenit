package nl.rivm.screenit.batch.jobs.mamma.aftergba.afsprakenannuleren;

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

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.batch.jobs.helpers.BaseWriter;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.rivm.screenit.model.mamma.enums.MammaAfspraakStatus;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.mamma.MammaBaseAfspraakService;

import org.springframework.stereotype.Component;

@Component
@Slf4j
@AllArgsConstructor
public class MammaAfsprakenAnnulerenWriter extends BaseWriter<Client>
{

	private final MammaBaseAfspraakService afspraakService;

	private final ICurrentDateSupplier currentDateSupplier;

	private final LogService logService;

	@Override
	protected void write(Client client) throws Exception
	{
		var laatsteAfspraak = client.getMammaDossier().getLaatsteScreeningRonde().getLaatsteUitnodiging().getLaatsteAfspraak();
		if (MammaAfspraakStatus.GEPLAND.equals(laatsteAfspraak.getStatus()))
		{
			var reden = MammaAfspraakStatus.GEANNULEERD_VERHUIZING_BUITENLAND;
			if (client.getPersoon().getOverlijdensdatum() != null)
			{
				reden = MammaAfspraakStatus.GEANNULEERD_OVERLIJDEN;
			}
			afspraakService.afspraakAnnuleren(laatsteAfspraak, reden, currentDateSupplier.getDate());
			LOG.info("Afspraak afzeggen voor client {}, met afspraak id {}, en reden {}", client.getId(), laatsteAfspraak.getId(), reden.name());
			logService.logGebeurtenis(LogGebeurtenis.MAMMA_AFSPRAAK_GEANNULEERD, new LogEvent("Afspraak automatisch afgezegd met reden " + reden.name()),
				Bevolkingsonderzoek.MAMMA);
		}
	}
}
