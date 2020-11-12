package nl.rivm.screenit.batch.jobs.mamma.aftergba.afsprakenAnnuleren;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bk
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

import nl.rivm.screenit.batch.jobs.helpers.BaseWriter;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.enums.MammaAfspraakStatus;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.mamma.MammaBaseAfspraakService;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;

public class MammaAfsprakenAnnulerenWriter extends BaseWriter<Client>
{
	private static final Logger LOGGER = LoggerFactory.getLogger(MammaAfsprakenAnnulerenWriter.class);

	@Autowired
	private MammaBaseAfspraakService afspraakService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private LogService logService;

	@Override
	protected void write(Client client) throws Exception
	{
		MammaAfspraak laatsteAfspraak = client.getMammaDossier().getLaatsteScreeningRonde().getLaatsteUitnodiging().getLaatsteAfspraak();
		if (MammaAfspraakStatus.GEPLAND.equals(laatsteAfspraak.getStatus()))
		{
			MammaAfspraakStatus reden = MammaAfspraakStatus.GEANNULEERD_VERHUIZING_BUITENLAND;
			if (client.getPersoon().getOverlijdensdatum() != null)
			{
				reden = MammaAfspraakStatus.GEANNULEERD_OVERLIJDEN;
			}
			afspraakService.afspraakAnnuleren(laatsteAfspraak, reden, currentDateSupplier.getDate());
			LOGGER.info("Afspraak afzeggen voor client {}, met afspraak id {}, en reden {}", client.getId(), laatsteAfspraak.getId(), reden.name());
			logService.logGebeurtenis(LogGebeurtenis.MAMMA_AFSPRAAK_GEANNULEERD, new LogEvent("Afspraak automatisch afgezegd met reden " + reden.name()),
				Bevolkingsonderzoek.MAMMA);
		}
	}
}
