package nl.rivm.screenit.batch.jobs.mamma.onderzoek;

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

import java.util.Collections;
import java.util.List;

import nl.rivm.screenit.batch.jobs.helpers.BaseWriter;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.service.LogService;

import org.springframework.beans.factory.annotation.Autowired;

public abstract class MammaOnderzoekenZonderBeelenOfNietAfgeslotenBaseWriter extends BaseWriter<MammaAfspraak>
{

	@Autowired
	private LogService logService;

	private final LogGebeurtenis logGebeurtenis;

	protected MammaOnderzoekenZonderBeelenOfNietAfgeslotenBaseWriter(LogGebeurtenis logGebeurtenis)
	{
		this.logGebeurtenis = logGebeurtenis;
	}

	@Override
	protected void write(MammaAfspraak afspraak)
	{
		var screeningRonde = afspraak.getUitnodiging().getScreeningRonde();
		var melding = "UitnodigingsNr: " + screeningRonde.getUitnodigingsNr();
		var client = screeningRonde.getDossier().getClient();
		var screeningsEenheid = afspraak.getStandplaatsPeriode().getScreeningsEenheid();
		List<Instelling> dashboardOrganisaties = Collections.singletonList(client.getPersoon().getGbaAdres().getGbaGemeente().getScreeningOrganisatie());

		logService.logGebeurtenis(logGebeurtenis, screeningsEenheid, dashboardOrganisaties, client, melding);
	}
}
