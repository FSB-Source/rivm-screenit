package nl.rivm.screenit.batch.jobs.mamma.aftergba.nieuwepostcodes;

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

import java.util.List;

import lombok.AllArgsConstructor;

import nl.rivm.screenit.batch.jobs.helpers.BaseWriter;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.service.LogService;

import org.springframework.stereotype.Component;

@Component
@AllArgsConstructor
public class MammaNieuwePostcodesWriter extends BaseWriter<Client>
{
	private final LogService logService;

	@Override
	protected void write(Client client)
	{
		List<Instelling> dashboardInstellingen = List.of(client.getPersoon().getGbaAdres().getGbaGemeente().getScreeningOrganisatie());
		logService.logGebeurtenis(LogGebeurtenis.MAMMA_ONTBREKENDE_POSTCODEREEKS, dashboardInstellingen, null, client,
			"Postcode: " + client.getPersoon().getGbaAdres().getPostcode(),
			Bevolkingsonderzoek.MAMMA);
	}
}
