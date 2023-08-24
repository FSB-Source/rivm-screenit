package nl.rivm.screenit.batch.jobs.colon.aftergba.retourzendingstep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-dk
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

import lombok.AllArgsConstructor;

import nl.rivm.screenit.batch.jobs.aftergba.retourzendingstep.BaseRetourzendingWriter;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.colon.ColonDossier;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.colon.ColonUitnodiging;
import nl.rivm.screenit.service.colon.ColonScreeningsrondeService;
import nl.rivm.screenit.service.colon.ColonUitnodigingService;

import org.springframework.stereotype.Component;

@Component
@AllArgsConstructor
public class RetourzendingWriter extends BaseRetourzendingWriter<Client, ColonUitnodiging, ColonScreeningRonde, ColonDossier>
{

	private final ColonScreeningsrondeService screeningsrondeService;

	private final ColonUitnodigingService uitnodigingsService;

	@Override
	protected ColonUitnodiging maakNieuweUitnodiging(ColonUitnodiging uitnoding)
	{
		return uitnodigingsService.cloneUitnodiging(uitnoding, false);
	}

	@Override
	protected boolean nieuwUitnodigingNodig(ColonUitnodiging uitnodiging)
	{
		return !screeningsrondeService.heeftUitslag(uitnodiging, false);
	}

	@Override
	protected ColonDossier getDossier(Client client)
	{
		return client.getColonDossier();
	}

}
