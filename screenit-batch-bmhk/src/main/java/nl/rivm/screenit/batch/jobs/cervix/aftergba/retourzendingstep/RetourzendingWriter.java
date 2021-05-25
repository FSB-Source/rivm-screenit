package nl.rivm.screenit.batch.jobs.cervix.aftergba.retourzendingstep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.batch.jobs.aftergba.retourzendingstep.BaseRetourzendingWriter;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.cervix.CervixDossier;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde;
import nl.rivm.screenit.model.cervix.CervixUitnodiging;
import nl.rivm.screenit.service.cervix.CervixBaseScreeningrondeService;
import nl.rivm.screenit.service.cervix.CervixFactory;

import org.springframework.beans.factory.annotation.Autowired;

public class RetourzendingWriter extends BaseRetourzendingWriter<Client, CervixUitnodiging, CervixScreeningRonde, CervixDossier>
{

	@Autowired
	private CervixBaseScreeningrondeService screeningrondeService;

	@Autowired
	private CervixFactory factory;

	@Override
	protected CervixUitnodiging maakNieuweUitnodiging(CervixUitnodiging uitnoding)
	{
		return factory.maakZasUitnodiging(uitnoding.getScreeningRonde().getDossier().getClient(), null, false, false);
	}

	@Override
	protected boolean nieuwUitnodigingNodig(CervixUitnodiging uitnodiging)
	{
		return !screeningrondeService.heeftUitslagOfHeeftGehad(uitnodiging);
	}

	@Override
	protected CervixDossier getDossier(Client client)
	{
		return client.getCervixDossier();
	}

}
