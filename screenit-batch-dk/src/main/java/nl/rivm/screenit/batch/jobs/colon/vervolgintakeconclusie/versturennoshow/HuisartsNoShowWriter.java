package nl.rivm.screenit.batch.jobs.colon.vervolgintakeconclusie.versturennoshow;

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
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.batch.jobs.helpers.BaseWriter;
import nl.rivm.screenit.model.MailMergeContext;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.enums.HuisartsBerichtType;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.colon.ColonHuisartsBerichtService;

import org.springframework.stereotype.Component;

@Component
@Slf4j
@AllArgsConstructor
public class HuisartsNoShowWriter extends BaseWriter<ColonScreeningRonde>
{

	private final ColonHuisartsBerichtService huisartsBerichtService;

	private final ICurrentDateSupplier currentDateSupplier;

	@Override
	protected void write(ColonScreeningRonde ronde) throws Exception
	{
		var client = ronde.getDossier().getClient();
		var colonIntakeAfspraak = ronde.getLaatsteAfspraak();
		var conclusie = colonIntakeAfspraak.getConclusie();
		var context = new MailMergeContext();
		context.setClient(client);
		context.setIntakeAfspraak(colonIntakeAfspraak);
		if (ronde.getLaatsteUitnodiging() != null)
		{
			context.setColonUitnodiging(ronde.getLaatsteUitnodiging());
		}

		huisartsBerichtService.verstuurColonHuisartsBericht(client, ronde, HuisartsBerichtType.NO_SHOW_INTAKE, context);

		LOG.info("Edi bericht NO_SHOW verstuurd voor client(id: " + client.getId() + ")");
		conclusie.setNoShowBericht(currentDateSupplier.getDate());

		getHibernateService().saveOrUpdate(conclusie);

	}
}
