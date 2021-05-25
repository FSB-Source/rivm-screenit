package nl.rivm.screenit.batch.jobs.colon.vervolgintakeconclusie.versturennoshow;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-dk
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

import nl.rivm.screenit.batch.jobs.helpers.BaseWriter;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.MailMergeContext;
import nl.rivm.screenit.model.colon.ColonConclusie;
import nl.rivm.screenit.model.colon.ColonIntakeAfspraak;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.enums.HuisartsBerichtType;
import nl.rivm.screenit.service.colon.ColonHuisartsBerichtService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.topicuszorg.patientregistratie.persoonsgegevens.model.Persoon;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;

public class HuisartsNoShowWriter extends BaseWriter<ColonScreeningRonde>
{

	private static final Logger LOG = LoggerFactory.getLogger(HuisartsNoShowWriter.class);

	@Autowired
	private ColonHuisartsBerichtService huisartsBerichtService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Override
	protected void write(ColonScreeningRonde ronde) throws Exception
	{
		Client client = ronde.getDossier().getClient();
		ColonIntakeAfspraak colonIntakeAfspraak = ronde.getLaatsteAfspraak();
		ColonConclusie conclusie = colonIntakeAfspraak.getConclusie();
		MailMergeContext context = new MailMergeContext();
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
