package nl.rivm.screenit.batch.jobs.cervix.order.versturenstep;

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

import java.util.ArrayList;
import java.util.List;

import nl.rivm.screenit.batch.jobs.cervix.order.CervixOrderConstants;
import nl.rivm.screenit.batch.jobs.helpers.BaseWriter;
import nl.rivm.screenit.batch.model.HL7v24ResponseWrapper;
import nl.rivm.screenit.batch.service.CervixHL7BaseService;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.Rivm;
import nl.rivm.screenit.model.cervix.CervixCytologieOrder;
import nl.rivm.screenit.model.cervix.enums.CervixCytologieOrderStatus;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;

public class CervixOrderVersturenWriter extends BaseWriter<CervixCytologieOrder>
{

	private static final Logger LOG = LoggerFactory.getLogger(CervixOrderVersturenWriter.class);

	@Autowired
	private LogService logService;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private ICurrentDateSupplier dateSupplier;

	@Autowired
	private CervixHL7BaseService hl7BaseService;

	@Override
	protected void write(CervixCytologieOrder cytologieOrder) throws Exception
	{
		LOG.info("Order bericht wordt verstuurd voor cytologieOrder:" + cytologieOrder.getId());

		HL7v24ResponseWrapper responseWrapper = hl7BaseService.sendHL7Message(cytologieOrder.getHl7Bericht(), cytologieOrder.getUitstrijkje().getLaboratorium());

		if (responseWrapper.isSuccess())
		{
			verstuurd(cytologieOrder);
		}
		else
		{
			String melding = "Bericht kon niet verzonden worden: " + responseWrapper.getMelding();
			LOG.error(melding, responseWrapper.getCrashException());
			versturenMislukt(cytologieOrder, melding);
		}

	}

	private void verstuurd(CervixCytologieOrder cytologieOrder)
	{

		cytologieOrder.setStatus(CervixCytologieOrderStatus.VERSTUURD);
		cytologieOrder.setStatusDatum(dateSupplier.getDate());
		hibernateService.saveOrUpdate(cytologieOrder);

		aantalContextOphogen(CervixOrderConstants.KEY_ORDER_VERSTUURD);
	}

	private void versturenMislukt(CervixCytologieOrder cytologieOrder, String melding)
	{
		cytologieOrder.setStatus(CervixCytologieOrderStatus.MISLUKT);
		cytologieOrder.setStatusDatum(dateSupplier.getDate());
		hibernateService.saveOrUpdate(cytologieOrder);

		List<Instelling> instellingen = new ArrayList<>();
		instellingen.add(hibernateService.loadAll(Rivm.class).get(0));
		Client client = cytologieOrder.getUitstrijkje().getOntvangstScreeningRonde().getDossier().getClient();
		logService.logGebeurtenis(LogGebeurtenis.CERVIX_ORDER_VERSTUREN_MISLUKT, instellingen, client, melding, Bevolkingsonderzoek.CERVIX);
	}
}
