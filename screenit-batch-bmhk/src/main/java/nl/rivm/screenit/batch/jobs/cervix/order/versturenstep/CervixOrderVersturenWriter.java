package nl.rivm.screenit.batch.jobs.cervix.order.versturenstep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
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

import java.util.ArrayList;
import java.util.List;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.batch.jobs.cervix.order.CervixOrderConstants;
import nl.rivm.screenit.batch.jobs.helpers.BaseWriter;
import nl.rivm.screenit.batch.service.CervixHL7BaseService;
import nl.rivm.screenit.model.BMHKLaboratorium;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.OrganisatieParameterKey;
import nl.rivm.screenit.model.Rivm;
import nl.rivm.screenit.model.cervix.CervixCytologieOrder;
import nl.rivm.screenit.model.cervix.enums.CervixCytologieOrderStatus;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.stereotype.Component;

@Component
@AllArgsConstructor
@Slf4j
public class CervixOrderVersturenWriter extends BaseWriter<CervixCytologieOrder>
{

	private final LogService logService;

	private final HibernateService hibernateService;

	private final ICurrentDateSupplier dateSupplier;

	private final CervixHL7BaseService hl7BaseService;

	@Override
	protected void write(CervixCytologieOrder cytologieOrder) throws Exception
	{
		LOG.info("Order bericht wordt verstuurd voor cytologieOrder:" + cytologieOrder.getId());

		BMHKLaboratorium laboratorium = cytologieOrder.getUitstrijkje().getLaboratorium();
		var responseWrapper = hl7BaseService.sendHL7Message(cytologieOrder.getHl7Bericht(), laboratorium, OrganisatieParameterKey.CERVIX_CYTOLOGIE_ORDER_HOST,
			OrganisatieParameterKey.CERVIX_CYTOLOGIE_ORDER_PORT);

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
		var client = cytologieOrder.getUitstrijkje().getOntvangstScreeningRonde().getDossier().getClient();
		logService.logGebeurtenis(LogGebeurtenis.CERVIX_ORDER_VERSTUREN_MISLUKT, instellingen, client, melding, Bevolkingsonderzoek.CERVIX);
	}
}
