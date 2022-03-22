package nl.rivm.screenit.batch.jobs.cervix.hpvoru;

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

import java.io.IOException;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicInteger;

import nl.rivm.screenit.batch.jobs.helpers.BaseWriter;
import nl.rivm.screenit.batch.model.HL7v24ResponseWrapper;
import nl.rivm.screenit.batch.model.HapiContextType;
import nl.rivm.screenit.batch.model.ScreenITHL7MessageContext;
import nl.rivm.screenit.batch.service.CervixHL7BaseService;
import nl.rivm.screenit.batch.service.CervixHpvOruBerichtService;
import nl.rivm.screenit.batch.service.HL7BaseSendMessageService;
import nl.rivm.screenit.model.BMHKLaboratorium;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.Rivm;
import nl.rivm.screenit.model.cervix.CervixMonster;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;

import ca.uhn.hl7v2.HL7Exception;
import ca.uhn.hl7v2.app.Connection;
import ca.uhn.hl7v2.llp.LLPException;

import static nl.rivm.screenit.batch.jobs.cervix.hpvoru.CervixHpvOruBerichtenConstants.CERVIX_HPV_ORU_BERICHT_VERSTUURD;
import static nl.rivm.screenit.batch.jobs.cervix.hpvoru.CervixHpvOruBerichtenConstants.CERVIX_HPV_ORU_BERICHT_VERSTUURDEN_TIMEOUT;
import static nl.rivm.screenit.batch.jobs.cervix.hpvoru.CervixHpvOruBerichtenConstants.CERVIX_HPV_ORU_BERICHT_VERSTUURD_PER_LAB;
import static nl.rivm.screenit.batch.jobs.cervix.hpvoru.CervixHpvOruBerichtenConstants.KEY_LABORATORIUMID;

public class CervixHpvOruBerichtenWriter extends BaseWriter<CervixScreeningRonde>
{

	private static final Logger LOG = LoggerFactory.getLogger(CervixHpvOruBerichtenWriter.class);

	@Autowired
	private ICurrentDateSupplier dateSupplier;

	@Autowired
	private LogService logService;

	@Autowired
	private CervixHpvOruBerichtService hpvOruBerichtService;

	@Autowired
	private CervixHL7BaseService hl7BaseService;

	@Autowired
	private HL7BaseSendMessageService sendMessageService;

	@Override
	public void writeItems(List<? extends Long> items) throws Exception
	{
		BMHKLaboratorium laboratorium = getHibernateService().load(BMHKLaboratorium.class, getStepExecution().getExecutionContext().getLong(KEY_LABORATORIUMID));

		LocalDateTime limit = (LocalDateTime) getStepExecution().getExecutionContext().get(CERVIX_HPV_ORU_BERICHT_VERSTUURDEN_TIMEOUT);
		if (LocalDateTime.now().isBefore(limit))
		{
			ScreenITHL7MessageContext messageContext = new ScreenITHL7MessageContext(HapiContextType.UTF_8);
			Connection connection = hl7BaseService.openConnection(laboratorium, 3, messageContext);
			AtomicInteger verzonden = new AtomicInteger(0);

			items.stream().map(item -> getHibernateService().get(CervixScreeningRonde.class, item)).filter(Objects::nonNull).map(CervixScreeningRonde::getMonsterHpvUitslag)
				.forEach(monster -> {
					LOG.info("Oru bericht wordt verstuurd voor monster-id:" + monster.getMonsterId());

					if (!laboratorium.equals(monster.getLaboratorium()))
					{
						throw new IllegalArgumentException("Het geselecteerde laboratorium in de reader is anders dan het laboratorium waarmee de verbinding geopend is.");
					}
					if (connection.isOpen())
					{
						HL7v24ResponseWrapper berichtResponseWrapper = messageContext.getResponseWrapper();
						try
						{
							sendMessageService.sendHL7Message(hpvOruBerichtService.maakHpvOruBericht(monster).toString(), messageContext);
						}
						catch (HL7Exception | IOException | LLPException e)
						{
							LOG.error("Exceptie bij versturen bericht", e);
						}

						if (berichtResponseWrapper.isSuccess())
						{
							verstuurd(monster);
							verzonden.incrementAndGet();
						}
						else
						{
							StringBuilder sb = new StringBuilder("Bericht kon niet verzonden worden naar ");
							sb.append(laboratorium.getNaam());
							Optional.ofNullable(berichtResponseWrapper.getMelding()).ifPresent(fout -> sb.append(": ").append(fout));
							LOG.error(sb.toString(), berichtResponseWrapper.getCrashException());
							versturenMislukt(monster, sb.toString());
						}
					}
					else
					{
						String melding = "Bericht kon niet verzonden worden: verbinding met " + laboratorium.getNaam() + " is gesloten";
						LOG.error(melding);
						versturenMislukt(monster, melding);
					}
				});
			verhoogAantalVerzondenBerichten(laboratorium, verzonden.get());
			sendMessageService.discardConnection(messageContext);
		}
		else
		{
			LOG.error(String.format("%s berichten voor laboratorium: %s worden niet verzonden, timeout is in werking getreden", items.size(), laboratorium.getNaam()));
		}
	}

	@Override
	protected void write(CervixScreeningRonde item) throws Exception
	{

	}

	private void verstuurd(CervixMonster monster)
	{
		monster.setDatumOruVerstuurd(dateSupplier.getDate());
		getHibernateService().saveOrUpdate(monster);
		aantalContextOphogen(CERVIX_HPV_ORU_BERICHT_VERSTUURD);
	}

	private void versturenMislukt(CervixMonster monster, String melding)
	{
		List<Instelling> instellingen = new ArrayList<>();
		instellingen.add(getHibernateService().loadAll(Rivm.class).get(0));
		Client client = monster.getOntvangstScreeningRonde().getDossier().getClient();
		logService.logGebeurtenis(LogGebeurtenis.CERVIX_HPV_ORU_BERICHTEN_VERSTUREN_MISLUKT, instellingen, client, melding, Bevolkingsonderzoek.CERVIX);
	}

	private void verhoogAantalVerzondenBerichten(BMHKLaboratorium laboratorium, int value)
	{
		Map<Long, Integer> map = (Map<Long, Integer>) getExecutionContext().get(CERVIX_HPV_ORU_BERICHT_VERSTUURD_PER_LAB);
		if (map == null)
		{
			map = new HashMap<>();
			getExecutionContext().put(CERVIX_HPV_ORU_BERICHT_VERSTUURD_PER_LAB, map);
		}
		Integer currentValue = map.get(laboratorium.getId());
		if (currentValue == null)
		{
			currentValue = 0;
		}
		map.put(laboratorium.getId(), currentValue + value);
	}
}
