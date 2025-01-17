package nl.rivm.screenit.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.jms.Destination;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.helper.ActiveMQHelper;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.service.BerichtToSeRestBkService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.websocket.WebsocketBerichtType;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.jms.core.JmsTemplate;
import org.springframework.stereotype.Service;

import com.google.common.collect.Range;

@Service(value = "berichtToSeRestBkService")
public class BerichtToSeRestBkServiceImpl implements BerichtToSeRestBkService
{

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private JmsTemplate jmsTemplate;

	@Autowired
	@Qualifier("testModus")
	private Boolean testModus;

	@Autowired
	@Qualifier("verwerkMammaSeRestDestination")
	private Destination mammaSeRestDestination;

	@Autowired
	private SimplePreferenceService preferenceService;

	@Override
	public void notificeerScreeningsEenhedenVerversenDaglijst(Client client)
	{
		Map<MammaScreeningsEenheid, HashSet<LocalDate>> updateEenheden = new HashMap<>();
		List<MammaAfspraak> afspraken = client.getMammaDossier().getLaatsteScreeningRonde().getLaatsteUitnodiging().getAfspraken();
		for (MammaAfspraak afspraak : afspraken)
		{
			LocalDate afspraakDatum = DateUtil.toLocalDate(afspraak.getVanaf());
			HashSet<LocalDate> updateDates = updateEenheden.computeIfAbsent(afspraak.getStandplaatsPeriode().getScreeningsEenheid(), k -> new HashSet<>());
			updateDates.add(afspraakDatum);
		}
		LocalDate daglijstNotificerenTotEnMet = getDaglijstNotificerenTotEnMet();
		updateEenheden.forEach((se, datums) -> notificeerScreeningsEenheidVerversenDaglijst(se, datums, daglijstNotificerenTotEnMet));
	}

	@Override
	public void notificeerScreeningsEenheidVerversenDaglijst(MammaScreeningsEenheid se, Set<LocalDate> updateDatums)
	{
		notificeerScreeningsEenheidVerversenDaglijst(se, updateDatums, getDaglijstNotificerenTotEnMet());
	}

	@Override
	public void notificeerScreeningsEenheidVerversenDaglijst(MammaScreeningsEenheid se, Set<LocalDate> updateDatums, LocalDate daglijstNotificerenTotEnMet)
	{
		LocalDate vandaag = currentDateSupplier.getLocalDate();
		updateDatums.stream()
			.filter(ud -> Range.closed(vandaag, daglijstNotificerenTotEnMet).contains(ud))
			.forEach(ud -> queueBericht(mammaSeRestDestination, se.getCode() + ":" + ud.format(DateTimeFormatter.ISO_DATE)));
	}

	@Override
	public void dbCleanupVoorIedereSe()
	{
		if (Boolean.TRUE.equals(testModus))
		{
			queueBericht(mammaSeRestDestination, WebsocketBerichtType.DB_CLEANUP.name());
		}
	}

	@Override
	public void statusAanvragenVoorIedereSe()
	{
		queueBericht(mammaSeRestDestination, WebsocketBerichtType.VERSTUUR_STATUS.name());
	}

	private LocalDate getDaglijstNotificerenTotEnMet()
	{
		int aantalDagenDaglijst = preferenceService.getInteger(PreferenceKey.MAMMA_SE_DAGLIJST_OPHALEN_DAGEN.name(), 0);
		LocalDate vandaag = currentDateSupplier.getLocalDate();
		return vandaag.plusDays(aantalDagenDaglijst + 1);
	}

	private void queueBericht(Destination destination, final String bericht)
	{
		jmsTemplate.send(destination, session -> ActiveMQHelper.getActiveMqObjectMessage(bericht));
	}
}
