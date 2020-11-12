package nl.rivm.screenit.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.jms.Destination;

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.helper.ActiveMQHelper;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.service.BerichtToSeRestBkService;
import nl.rivm.screenit.service.ICurrentDateSupplier;

import org.apache.commons.lang.time.DateUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.jms.core.JmsTemplate;
import org.springframework.stereotype.Service;

@Service(value = "berichtToSeRestBkService")
public class BerichtToSeRestBkServiceImpl implements BerichtToSeRestBkService
{

	private static final Logger LOG = LoggerFactory.getLogger(BerichtToSeRestBkServiceImpl.class);

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

	@Override
	public Set<MammaScreeningsEenheid> notificeerSesEnGeefSesTerug(Client client)
	{
		return notificeerSesMetUitzonderingVanEnGeefSesTerug(new HashSet<>(), client);
	}

	@Override
	public void notificeerSesMetUitzonderingVan(Set<MammaScreeningsEenheid> genotificeerdeSes, Client client)
	{
		notificeerSesMetUitzonderingVanEnGeefSesTerug(genotificeerdeSes, client);
	}

	private Set<MammaScreeningsEenheid> notificeerSesMetUitzonderingVanEnGeefSesTerug(Set<MammaScreeningsEenheid> genotificeerdeSes, Client client)
	{

		Date vandaag = currentDateSupplier.getDate();
		Set<MammaScreeningsEenheid> updateEenheden = new HashSet<>();
		List<MammaAfspraak> afspraken = client.getMammaDossier().getLaatsteScreeningRonde().getLaatsteUitnodiging().getAfspraken();
		for (MammaAfspraak afspraak : afspraken)
		{
			if (DateUtils.isSameDay(vandaag, afspraak.getVanaf()))
			{
				updateEenheden.add(afspraak.getStandplaatsPeriode().getScreeningsEenheid());
			}
		}

		updateEenheden.removeAll(genotificeerdeSes);
		updateEenheden.forEach(se -> queueBericht(mammaSeRestDestination, se.getCode()));
		return updateEenheden;
	}

	@Override
	public void notificeerSe(MammaScreeningsEenheid se)
	{
		queueBericht(mammaSeRestDestination, se.getCode());
	}

	@Override
	public void updateTijdVoorIedereSe(String durationOffset)
	{
		if (Boolean.TRUE.equals(testModus))
		{
			queueBericht(mammaSeRestDestination, durationOffset);
		}
	}

	private void queueBericht(Destination destination, final String bericht)
	{
		jmsTemplate.send(destination, session -> ActiveMQHelper.getActiveMqObjectMessage(bericht));
	}
}
