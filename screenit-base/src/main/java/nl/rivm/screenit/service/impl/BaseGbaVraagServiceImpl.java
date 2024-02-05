package nl.rivm.screenit.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.util.Optional;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.RedenGbaVraag;
import nl.rivm.screenit.model.enums.GbaStatus;
import nl.rivm.screenit.model.enums.GbaVraagType;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.enums.RedenIntrekkenGbaIndicatie;
import nl.rivm.screenit.model.gba.GbaVraag;
import nl.rivm.screenit.repository.algemeen.GbaVraagRepository;
import nl.rivm.screenit.service.BaseGbaVraagService;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@RequiredArgsConstructor
@Slf4j
public class BaseGbaVraagServiceImpl implements BaseGbaVraagService
{
	private final GbaVraagRepository gbaVraagRepository;

	private final ICurrentDateSupplier currentDateSupplier;

	private final LogService logService;

	private final ClientService clientService;

	@Override
	public Optional<GbaVraag> findLaatsteGbaVraag(String bsn, Client client)
	{
		return gbaVraagRepository.findLaatsteGbaVraag(bsn, client);
	}

	@Override
	@Transactional
	public void vraagGbaGegevensOpnieuwAan(Client client, Account account, RedenGbaVraag reden)
	{
		maakGbaVraag(GbaVraagType.VERWIJDER_INDICATIE, null, client, reden, null);
		client.setGbaStatus(GbaStatus.INDICATIE_VERWIJDERD);
		logGebeurtenisGbaGegevensOpnieuwAangevraagd(client, account, reden);
	}

	private void logGebeurtenisGbaGegevensOpnieuwAangevraagd(Client client, Account account, RedenGbaVraag reden)
	{
		logService.logGebeurtenis(LogGebeurtenis.GBA_GEGEVENS_OPNIEUW_AANGEVRAAGD, clientService.getScreeningOrganisatieVan(client), account, client, "Reden: " + reden.getNaam());
	}

	@Override
	@Transactional
	public void vraagGbaGegevensOpnieuwAanDoorRetourzending(Client client, String aanvullendeInformatie)
	{
		maakGbaVraag(GbaVraagType.VERWIJDER_INDICATIE, null, client, RedenGbaVraag.ONJUIST_ADRES, aanvullendeInformatie);
		client.setGbaStatus(GbaStatus.INDICATIE_VERWIJDERD);
		client.setRedenIntrekkenGbaIndicatieDoorBvo(RedenIntrekkenGbaIndicatie.NIET_INGETROKKEN);
		logService.logGebeurtenis(LogGebeurtenis.GBA_GEGEVENS_OPNIEUW_AANGEVRAAGD, clientService.getScreeningOrganisatieVan(client), null, client, "Reden: Retourzending");
	}

	@Override
	@Transactional
	public void verzoekPlaatsIndicatie(String bsn, Client client, RedenGbaVraag reden, String aanvullendeInformatie)
	{
		maakGbaVraag(GbaVraagType.PLAATS_INDICATIE, bsn, client, reden, aanvullendeInformatie);
	}

	@Override
	@Transactional
	public void verzoekVerwijderIndicatieOnbekendeClient(String bsn)
	{
		maakGbaVraag(GbaVraagType.VERWIJDER_INDICATIE, bsn, null, RedenGbaVraag.MUTATIEBERICHT_ONBEKENDE_CLIENT, null);
	}

	@Override
	@Transactional
	public void verzoekVerwijderIndicatieBijBezwaarBrp(Client client)
	{
		maakGbaVraag(GbaVraagType.VERWIJDER_INDICATIE, null, client, RedenGbaVraag.BEZWAAR, null);
	}

	@Override
	@Transactional
	public void verzoekPlaatsIndicatieBijIntrekkenBezwaarBrp(Client client, Account account)
	{
		var gbaVraagReden = RedenGbaVraag.BEZWAAR_INGETROKKEN;
		maakGbaVraag(GbaVraagType.PLAATS_INDICATIE, null, client, gbaVraagReden, null);
		client.setGbaStatus(GbaStatus.INDICATIE_AANGEVRAAGD);
		logGebeurtenisGbaGegevensOpnieuwAangevraagd(client, account, gbaVraagReden);
	}

	private void maakGbaVraag(GbaVraagType vraagType, String bsn, Client client, RedenGbaVraag reden, String aanvullendeInformatie)
	{
		if (heeftOpenstaandeGbaVraag(bsn, client))
		{

			LOG.info("Nieuwe GbaVraag({}, {}) voor client '{}' niet gemaakt i.v.m. nog openstaande vraag",
				vraagType, reden.name(), client != null ? client.getId() : "Onbekend");
			return;
		}

		var gbaVraag = new GbaVraag();
		gbaVraag.setClient(client);
		gbaVraag.setBsn(bsn);
		gbaVraag.setDatum(currentDateSupplier.getLocalDateTime());
		gbaVraag.setVraagType(vraagType);
		gbaVraag.setReactieOntvangen(false);
		gbaVraag.setVerstuurd(false);
		gbaVraag.setReden(reden);
		gbaVraag.setAanvullendeInformatie(aanvullendeInformatie);

		gbaVraagRepository.save(gbaVraag);
	}

	private boolean heeftOpenstaandeGbaVraag(String bsn, Client client)
	{
		var laatsteGbaVraag = findLaatsteGbaVraag(bsn, client);
		return laatsteGbaVraag.isPresent() && !laatsteGbaVraag.get().isReactieOntvangen();
	}
}
