package nl.rivm.screenit.batch.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-alg
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

import java.util.List;
import java.util.Optional;

import javax.transaction.Transactional;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.batch.service.GbaVraagService;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.RedenGbaVraag;
import nl.rivm.screenit.model.enums.GbaStatus;
import nl.rivm.screenit.model.enums.GbaVraagType;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.enums.RedenIntrekkenGbaIndicatie;
import nl.rivm.screenit.model.gba.GbaVraag;
import nl.rivm.screenit.service.BaseGbaVraagService;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.LogService;
import nl.topicuszorg.gba.vertrouwdverbonden.model.Vo107Bericht;
import nl.topicuszorg.gba.vertrouwdverbonden.model.enums.Vo107_ArecordVeld;

import org.apache.commons.lang.StringUtils;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
@Slf4j
public class GbaVraagServiceImpl implements GbaVraagService
{
	private final BaseGbaVraagService baseGbaVraagService;

	private final ClientService clientService;

	private final LogService logService;

	@Override
	@Transactional
	public void verwerkNullBericht(String bsn, Client client, Vo107Bericht bericht)
	{

		var comm = bericht.getString(Vo107_ArecordVeld.COMM);
		if (comm.equals("RNI-AV01") || comm.equals("OVL-AV01"))
		{
			LOG.info("Bericht niet verwerkt, null-bericht o.b.v. door T&T gegenereerd AV01 bericht bij emigratie of overlijden. bericht EREF: '{}', Reden '{}'",
				bericht.getString(Vo107_ArecordVeld.EREF), comm);
			return;
		}

		var openstaandeVraag = openstaandeGbaVraag(bsn, client);

		markeerReactieOntvangen(openstaandeVraag);

		if (client == null && openstaandeVraag.isEmpty())
		{
			LOG.warn("Bericht niet verwerkt, onverwacht null-bericht voor onbekende client bericht EREF: {}", bericht.getString(Vo107_ArecordVeld.EREF));
		}
		else if (client == null)
		{
			verzoekPlaatsIndicatieNaNullBericht(bsn, null, openstaandeVraag);
		}
		else if (client.getGbaStatus() != GbaStatus.BEZWAAR && client.getRedenIntrekkenGbaIndicatieDoorBvo() == RedenIntrekkenGbaIndicatie.NIET_INGETROKKEN)
		{
			verzoekPlaatsIndicatieNaNullBericht(bsn, client, openstaandeVraag);
			client.setGbaStatus(GbaStatus.INDICATIE_AANGEVRAAGD);
		}
	}

	private Optional<GbaVraag> openstaandeGbaVraag(String bsn, Client client)
	{
		return baseGbaVraagService.findLaatsteGbaVraag(bsn, client).filter(v -> !v.isReactieOntvangen());
	}

	private void markeerReactieOntvangen(Optional<GbaVraag> gbaVraag)
	{
		gbaVraag.ifPresent(v -> v.setReactieOntvangen(true));
	}

	private void verzoekPlaatsIndicatieNaNullBericht(String bsn, Client client, Optional<GbaVraag> openstaandeVraag)
	{
		var plaatsReden = openstaandeVraag.map(this::bepaalPlaatsRedenOpBasisVanOpenstaandeVraag).orElse(RedenGbaVraag.ONVERWACHT_INDICATIE_VERWIJDERD);
		var aanvullendeInformatie = openstaandeVraag.map(GbaVraag::getAanvullendeInformatie).orElse(null);
		baseGbaVraagService.verzoekPlaatsIndicatie(bsn, client, plaatsReden, aanvullendeInformatie);
	}

	private RedenGbaVraag bepaalPlaatsRedenOpBasisVanOpenstaandeVraag(GbaVraag openstaandeVraag)
	{
		var openstaandeVraagReden = openstaandeVraag.getReden();

		if (openstaandeVraag.getVraagType() == GbaVraagType.PLAATS_INDICATIE)
		{
			return openstaandeVraagReden;
		}

		switch (openstaandeVraagReden)
		{
		case ONJUIST_ADRES:
		case ONJUISTE_PERSOONSGEGEVENS:
		case MUTATIEBERICHT_ONBEKENDE_CLIENT:
			return openstaandeVraagReden;
		case BEZWAAR:
			return RedenGbaVraag.BEZWAAR_INGETROKKEN;
		case AFGEMELD:
		case SELECTIEBLOKKADE:
			return RedenGbaVraag.AANGEMELD;
		case BOVENGRENS_LEEFTIJD:
			return RedenGbaVraag.ONVERWACHT_INDICATIE_VERWIJDERD;
		default:
			throw new IllegalStateException("Openstaande verwijdervraag met ongeldige reden: " + openstaandeVraagReden);
		}
	}

	@Override
	@Transactional
	public void onverwachtBerichtBijBezwaarBrp(Client client)
	{
		var openstaandeGbaVraag = openstaandeGbaVraag(client.getPersoon().getBsn(), client);
		markeerReactieOntvangen(openstaandeGbaVraag);
		baseGbaVraagService.verzoekVerwijderIndicatieBijBezwaarBrp(client);
	}

	@Override
	@Transactional
	public void gbaVraagAfrondenVoorMutatieOfVerstrekking(Client client, GbaStatus oudeGbaStatus, boolean persoonsGegevensGewijzigd, boolean adresGewijzigd)
	{
		var openstaandeGbaVraag = openstaandeGbaVraag(client.getPersoon().getBsn(), client);
		heropvraagCyclusAfronden(client, openstaandeGbaVraag, adresGewijzigd, persoonsGegevensGewijzigd);
		logAfrondenAanvraagPersoonsgegevens(client, oudeGbaStatus);
		markeerReactieOntvangen(openstaandeGbaVraag);
	}

	private void heropvraagCyclusAfronden(Client client, Optional<GbaVraag> openstaandeGbaVraag, boolean adresGewijzigd, boolean gegevensGewijzigd)
	{
		var redenOnjuistAdresOfPerfoonsgegevens =
			openstaandeGbaVraag.isPresent() && List.of(RedenGbaVraag.ONJUIST_ADRES, RedenGbaVraag.ONJUISTE_PERSOONSGEGEVENS).contains(openstaandeGbaVraag.get().getReden());

		if (client.getGbaStatus() == GbaStatus.INDICATIE_AANGEVRAAGD && redenOnjuistAdresOfPerfoonsgegevens)
		{
			var kanVersturenMetTijdelijkAdres = false;
			var aanvullendeInformatie = openstaandeGbaVraag.get().getAanvullendeInformatie();
			if (StringUtils.contains(aanvullendeInformatie, "|" + Constants.GBA_CHECK_ON_TIJDELIJK_ADRES_NU_ACTUEEL + "|"))
			{
				kanVersturenMetTijdelijkAdres = clientService.isTijdelijkeAdresNuActueel(client.getPersoon());
				if (adresGewijzigd)
				{
					aanvullendeInformatie = "|" + Constants.GBA_ADRES_GEGEVENS_GEWIJZIGD + aanvullendeInformatie;
				}
				client.getLaatsteGbaMutatie().setAanvullendeInformatie(aanvullendeInformatie);
			}

			var reden = openstaandeGbaVraag.get().getReden();
			if (reden == RedenGbaVraag.ONJUISTE_PERSOONSGEGEVENS && !gegevensGewijzigd)
			{
				logService.logGebeurtenis(LogGebeurtenis.GBA_PERSOONSGEGEVENS_NIET_GEWIJZIGD, clientService.getScreeningOrganisatieVan(client), client);
			}
			else if (reden == RedenGbaVraag.ONJUIST_ADRES && !adresGewijzigd && !kanVersturenMetTijdelijkAdres)
			{
				logService.logGebeurtenis(LogGebeurtenis.GBA_ADRES_NIET_GEWIJZIGD, clientService.getScreeningOrganisatieVan(client), client);
			}
		}
	}

	private void logAfrondenAanvraagPersoonsgegevens(Client client, GbaStatus oudeStatus)
	{
		if (oudeStatus == GbaStatus.INDICATIE_AANGEVRAAGD || oudeStatus == GbaStatus.INDICATIE_VERWIJDERD)
		{
			logService.logGebeurtenis(LogGebeurtenis.GBA_PERSOONSGEGEVENS_NIEUW_AANVRAGEN_AFGEROND, clientService.getScreeningOrganisatieVan(client), client);
		}
	}
}
