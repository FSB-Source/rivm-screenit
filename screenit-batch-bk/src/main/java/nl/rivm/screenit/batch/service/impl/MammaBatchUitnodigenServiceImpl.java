package nl.rivm.screenit.batch.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bk
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
import java.util.ArrayList;
import java.util.List;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.batch.model.MammaAfspraakWijzigenFilter;
import nl.rivm.screenit.batch.model.UitstelUitnodigingRaportageEntry;
import nl.rivm.screenit.batch.service.MammaBatchUitnodigenService;
import nl.rivm.screenit.dto.mamma.afspraken.MammaKandidaatAfspraakDto;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Rivm;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.mamma.MammaCapaciteitBlok;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode;
import nl.rivm.screenit.model.mamma.MammaStandplaatsRonde;
import nl.rivm.screenit.model.mamma.MammaUitnodiging;
import nl.rivm.screenit.model.mamma.MammaUitstel;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.InstellingService;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.mamma.MammaBaseAfspraakService;
import nl.rivm.screenit.service.mamma.MammaBaseFactory;
import nl.rivm.screenit.service.mamma.MammaBaseKansberekeningService;
import nl.rivm.screenit.service.mamma.MammaBaseScreeningrondeService;
import nl.rivm.screenit.service.mamma.MammaBaseStandplaatsService;
import nl.rivm.screenit.service.mamma.MammaVolgendeUitnodigingService;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Slf4j
@Service
@Transactional(propagation = Propagation.REQUIRED)
@AllArgsConstructor
public class MammaBatchUitnodigenServiceImpl implements MammaBatchUitnodigenService
{
	private final HibernateService hibernateService;

	private final MammaBaseStandplaatsService standplaatsService;

	private final MammaBaseFactory baseFactory;

	private final MammaBaseScreeningrondeService screeningrondeService;

	private final LogService logService;

	private final InstellingService instellingService;

	private final ClientService clientService;

	private final MammaBaseAfspraakService afspraakService;

	private final MammaBaseKansberekeningService baseKansberekeningService;

	private final MammaVolgendeUitnodigingService volgendeUitnodigingService;

	@Override
	public MammaUitnodiging maakNieuweRondeEnIntervalUitnodiging(Client client)
	{
		var standplaatsPeriode = bepaalStandplaatsPeriodeViaPostcode(client, "Interval");
		if (standplaatsPeriode != null)
		{
			var ronde = baseFactory.maakRonde(client.getMammaDossier(), standplaatsPeriode.getStandplaatsRonde(), false);
			var uitnodiging = maakOpenUitnodiging(ronde, ronde.getStandplaatsRonde());
			baseKansberekeningService.dossierEventHerzien(client.getMammaDossier());
			return uitnodiging;
		}
		return null;
	}

	private MammaStandplaatsPeriode bepaalStandplaatsPeriodeViaPostcode(Client client, String uitnodigingsreden)
	{
		var standplaats = standplaatsService.getStandplaatsMetPostcode(client);
		if (standplaats == null)
		{
			var dashboardIntstellingen = new ArrayList<>(clientService.getScreeningOrganisatieVan(client));
			dashboardIntstellingen.add(instellingService.getActieveInstellingen(Rivm.class).get(0));
			logService.logGebeurtenis(LogGebeurtenis.MAMMA_UITNODIGEN_FOUT, dashboardIntstellingen, null, client,
				uitnodigingsreden + "uitnodiging mislukt: geen standplaats gekoppeld aan postcode van client", Bevolkingsonderzoek.MAMMA);
			return null;
		}

		var standplaatsPeriode = standplaatsService.huidigeStandplaatsPeriodeInRouteVanStandplaats(standplaats);
		if (standplaatsPeriode == null)
		{
			var dashboardIntstellingen = new ArrayList<>(List.of(standplaats.getRegio(), instellingService.getActieveInstellingen(Rivm.class).get(0)));
			var melding = String.format("%suitnodiging mislukt: standplaats '%s' niet gevonden in een route", uitnodigingsreden, standplaats.getNaam());
			logService.logGebeurtenis(LogGebeurtenis.MAMMA_UITNODIGEN_FOUT, dashboardIntstellingen, null, client, melding, Bevolkingsonderzoek.MAMMA);
			return null;
		}

		return standplaatsPeriode;
	}

	private MammaUitnodiging maakOpenUitnodiging(MammaScreeningRonde screeningRonde, MammaStandplaatsRonde standplaatsRonde)
	{
		var dossier = screeningRonde.getDossier();
		var briefType = screeningrondeService.bepaalBriefTypeVoorOpenUitnodiging(volgendeUitnodigingService.isSuspect(dossier), dossier.getDoelgroep());
		return baseFactory.maakUitnodiging(screeningRonde, standplaatsRonde, briefType);
	}

	@Override
	public UitstelUitnodigingRaportageEntry maakUitnodigingVoorClientContactUitstel(MammaUitstel uitstel)
	{
		var client = clientVanUitstel(uitstel);
		var streefDatum = DateUtil.toLocalDate(uitstel.getStreefDatum());
		var huidigeStandplaatsPeriodeInRouteVanStandplaats = standplaatsService.huidigeStandplaatsPeriodeInRouteVanStandplaats(uitstel.getStandplaats());
		if (huidigeStandplaatsPeriodeInRouteVanStandplaats == null)
		{
			LOG.info("Uitstelstandplaats-id: '{}' voor client-id '{}' niet gevonden in een route -> geef open uitnodiging o.b.v. postcode", uitstel.getStandplaats().getId(),
				client.getId());
			return maakOpenUitnodigingViaPostcodeClient(uitstel);
		}

		var screeningsEenheid = huidigeStandplaatsPeriodeInRouteVanStandplaats.getScreeningsEenheid();

		if (streefDatumTeDichtbij(streefDatum))
		{
			LOG.info("Streefdatum '{}' voor client-id '{}' niet ver genoeg in toekomst -> geef open uitnodiging", streefDatum, client.getId());
			return maakOpenUitnodigingVoorUitstel(uitstel, huidigeStandplaatsPeriodeInRouteVanStandplaats);
		}

		var kandidaatAfspraken = afspraakService.getKandidaatAfspraken(client, MammaAfspraakWijzigenFilter.opStreefDatum(uitstel, screeningsEenheid));
		if (kandidaatAfspraken.isEmpty())
		{
			LOG.info("Geen afspraakmogelijkheid gevonden voor client-id '{}' met uitstelstandplaats-id '{}' op streefdatum '{}' in route van se-id '{}' "
				+ "-> geef open uitnodiging o.b.v. gevonden SE", client.getId(), uitstel.getStandplaats().getId(), streefDatum, screeningsEenheid.getId());
			return maakOpenUitnodigingVoorUitstel(uitstel, huidigeStandplaatsPeriodeInRouteVanStandplaats);
		}

		return maakUitnodigingMetAfspraak(uitstel, kandidaatAfspraken.get(0));
	}

	private boolean streefDatumTeDichtbij(LocalDate streefDatum)
	{
		return streefDatum.isBefore(afspraakService.getMinimaleAfspraakDatumBijUitnodigen());
	}

	private UitstelUitnodigingRaportageEntry maakOpenUitnodigingViaPostcodeClient(MammaUitstel uitstel)
	{
		var standplaatsPeriodeViaPostcode = bepaalStandplaatsPeriodeViaPostcode(clientVanUitstel(uitstel), "Uitstel");
		if (standplaatsPeriodeViaPostcode != null)
		{
			var uitnodiging = maakOpenUitnodiging(uitstel.getScreeningRonde(), standplaatsPeriodeViaPostcode.getStandplaatsRonde());
			koppelUitstelAanUitnodiging(uitstel, uitnodiging);
			return UitstelUitnodigingRaportageEntry.maak(standplaatsPeriodeViaPostcode, uitnodiging);
		}
		return null;
	}

	private UitstelUitnodigingRaportageEntry maakOpenUitnodigingVoorUitstel(MammaUitstel uitstel, MammaStandplaatsPeriode standplaatsPeriode)
	{
		var uitnodiging = maakOpenUitnodiging(uitstel.getScreeningRonde(), standplaatsPeriode.getStandplaatsRonde());
		koppelUitstelAanUitnodiging(uitstel, uitnodiging);
		return UitstelUitnodigingRaportageEntry.maak(standplaatsPeriode, uitnodiging);
	}

	private UitstelUitnodigingRaportageEntry maakUitnodigingMetAfspraak(MammaUitstel uitstel, MammaKandidaatAfspraakDto kandidaatAfspraak)
	{
		var standplaatsPeriode = hibernateService.load(MammaStandplaatsPeriode.class, kandidaatAfspraak.getStandplaatsPeriodeId());
		maakAfspraakUitnodiging(uitstel, standplaatsPeriode.getStandplaatsRonde());

		var capaciteitBlok = hibernateService.load(MammaCapaciteitBlok.class, kandidaatAfspraak.getCapaciteitBlokId());
		afspraakService.maakAfspraak(uitstel.getScreeningRonde(), capaciteitBlok, DateUtil.toUtilDate(kandidaatAfspraak.getDatumTijd()),
			standplaatsPeriode, null, false, true, false, true, false, null, false);

		return UitstelUitnodigingRaportageEntry.maak(standplaatsPeriode, uitstel.getUitnodiging());
	}

	private void maakAfspraakUitnodiging(MammaUitstel uitstel, MammaStandplaatsRonde standplaatsRonde)
	{
		var uitnodiging = baseFactory.maakUitnodiging(uitstel.getScreeningRonde(), standplaatsRonde, BriefType.MAMMA_AFSPRAAK_UITNODIGING);
		koppelUitstelAanUitnodiging(uitstel, uitnodiging);
	}

	private void koppelUitstelAanUitnodiging(MammaUitstel uitstel, MammaUitnodiging uitnodiging)
	{
		uitstel.setUitnodiging(uitnodiging);
		hibernateService.saveOrUpdate(uitstel);
		baseKansberekeningService.dossierEventHerzien(uitstel.getScreeningRonde().getDossier());
	}

	private static Client clientVanUitstel(MammaUitstel uitstel)
	{
		return uitstel.getScreeningRonde().getDossier().getClient();
	}
}
