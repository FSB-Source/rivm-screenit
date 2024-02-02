package nl.rivm.screenit.clientportaal.services.colon.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal
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

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.Locale;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.clientportaal.model.colon.ColonVolgendeUitnodigingTekstType;
import nl.rivm.screenit.clientportaal.services.colon.ColonAfspraakService;
import nl.rivm.screenit.clientportaal.services.colon.ColonVolgendeUitnodigingDatumTekstService;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.DossierStatus;
import nl.rivm.screenit.model.colon.ColonDossier;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.colon.ColonUitnodiging;
import nl.rivm.screenit.model.colon.enums.ColonUitnodigingsintervalType;
import nl.rivm.screenit.model.colon.enums.IFOBTTestStatus;
import nl.rivm.screenit.service.colon.ColonDossierBaseService;
import nl.rivm.screenit.service.colon.ColonVerwerkVerslagService;
import nl.rivm.screenit.util.AfmeldingUtil;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.FITTestUtil;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.springframework.stereotype.Service;

import static nl.rivm.screenit.clientportaal.model.colon.ColonVolgendeUitnodigingTekstType.DEELNEMEN_TOT;
import static nl.rivm.screenit.clientportaal.model.colon.ColonVolgendeUitnodigingTekstType.GEEN_NIEUWE_RONDE_LEEFTIJD;
import static nl.rivm.screenit.clientportaal.model.colon.ColonVolgendeUitnodigingTekstType.GEEN_TEKST;
import static nl.rivm.screenit.clientportaal.model.colon.ColonVolgendeUitnodigingTekstType.GEEN_UITNODIGING;
import static nl.rivm.screenit.clientportaal.model.colon.ColonVolgendeUitnodigingTekstType.UITNODIGING_ONTVANGEN_VANAF;

@Service
@RequiredArgsConstructor
@Slf4j
public class ColonVolgendeUitnodigingDatumTekstServiceImpl implements ColonVolgendeUitnodigingDatumTekstService
{
	private final ColonDossierBaseService dossierService;

	private final SimplePreferenceService preferenceService;

	private final ColonAfspraakService afspraakService;

	private final ColonVerwerkVerslagService verslagService;

	@Override
	public String geefVolgendeUitnodigingDatumString(Client client)
	{
		var datumVolgendeUitnodiging = dossierService.getDatumVolgendeUitnodiging(client.getColonDossier());

		return datumVolgendeUitnodiging != null ? datumVolgendeUitnodiging.format(DateTimeFormatter.ofPattern("MMMM yyyy", new Locale("nl", "NL"))) : null;
	}

	@Override
	public ColonVolgendeUitnodigingTekstType bepaalVolgendeUitnodigingTekstType(Client client)
	{
		var dossier = client.getColonDossier();
		var datumVolgendeUitnodiging = dossierService.getDatumVolgendeUitnodiging(dossier);

		var ronde = dossier.getLaatsteScreeningRonde();

		if (heeftGeenUitnodigingOntvangen(dossier))
		{
			return GEEN_TEKST;
		}

		if (clientTeOudVoorNogEenRonde(client, datumVolgendeUitnodiging))
		{
			return GEEN_NIEUWE_RONDE_LEEFTIJD;
		}
		if (ronde != null)
		{
			if (datumVolgendeUitnodiging == null || AfmeldingUtil.isAfgerondeDefinitieveAfmelding(dossier.getLaatsteAfmelding()) || dossierIsInactief(dossier))
			{
				return GEEN_UITNODIGING;
			}
			else if (heeftGunstigOfOngunstigeUitslagMetConclusie(dossier) || isEenmaligOfTijdelijkAfgemeld(ronde))
			{
				return UITNODIGING_ONTVANGEN_VANAF;
			}
			else if (heefOngunstigeUitslagZonderConclusie(dossier))
			{
				return GEEN_TEKST;
			}
			else if (clientHeeftNogGeenFitIngestuurd(dossier))
			{
				return DEELNEMEN_TOT;
			}
		}
		else
		{
			if (geenRondeDoorDossierVerwijderdBezwaar(dossier))
			{
				return UITNODIGING_ONTVANGEN_VANAF;
			}
		}

		LOG.error("Clientportaal volgende uitnodigingsdatumtekst genereren niet gelukt voor client met id: '{}'.", client.getId());

		return GEEN_TEKST;
	}

	private boolean dossierIsInactief(ColonDossier dossier)
	{
		return dossier.getStatus() == DossierStatus.INACTIEF;
	}

	private boolean clientTeOudVoorNogEenRonde(Client client, LocalDate datumVolgendeUitnodiging)
	{
		if (datumVolgendeUitnodiging != null)
		{
			var maximaleLeeftijd = preferenceService.getInteger(PreferenceKey.MAXIMALE_LEEFTIJD_COLON.name());
			var leeftijdBijVolgendeUitnodiging = DateUtil.getLeeftijd(DateUtil.toLocalDate(client.getPersoon().getGeboortedatum()), datumVolgendeUitnodiging);

			return leeftijdBijVolgendeUitnodiging > maximaleLeeftijd;
		}
		else
		{
			return false;
		}
	}

	private boolean heeftGunstigOfOngunstigeUitslagMetConclusie(ColonDossier dossier)
	{
		var heeftOngunstigeUitslag = FITTestUtil.heeftOngunstigeUitslagInLaatsteRonde(dossier);
		var heeftGunstigeUitslag = FITTestUtil.heeftGunstigeUitslagInLaatsteRonde(dossier);

		if (!heeftGunstigeUitslag && !heeftOngunstigeUitslag)
		{
			return false;
		}

		if (heeftGunstigeUitslag && !heeftOngunstigeUitslag)
		{
			return true;
		}

		var heeftVervolgbeleid = verslagService.rondeHeeftDefinitiefMdlVervolgbeleid(dossier.getLaatsteScreeningRonde());
		var heeftAfgerondeAfspraak = afspraakService.laatsteAfspraakHeeftDefinitieveIntakeconclusie(dossier.getClient());

		return heeftVervolgbeleid || heeftAfgerondeAfspraak;
	}

	private boolean heefOngunstigeUitslagZonderConclusie(ColonDossier dossier)
	{
		return FITTestUtil.heeftOngunstigeUitslagInLaatsteRonde(dossier) && !afspraakService.laatsteAfspraakHeeftDefinitieveIntakeconclusie(dossier.getClient());
	}

	private boolean isEenmaligOfTijdelijkAfgemeld(ColonScreeningRonde screeningRonde)
	{
		var afmelding = screeningRonde.getLaatsteAfmelding();

		return AfmeldingUtil.isAfgerondeEenmaligeAfmelding(afmelding) || AfmeldingUtil.isAfgerondeTijdelijkeAfmelding(afmelding);
	}

	private boolean geenRondeDoorDossierVerwijderdBezwaar(ColonDossier dossier)
	{
		return dossier.getVolgendeUitnodiging() != null && dossier.getVolgendeUitnodiging().getInterval().getType() == ColonUitnodigingsintervalType.VERWIJDERD_DOSSIER;
	}

	private boolean heeftGeenUitnodigingOntvangen(ColonDossier dossier)
	{
		var ronde = dossier.getLaatsteScreeningRonde();

		var geenVolgendeUitnodiging = dossier.getVolgendeUitnodiging() == null;
		var geenRondeDoorBezwaar = geenRondeDoorDossierVerwijderdBezwaar(dossier);

		var heeftAlUitnodigingGehad = dossier.getScreeningRondes()
			.stream()
			.flatMap(screeningRonde -> screeningRonde.getUitnodigingen().stream())
			.anyMatch(ColonUitnodiging::isVerstuurdDoorInpakcentrum);

		return ronde == null ? geenVolgendeUitnodiging || !geenRondeDoorBezwaar : !heeftAlUitnodigingGehad;
	}

	private boolean clientHeeftNogGeenFitIngestuurd(ColonDossier dossier)
	{
		return dossier.getLaatsteScreeningRonde().getIfobtTesten().stream().noneMatch(
			fit -> fit.getStatus() != IFOBTTestStatus.ACTIEF);
	}
}
