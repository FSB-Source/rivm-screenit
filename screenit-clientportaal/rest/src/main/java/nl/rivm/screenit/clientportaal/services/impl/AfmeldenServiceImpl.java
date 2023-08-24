package nl.rivm.screenit.clientportaal.services.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import lombok.AllArgsConstructor;

import nl.rivm.screenit.clientportaal.model.AfmeldOptiesDto;
import nl.rivm.screenit.clientportaal.model.AfmeldingDto;
import nl.rivm.screenit.clientportaal.services.AfmeldenService;
import nl.rivm.screenit.model.AanvraagBriefStatus;
import nl.rivm.screenit.model.Afmelding;
import nl.rivm.screenit.model.AfmeldingType;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Dossier;
import nl.rivm.screenit.model.ScreeningRonde;
import nl.rivm.screenit.model.cervix.CervixAfmelding;
import nl.rivm.screenit.model.cervix.enums.CervixAfmeldingReden;
import nl.rivm.screenit.model.colon.ColonAfmelding;
import nl.rivm.screenit.model.colon.enums.ColonAfmeldingReden;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.mamma.MammaAfmelding;
import nl.rivm.screenit.model.mamma.enums.MammaAfmeldingReden;
import nl.rivm.screenit.service.ClientContactService;
import nl.rivm.screenit.service.colon.ColonTijdelijkAfmeldenJaartallenService;
import nl.rivm.screenit.util.AfmeldingUtil;
import nl.rivm.screenit.util.EnumStringUtil;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@AllArgsConstructor
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class AfmeldenServiceImpl implements AfmeldenService
{
	private final ClientContactService clientContactService;

	private final ColonTijdelijkAfmeldenJaartallenService tijdelijkAfmeldenJaartallenService;

	@Override
	public AfmeldOptiesDto getAfmeldOpties(Client client, Bevolkingsonderzoek bvo)
	{
		List<AfmeldingType> afmeldOpties = new ArrayList<>();
		List<String> afmeldRedenenEenmalig = new ArrayList<>();
		List<String> afmeldRedenenTijdelijk = new ArrayList<>();
		List<String> afmeldRedenenDefinitief = new ArrayList<>();
		List<Integer> mogelijkeAfmeldJaren = new ArrayList<>();
		boolean heeftOpenIntakeAfspraak = false;

		switch (bvo)
		{
		case CERVIX:
			afmeldOpties = clientContactService.getAvailableAfmeldoptiesCervix(client, true);
			Arrays.stream(CervixAfmeldingReden.values()).forEach(afmeldingReden -> afmeldRedenenDefinitief.add(EnumStringUtil.getPropertyString(afmeldingReden)));
			break;
		case COLON:
			afmeldOpties = clientContactService.getAvailableAfmeldoptiesColon(client, true);
			List<ColonAfmeldingReden> gefilterdeColonAfmeldingRedenen = Arrays.stream(ColonAfmeldingReden.values())
				.filter(afmeldingReden -> !afmeldingReden.equals(ColonAfmeldingReden.ONTERECHT)
					&& !afmeldingReden.equals(ColonAfmeldingReden.PROEF_BEVOLKINGSONDERZOEK))
				.collect(Collectors.toList());

			gefilterdeColonAfmeldingRedenen.forEach(afmeldingReden -> afmeldRedenenEenmalig.add(EnumStringUtil.getPropertyString(afmeldingReden)));
			gefilterdeColonAfmeldingRedenen.forEach(afmeldingReden -> afmeldRedenenTijdelijk.add(EnumStringUtil.getPropertyString(afmeldingReden)));
			gefilterdeColonAfmeldingRedenen.forEach(afmeldingReden -> afmeldRedenenDefinitief.add(EnumStringUtil.getPropertyString(afmeldingReden)));
			mogelijkeAfmeldJaren = tijdelijkAfmeldenJaartallenService.bepaalMogelijkeAfmeldJaren(client);

			heeftOpenIntakeAfspraak = clientContactService.heeftOpenIntakeAfspraak(client);
			break;
		case MAMMA:
			afmeldOpties = clientContactService.getAvailableAfmeldoptiesMamma(client, true);
			MammaAfmeldingReden.eenmaligeRedenen().forEach(afmeldingReden -> afmeldRedenenEenmalig.add(EnumStringUtil.getPropertyString(afmeldingReden)));
			MammaAfmeldingReden.definitieveRedenen().forEach(afmeldingReden -> afmeldRedenenDefinitief.add(EnumStringUtil.getPropertyString(afmeldingReden)));
			break;
		}
		return new AfmeldOptiesDto(afmeldOpties, afmeldRedenenEenmalig, afmeldRedenenTijdelijk, afmeldRedenenDefinitief, mogelijkeAfmeldJaren, heeftOpenIntakeAfspraak);
	}

	@Override
	public CervixAfmelding valideerEnGetCervixAfmelding(AfmeldingDto<CervixAfmeldingReden> afmeldingDto, Client client)
	{
		if (afmeldingDto.getAfmeldType() == null)
		{
			throw new IllegalStateException("Niet alle verplichte velden zijn gevuld");
		}
		else if (!clientContactService.getAvailableAfmeldoptiesCervix(client, true).contains(afmeldingDto.getAfmeldType()))
		{
			throw new IllegalStateException("Afmeldtype " + afmeldingDto.getAfmeldType().name() + " is ongeldig");
		}

		CervixAfmelding cervixAfmelding = vulAfmelding(afmeldingDto, new CervixAfmelding(), client.getCervixDossier());

		if (AfmeldingType.DEFINITIEF.equals(cervixAfmelding.getType()))
		{
			if (afmeldingDto.getAfmeldReden() == null)
			{
				throw new IllegalStateException("Niet alle verplichte velden zijn gevuld");
			}
			cervixAfmelding.setReden(afmeldingDto.getAfmeldReden());
		}
		return cervixAfmelding;
	}

	@Override
	public ColonAfmelding valideerEnGetColonAfmelding(AfmeldingDto<ColonAfmeldingReden> afmeldingDto, Client client)
	{
		if (afmeldingDto.getAfmeldType() == null)
		{
			throw new IllegalStateException("Niet alle verplichte velden zijn gevuld");
		}
		else if (!clientContactService.getAvailableAfmeldoptiesColon(client, true).contains(afmeldingDto.getAfmeldType()))
		{
			throw new IllegalStateException("Afmeldtype " + afmeldingDto.getAfmeldType().name() + " is ongeldig");
		}
		else if (AfmeldingType.TIJDELIJK.equals(afmeldingDto.getAfmeldType()) && afmeldingDto.getAfmeldenTotJaartal() == null)
		{
			throw new IllegalStateException("Tijdelijke afmelding zonder jaartal tot wanneer er afgemeld moet worden!");
		}

		ColonAfmelding colonAfmelding = vulAfmelding(afmeldingDto, new ColonAfmelding(), client.getColonDossier());

		if (afmeldingDto.getAfmeldenTotJaartal() != null)
		{
			colonAfmelding.setTijdelijkAfmeldenTotJaartal(afmeldingDto.getAfmeldenTotJaartal());
		}

		return colonAfmelding;
	}

	private <S extends ScreeningRonde<D, ?, A, ?>, D extends Dossier<S, A>, A extends Afmelding<S, D, ?>> A vulAfmelding(AfmeldingDto<?> afmeldingDto, A afmelding, D dossier)
	{
		var laatsteAfmelding = dossier.getLaatsteAfmelding();
		if (AfmeldingUtil.isAangevraagdeDefinitieveAfmelding(laatsteAfmelding))
		{
			afmelding = laatsteAfmelding;
		}
		else
		{
			afmelding = haalTijdelijkeAfmeldingAanvraagUitLaatsteRonde(afmelding, dossier);
		}
		if (afmelding.getId() == null)
		{
			afmelding.setAfmeldingStatus(AanvraagBriefStatus.BRIEF);
		}
		afmelding.setType(afmeldingDto.getAfmeldType());
		return afmelding;
	}

	private static <S extends ScreeningRonde<D, ?, A, ?>, D extends Dossier<S, A>, A extends Afmelding<S, D, ?>> A haalTijdelijkeAfmeldingAanvraagUitLaatsteRonde(A afmelding,
		D dossier)
	{
		var laatsteScreeningRonde = dossier.getLaatsteScreeningRonde();
		if (laatsteScreeningRonde != null)
		{
			A laatsteAfmelding = laatsteScreeningRonde.getLaatsteAfmelding();
			if (AfmeldingUtil.isAangevraagdeTijdelijkeAfmelding(laatsteAfmelding))
			{
				afmelding = laatsteAfmelding;
			}
		}
		return afmelding;
	}

	@Override
	public MammaAfmelding valideerEnGetMammaAfmelding(AfmeldingDto<MammaAfmeldingReden> afmeldingDto, Client client)
	{
		if (afmeldingDto.getAfmeldReden() == null || afmeldingDto.getAfmeldType() == null)
		{
			throw new IllegalStateException("Niet alle verplichte velden zijn gevuld");
		}
		else if (!clientContactService.getAvailableAfmeldoptiesMamma(client, true).contains(afmeldingDto.getAfmeldType()))
		{
			throw new IllegalStateException("Afmeldtype " + afmeldingDto.getAfmeldType().name() + " is ongeldig");
		}

		MammaAfmelding mammaAfmelding = vulAfmelding(afmeldingDto, new MammaAfmelding(), client.getMammaDossier());

		mammaAfmelding.setReden(afmeldingDto.getAfmeldReden());

		return mammaAfmelding;
	}
}
