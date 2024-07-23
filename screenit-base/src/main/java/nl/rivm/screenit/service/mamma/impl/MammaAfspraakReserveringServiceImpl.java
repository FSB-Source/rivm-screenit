package nl.rivm.screenit.service.mamma.impl;

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

import java.time.LocalDateTime;
import java.util.List;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaAfspraakReservering;
import nl.rivm.screenit.model.mamma.MammaCapaciteitBlok;
import nl.rivm.screenit.repository.mamma.MammaAfspraakReserveringRepository;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.mamma.MammaAfspraakReserveringService;
import nl.rivm.screenit.specification.mamma.MammaAfspraakReserveringSpecification;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

@Component
@RequiredArgsConstructor
@Slf4j
public class MammaAfspraakReserveringServiceImpl implements MammaAfspraakReserveringService
{
	private final MammaAfspraakReserveringRepository afspraakReserveringRepository;

	private final ICurrentDateSupplier currentDateSupplier;

	private final SimplePreferenceService preferenceService;

	@Transactional
	public MammaAfspraakReservering maakAfspraakReservering(MammaAfspraak afspraak, InstellingGebruiker gebruiker)
	{
		var client = afspraak.getUitnodiging().getScreeningRonde().getDossier().getClient();
		var reservering = afspraakReserveringRepository.findByClient(client).orElse(new MammaAfspraakReservering());
		reservering.setClient(client);
		reservering.setMedewerker(gebruiker);
		reservering.setCapaciteitBlok(afspraak.getCapaciteitBlok());
		reservering.setOpkomstkans(afspraak.getOpkomstkans().getOpkomstkans());
		reservering.setVanaf(DateUtil.toLocalDateTime(afspraak.getVanaf()));
		reservering.setAangemaaktOp(currentDateSupplier.getLocalDateTime());
		var opgeslagenReservering = afspraakReserveringRepository.save(reservering);
		LOG.info("BK Afspraak reservering aangemaakt voor vanaf: '{}' capaciteitBlokId: '{}' clientId: '{}'",
			reservering.getVanaf(), reservering.getCapaciteitBlok().getId(), reservering.getClient().getId());
		return opgeslagenReservering;
	}

	public List<MammaAfspraakReservering> getActieveReserveringenVoorCapaciteitBlok(MammaCapaciteitBlok blok)
	{
		var maximaleReserveringsTijd = preferenceService.getInteger(PreferenceKey.MAMMA_AFSPRAAK_RESERVERING_GELDIG_VOOR.name(), 0);
		var vroegstOpTeHalenReservering = currentDateSupplier.getLocalDateTime().minusMinutes(maximaleReserveringsTijd);
		return afspraakReserveringRepository.findAll(MammaAfspraakReserveringSpecification.heeftCapaciteitBlok(blok)
			.and(MammaAfspraakReserveringSpecification.heeftReserveringGemaaktNa(vroegstOpTeHalenReservering)));
	}

	@Override
	@Transactional
	public void verwijderReserveringenVoorClient(Client client)
	{
		afspraakReserveringRepository.deleteAllByClient(client);
	}

	@Override
	@Transactional
	public void verwijderReserveringenVanMedewerker(InstellingGebruiker medewerker)
	{
		afspraakReserveringRepository.deleteAllByMedewerker(medewerker);
	}

	@Override
	@Transactional
	public void verwijderAfspraakReserveringenDieGemaaktZijnVoor(LocalDateTime moment)
	{
		afspraakReserveringRepository.deleteAllByAangemaaktOpBefore(moment);
	}
}
