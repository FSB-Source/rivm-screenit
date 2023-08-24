package nl.rivm.screenit.batch.jobs.generalis.wachtwoordverlooptherinnering;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-alg
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

import lombok.RequiredArgsConstructor;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.batch.jobs.helpers.BaseSpecificationScrollableResultReader;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.specification.algemeen.GebruikerSpecification;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class WachtwoordVerlooptHerinneringReader extends BaseSpecificationScrollableResultReader<Gebruiker, Long>
{
	private final SimplePreferenceService preferenceService;

	private final ICurrentDateSupplier currentDateSupplier;

	@Override
	public Specification<Gebruiker> createSpecification()
	{
		var dagenWachtwoordGeldig = preferenceService.getInteger(PreferenceKey.DAGEN_WACHTWOORD_GELDIG.name(), 365);
		var wachtwoordVerlooptTermijn = preferenceService.getInteger(PreferenceKey.WACHTWOORD_VERLOOPT_HERINNERINGS_TERMIJN.name(), 14);

		var vandaag = currentDateSupplier.getLocalDate();
		var peildatumHerinneringsTermijn = vandaag.minusDays(dagenWachtwoordGeldig).plusDays(wachtwoordVerlooptTermijn);
		return GebruikerSpecification.isActieveGebruiker(vandaag, dagenWachtwoordGeldig)
			.and(GebruikerSpecification.heeftEmailAdres())
			.and(GebruikerSpecification.heeftWachtwoordInlogMethode())
			.and(GebruikerSpecification.isNietGeblokkeerd())
			.and(GebruikerSpecification.moetHerinneringKrijgen(peildatumHerinneringsTermijn));
	}

}
