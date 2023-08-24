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

import lombok.AllArgsConstructor;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.clientportaal.model.BvoParametersDto;
import nl.rivm.screenit.clientportaal.model.LandingOverzichtDto;
import nl.rivm.screenit.clientportaal.services.LandingOverzichtService;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.cervix.enums.CervixLeeftijdcategorie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.service.ClientDoelgroepService;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@AllArgsConstructor
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class LandingOverzichtServiceImpl implements LandingOverzichtService
{

	private final SimplePreferenceService preferenceService;

	private final ClientDoelgroepService doelgroepService;

	@Override
	public LandingOverzichtDto getLandingOverzicht(Client client)
	{
		var behoortTotCervixDoelgroep = doelgroepService.behoortTotDoelgroep(client, Bevolkingsonderzoek.CERVIX);
		var behoortTotColonDoelgroep = doelgroepService.behoortTotDoelgroep(client, Bevolkingsonderzoek.COLON);
		var behoortTotMammaDoelgroep = doelgroepService.behoortTotDoelgroep(client, Bevolkingsonderzoek.MAMMA);

		var dto = new LandingOverzichtDto();
		dto.setMammaParameters(getMammaParameters());
		dto.setBehoortTotMammaDoelgroep(behoortTotMammaDoelgroep);
		dto.setCervixParameters(getCervixParameters());
		dto.setBehoortTotCervixDoelgroep(behoortTotCervixDoelgroep);
		dto.setColonParameters(getColonParameters());
		dto.setBehoortTotColonDoelgroep(behoortTotColonDoelgroep);
		return dto;
	}

	private BvoParametersDto getMammaParameters()
	{
		return new BvoParametersDto(
			preferenceService.getInteger(PreferenceKey.MAMMA_MINIMALE_LEEFTIJD.name()),
			preferenceService.getInteger(PreferenceKey.MAMMA_MAXIMALE_LEEFTIJD.name()),
			preferenceService.getString(PreferenceKey.MAMMA_CLIENTPORTAAL_TIJDELIJKE_MELDING.name()),
			preferenceService.getBoolean(PreferenceKey.MAMMA_CLIENTPORTAAL_TOON_VERVANGENDE_TEKST.name(), false),
			preferenceService.getString(PreferenceKey.MAMMA_CLIENTPORTAAL_VERVANGENDE_TEKST.name()));
	}

	private BvoParametersDto getCervixParameters()
	{
		return new BvoParametersDto(
			CervixLeeftijdcategorie.minimumLeeftijd(),
			CervixLeeftijdcategorie._60.getLeeftijd(),
			preferenceService.getString(PreferenceKey.CERVIX_CLIENTPORTAAL_TIJDELIJKE_MELDING.name()),
			preferenceService.getBoolean(PreferenceKey.CERVIX_CLIENTPORTAAL_TOON_VERVANGENDE_TEKST.name(), false),
			preferenceService.getString(PreferenceKey.CERVIX_CLIENTPORTAAL_VERVANGENDE_TEKST.name()));
	}

	private BvoParametersDto getColonParameters()
	{
		return new BvoParametersDto(
			preferenceService.getInteger(PreferenceKey.MINIMALE_LEEFTIJD_COLON.name()),
			preferenceService.getInteger(PreferenceKey.MAXIMALE_LEEFTIJD_COLON.name()),
			preferenceService.getString(PreferenceKey.COLON_CLIENTPORTAAL_TIJDELIJKE_MELDING.name()),
			preferenceService.getBoolean(PreferenceKey.COLON_CLIENTPORTAAL_TOON_VERVANGENDE_TEKST.name(), false),
			preferenceService.getString(PreferenceKey.COLON_CLIENTPORTAAL_VERVANGENDE_TEKST.name()));
	}
}
