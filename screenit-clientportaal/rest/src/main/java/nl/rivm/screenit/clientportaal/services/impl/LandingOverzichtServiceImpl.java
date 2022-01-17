package nl.rivm.screenit.clientportaal.services.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.clientportaal.model.BvoParametersDto;
import nl.rivm.screenit.clientportaal.model.LandingOverzichtDto;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.cervix.CervixDossier;
import nl.rivm.screenit.model.colon.ColonDossier;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.service.ClientDoelgroepService;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class LandingOverzichtServiceImpl
{

	@Autowired
	private SimplePreferenceService preferenceService;

	@Autowired
	private ClientDoelgroepService doelgroepService;

	public LandingOverzichtDto getLandingOverzicht(Client client)
	{

		boolean behoortTotCervixDoelgroep = doelgroepService.behoortTotDoelgroep(client, Bevolkingsonderzoek.CERVIX);
		boolean behoortTotColonDoelgroep = doelgroepService.behoortTotDoelgroep(client, Bevolkingsonderzoek.COLON);
		boolean behoortTotMammaDoelgroep = doelgroepService.behoortTotDoelgroep(client, Bevolkingsonderzoek.MAMMA);

		return new LandingOverzichtDto(
			getMammaParameters(client.getMammaDossier()),
			behoortTotMammaDoelgroep,
			getCervixParameters(client.getCervixDossier()),
			behoortTotCervixDoelgroep,
			getColonParameters(client.getColonDossier()),
			behoortTotColonDoelgroep);
	}

	private BvoParametersDto getMammaParameters(MammaDossier dossier)
	{
		if (dossier == null)
		{
			return null;
		}

		return new BvoParametersDto(
			preferenceService.getInteger(PreferenceKey.MAMMA_MINIMALE_LEEFTIJD.name()),
			preferenceService.getInteger(PreferenceKey.MAMMA_MAXIMALE_LEEFTIJD.name()),
			preferenceService.getString(PreferenceKey.MAMMA_CLIENTPORTAAL_TIJDELIJKE_MELDING.name()),
			preferenceService.getBoolean(PreferenceKey.MAMMA_CLIENTPORTAAL_TOON_VERVANGENDE_TEKST.name(), false),
			preferenceService.getString(PreferenceKey.MAMMA_CLIENTPORTAAL_VERVANGENDE_TEKST.name()));

	}

	private BvoParametersDto getCervixParameters(CervixDossier dossier)
	{
		if (dossier == null)
		{
			return null;
		}

		return new BvoParametersDto(
			30,
			60,
			preferenceService.getString(PreferenceKey.CERVIX_CLIENTPORTAAL_TIJDELIJKE_MELDING.name()),
			preferenceService.getBoolean(PreferenceKey.CERVIX_CLIENTPORTAAL_TOON_VERVANGENDE_TEKST.name(), false),
			preferenceService.getString(PreferenceKey.CERVIX_CLIENTPORTAAL_VERVANGENDE_TEKST.name()));
	}

	private BvoParametersDto getColonParameters(ColonDossier dossier)
	{
		if (dossier == null)
		{
			return null;
		}

		return new BvoParametersDto(
			preferenceService.getInteger(PreferenceKey.MINIMALE_LEEFTIJD_COLON.name()),
			preferenceService.getInteger(PreferenceKey.MAXIMALE_LEEFTIJD_COLON.name()),
			preferenceService.getString(PreferenceKey.COLON_CLIENTPORTAAL_TIJDELIJKE_MELDING.name()),
			preferenceService.getBoolean(PreferenceKey.COLON_CLIENTPORTAAL_TOON_VERVANGENDE_TEKST.name(), false),
			preferenceService.getString(PreferenceKey.COLON_CLIENTPORTAAL_VERVANGENDE_TEKST.name()));
	}
}
