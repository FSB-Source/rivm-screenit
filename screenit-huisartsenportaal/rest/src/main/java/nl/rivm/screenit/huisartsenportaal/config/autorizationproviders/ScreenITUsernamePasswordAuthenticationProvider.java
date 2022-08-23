package nl.rivm.screenit.huisartsenportaal.config.autorizationproviders;

/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
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

import nl.rivm.screenit.huisartsenportaal.exception.BadCredentialsException;
import nl.rivm.screenit.huisartsenportaal.exception.UserNotFoundException;
import nl.rivm.screenit.huisartsenportaal.model.Huisarts;
import nl.rivm.screenit.huisartsenportaal.model.enums.AanmeldStatus;
import nl.rivm.screenit.huisartsenportaal.model.enums.Scope;
import nl.rivm.screenit.huisartsenportaal.repository.HuisartsRepository;
import nl.rivm.screenit.huisartsenportaal.service.HuisartsService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.authentication.CredentialsExpiredException;
import org.springframework.security.authentication.LockedException;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.stereotype.Component;

@Component
public class ScreenITUsernamePasswordAuthenticationProvider extends AbstractAuthenticationProvider
{
	@Autowired
	private HuisartsService huisartsService;

	@Autowired
	private HuisartsRepository huisartsRepository;

	@Override
	protected void additionalAuthenticationChecks(UserDetails userDetails, UsernamePasswordAuthenticationToken usernamePasswordAuthenticationToken) throws AuthenticationException
	{
		Huisarts huisarts = (Huisarts) userDetails;
		String encodedPassword = userDetails.getPassword();
		if (!huisartsService.controleerWachtwoord(String.valueOf(usernamePasswordAuthenticationToken.getCredentials()), encodedPassword))
		{
			Integer attempts = huisartsService.incrementAttempts(huisarts);
			throw new BadCredentialsException("Uw ingevoerde wachtwoord is ongeldig. U heeft nog " + attempts + " pogingen.");
		}
		huisartsService.resetAttempts(huisarts);
	}

	@Override
	protected UserDetails getUser(String username)
	{

		Huisarts huisarts = huisartsRepository.findByGebruikersnaam(username);
		if (huisarts == null || !huisarts.isAccountNonExpired())
		{
			throw new UserNotFoundException("Inloggen mislukt. Gebruiker niet gevonden.");
		}
		if (AanmeldStatus.GEREGISTREERD != huisarts.getAanmeldStatus() && huisarts.getInlogCode() != null)
		{
			throw new CredentialsExpiredException("U moet zich eerst opnieuw registreren voor u weer kunt inloggen. Volg de instructies die u per brief heeft ontvangen.");
		}
		if (AanmeldStatus.GEREGISTREERD == huisarts.getAanmeldStatus() && huisarts.getInlogCode() != null)
		{
			throw new CredentialsExpiredException("U heeft een nieuw wachtwoord aangevraagd. Volg de instructies die u per e-mail heeft ontvangen.");
		}
		if (!huisarts.isAccountNonLocked())
		{
			Long remainingMinutes = huisartsService.remainingMinutesLock(huisarts);
			throw new LockedException("Inloggen mislukt. Uw account is voor " + remainingMinutes + " minuten geblokkeerd.");
		}
		return huisarts;
	}

	@Override
	protected String getScope()
	{
		return Scope.LOGIN;
	}

}
