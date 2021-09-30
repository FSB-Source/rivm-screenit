package nl.rivm.screenit.huisartsenportaal.config.autorizationproviders;

/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
 * %%
 * Copyright (C) 2016 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.util.regex.Pattern;

import nl.rivm.screenit.huisartsenportaal.exception.BadCredentialsException;
import nl.rivm.screenit.huisartsenportaal.exception.UserNotFoundException;
import nl.rivm.screenit.huisartsenportaal.model.Huisarts;
import nl.rivm.screenit.huisartsenportaal.model.enums.AanmeldStatus;
import nl.rivm.screenit.huisartsenportaal.model.enums.InlogMethode;
import nl.rivm.screenit.huisartsenportaal.model.enums.Scope;
import nl.rivm.screenit.huisartsenportaal.repository.HuisartsRepository;
import nl.rivm.screenit.huisartsenportaal.service.HuisartsService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.authentication.CredentialsExpiredException;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.stereotype.Component;

@Component
public class ScreenITWachtwoordVergetenAuthenticationProvider extends AbstractAuthenticationProvider
{
	public static final Pattern VALID_EMAIL_ADDRESS_REGEX = Pattern.compile("^[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,6}$", Pattern.CASE_INSENSITIVE);

	@Autowired
	private HuisartsService huisartsService;

	@Autowired
	private HuisartsRepository huisartsRepository;

	@Override
	protected void additionalAuthenticationChecks(UserDetails userDetails, UsernamePasswordAuthenticationToken authentication) throws AuthenticationException
	{
		Huisarts huisarts = (Huisarts) userDetails;
		if (!huisarts.getInlogCode().equals(authentication.getCredentials()))
		{
			Integer remainingAttempts = huisartsService.increaseAttemps(huisarts);
			throw new BadCredentialsException("U heeft een foutieve inlogcode ingevoerd. U heeft nog " + remainingAttempts + " pogingen.");
		}
		huisartsService.resetAttemps(huisarts);
	}

	@Override
	protected UserDetails getUser(String username)
	{
		Huisarts huisarts = huisartsRepository.findByGebruikersnaam(username);
		if (huisarts == null && VALID_EMAIL_ADDRESS_REGEX.matcher(username).find())
		{
			List<Huisarts> huisartsen = huisartsRepository.findByEmail(username);
			huisarts = huisartsen.stream()
				.filter(h -> InlogMethode.USERNAME_PASSWORD == h.getInlogMethode() && h.getInlogCode() != null && h.getInlogCode().equals(getCredentials())).findAny().orElse(null);
		}
		if (huisarts == null || !huisarts.isAccountNonExpired())
		{
			throw new UserNotFoundException("Gebruikersnaam of e-mail is onbekend. Er is geen gebruiker gevonden.");
		}
		if ((AanmeldStatus.GEREGISTREERD == huisarts.getAanmeldStatus() && huisarts.getInlogCode() == null)
			|| AanmeldStatus.GEREGISTREERD != huisarts.getAanmeldStatus())
		{
			throw new CredentialsExpiredException("Gebruikersnaam of e-mailadres is onbekend. Gebruiker niet gevonden.");
		}
		if (!huisarts.isCredentialsNonExpired())
		{
			throw new CredentialsExpiredException("Wachtwoord aanvragen mislukt. Vraag opnieuw uw wachtwoord aan.");
		}
		return huisarts;
	}

	@Override
	protected String getScope()
	{
		return Scope.WACHTWOORDVERGETEN;
	}

}
