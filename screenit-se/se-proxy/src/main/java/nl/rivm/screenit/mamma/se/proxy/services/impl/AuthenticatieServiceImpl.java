package nl.rivm.screenit.mamma.se.proxy.services.impl;

/*-
 * ========================LICENSE_START=================================
 * se-proxy
 * %%
 * Copyright (C) 2017 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.mamma.se.proxy.dao.AuthenticatieDao;
import nl.rivm.screenit.mamma.se.proxy.model.IngelogdeGebruikerDto;
import nl.rivm.screenit.mamma.se.proxy.model.LoginContext;
import nl.rivm.screenit.mamma.se.proxy.services.AuthenticatieService;
import nl.rivm.screenit.mamma.se.proxy.util.DateUtil;

import org.apache.shiro.crypto.hash.Sha512Hash;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class AuthenticatieServiceImpl implements AuthenticatieService
{
	private static final int PASSWORDHASHINGITERATIONS = 300000;

	@Autowired
	private AuthenticatieDao authenticatieDao;

	@Override
	public IngelogdeGebruikerDto getIngelogdeGebruiker(LoginContext loginContext)
	{
		return authenticatieDao.getIngelogdeGebruiker(loginContext);
	}

	@Override
	public void insertOrUpdateIngelogdeGebruiker(IngelogdeGebruikerDto ingelogdeGebruikerDto)
	{
		authenticatieDao.insertOrUpdateIngelogdeGebruiker(ingelogdeGebruikerDto);
	}

	@Override
	public void updateIngelogdeGebruiker(IngelogdeGebruikerDto ingelogdeGebruikerDto)
	{
		authenticatieDao.updateIngelogdeGebruiker(ingelogdeGebruikerDto);
	}

	@Override
	public Long getAccountIdFromUsername(String gebruikersnaam)
	{
		return authenticatieDao.getAccountIdFromUsername(gebruikersnaam);
	}

	@Override
	public void verwijderOudeIngelogdeGebruikers()
	{
		authenticatieDao.verwijderOudeIngelogdeGebruikers();
	}

	@Override
	public String hashWachtwoord(String plainWachtwoord, Long accountId)
	{
		Sha512Hash hash = new Sha512Hash(plainWachtwoord, accountId.toString(), PASSWORDHASHINGITERATIONS);
		return hash.toHex();
	}

	@Override
	public void administreerOnlineInlog(LoginContext loginContext, String loginResponse)
	{
		IngelogdeGebruikerDto ingelogdeGebruikerDto = new IngelogdeGebruikerDto(loginContext.getGebruikersnaam(), loginContext.getEncryptedWachtwoord(),
			DateUtil.getCurrentDateTime().toLocalDate(), loginContext.getYubikeyIdentificatie(), loginResponse, loginContext.getAccountId());
		insertOrUpdateIngelogdeGebruiker(ingelogdeGebruikerDto);
	}
}
