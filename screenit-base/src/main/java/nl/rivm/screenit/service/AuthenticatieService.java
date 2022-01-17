package nl.rivm.screenit.service;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.util.List;
import java.util.Map;

import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.InstellingGebruiker;

public interface AuthenticatieService
{

	Map<Gebruiker, Boolean> requestNewPassword(Gebruiker searchObject);

	Map<Gebruiker, Boolean> accountGeblokkeerd(Gebruiker searchObject);

	List<InstellingGebruiker> getActieveInstellingGebruikers(Gebruiker gebruiker);

	Boolean isAccountLocked(Gebruiker gebruiker);

	void unlockAccount(Gebruiker gebruiker);

	void foutieveInlogpoging(Gebruiker gebruiker);

	void sendUziEmail(Gebruiker gebruiker);
}
