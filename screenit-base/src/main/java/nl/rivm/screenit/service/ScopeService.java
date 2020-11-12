
package nl.rivm.screenit.service;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.enums.ToegangLevel;
import nl.rivm.screenit.security.Constraint;

import org.apache.shiro.subject.PrincipalCollection;

public interface ScopeService
{
	
	boolean isObjectInScope(Constraint constraintToCheck, Account account, PrincipalCollection principals);

	ToegangLevel getHoogsteToegangLevel(PrincipalCollection principalCollection, Constraint constraintToCheck);

	ToegangLevel getHoogsteToegangLevel(InstellingGebruiker instellingGebruiker, Constraint constraint, boolean checkBvo);
}
