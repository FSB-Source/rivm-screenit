package nl.rivm.screenit.model.envers;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.InstellingGebruiker;

import org.hibernate.envers.RevisionListener;

public class ScreenitRevisionListener implements RevisionListener
{
	@Override
	public void newRevision(Object revisionEntity)
	{
		ScreenitRevisionEntity screenitRevisionEntity = (ScreenitRevisionEntity) revisionEntity;
		Account account = RevisionInformationResolver.getAccount();
		if (account != null)
		{
			if (account instanceof Gebruiker)
			{
				Gebruiker gebruiker = (Gebruiker) account;
				screenitRevisionEntity.setGebruiker(gebruiker);
			}
			if (account instanceof InstellingGebruiker)
			{
				InstellingGebruiker instellingGebruiker = (InstellingGebruiker) account;
				screenitRevisionEntity.setInstellingGebruiker(instellingGebruiker);
			}
			else if (account instanceof Client)
			{
				Client client = (Client) account;
				screenitRevisionEntity.setClient(client);
			}
		}
		screenitRevisionEntity.setKenmerk(RevisionInformationResolver.getKenmerk());
	}
}
