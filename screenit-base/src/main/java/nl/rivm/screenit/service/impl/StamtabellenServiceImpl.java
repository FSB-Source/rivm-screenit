
package nl.rivm.screenit.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.dao.StamtabellenDao;
import nl.rivm.screenit.model.Functie;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.Titel;
import nl.rivm.screenit.service.StamtabellenService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS)
public class StamtabellenServiceImpl implements StamtabellenService
{

	@Autowired
	private StamtabellenDao stamtabellenDao;

	@Override
	public List<Titel> getTitels(Gebruiker medewerker)
	{
		List<Titel> titels = stamtabellenDao.getTitels();

		if (medewerker != null && medewerker.getTitel() != null)
		{
			addInactiveTitel(medewerker.getTitel(), titels);
		}

		return titels;
	}

	private void addInactiveTitel(Titel titel, List<Titel> titels)
	{
		boolean contains = false;
		for (Titel titelInList : titels)
		{
			if (titelInList.getId().equals(titel.getId()))
			{
				contains = true;
			}
		}

		if (!contains)
		{
			titels.add(titel);
		}
	}

	@Override
	public List<Functie> getFuncties(Gebruiker medewerker)
	{
		List<Functie> functies = stamtabellenDao.getFuncties();

		if (medewerker != null && medewerker.getFunctie() != null)
		{
			boolean contains = false;
			for (Functie functieInList : functies)
			{
				if (functieInList.getId().equals(medewerker.getFunctie().getId()))
				{
					contains = true;
				}
			}

			if (!contains)
			{
				functies.add(medewerker.getFunctie());
			}
		}

		return functies;
	}

	@Override
	public Functie getFunctie(String functie)
	{
		return stamtabellenDao.getFunctie(functie);
	}

}
