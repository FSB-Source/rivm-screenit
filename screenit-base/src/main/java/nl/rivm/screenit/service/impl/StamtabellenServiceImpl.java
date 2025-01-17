package nl.rivm.screenit.service.impl;

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

import java.util.List;

import lombok.AllArgsConstructor;

import nl.rivm.screenit.model.Functie;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.Titel;
import nl.rivm.screenit.repository.algemeen.FunctieRepository;
import nl.rivm.screenit.repository.algemeen.TitelRepository;
import nl.rivm.screenit.service.StamtabellenService;

import org.springframework.stereotype.Service;

@Service
@AllArgsConstructor
public class StamtabellenServiceImpl implements StamtabellenService
{
	private FunctieRepository functieRepository;

	private TitelRepository titelRepository;

	@Override
	public List<Titel> getTitels(Gebruiker medewerker)
	{
		var titels = titelRepository.findByActiefTrueOrderByTitelAsc();

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
		var functies = functieRepository.findByActiefTrueOrderByFunctieAsc();

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
}
