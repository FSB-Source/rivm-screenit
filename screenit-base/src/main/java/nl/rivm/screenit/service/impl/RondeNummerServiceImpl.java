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

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

import nl.rivm.screenit.model.ScreeningRonde;
import nl.rivm.screenit.model.cervix.CervixDossier;
import nl.rivm.screenit.model.cervix.cis.CervixCISHistorie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;

import nl.rivm.screenit.service.RondeNummerService;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS)
public class RondeNummerServiceImpl implements RondeNummerService
{

	@Override
	public <SR extends ScreeningRonde<?, ?, ?, ?>> SR getVorigeRonde(SR huidigeRonde)
	{
		List<SR> rondes = new ArrayList<SR>((List<SR>) huidigeRonde.getDossier().getScreeningRondes());
		rondes = rondes.stream().sorted(Comparator.comparing(SR::getCreatieDatum)).collect(Collectors.toList());
		for (int i = 1; i < rondes.size(); i++)
		{
			if (huidigeRonde.equals(rondes.get(i)))
			{
				return rondes.get(i - 1);
			}
		}
		return null;
	}

	@Override
	public Integer geefRondeNummer(ScreeningRonde ronde)
	{
		List<ScreeningRonde> rondes = new ArrayList<ScreeningRonde>(ronde.getDossier().getScreeningRondes());
		rondes.sort(Comparator.comparing(ScreeningRonde::getCreatieDatum));

		ScreeningRonde ronde0 = null;
		if (ronde.getBevolkingsonderzoek().equals(Bevolkingsonderzoek.CERVIX))
		{
			CervixDossier cervixDossier = (CervixDossier) ronde.getDossier();
			CervixCISHistorie cisHistorie = cervixDossier.getCisHistorie();
			if (cisHistorie != null)
			{
				ronde0 = cisHistorie.getScreeningRonde();
			}
			if (ronde.equals(ronde0))
			{
				return 0;
			}
		}

		for (int i = 0; i < rondes.size(); i++)
		{
			ScreeningRonde screeningRonde = rondes.get(i);
			if (screeningRonde.equals(ronde) && ronde0 == null)
			{
				return i + 1;
			}
			else if (screeningRonde.equals(ronde))
			{
				return i;
			}
		}
		throw new IllegalStateException();
	}
}
