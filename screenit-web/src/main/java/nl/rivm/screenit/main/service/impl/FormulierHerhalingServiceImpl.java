
package nl.rivm.screenit.main.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.util.Iterator;

import nl.topicuszorg.formulieren2.api.definitie.VraagDefinitie;
import nl.topicuszorg.formulieren2.api.instantie.containers.HerhalendBlok;
import nl.topicuszorg.formulieren2.api.resultaat.Antwoord;
import nl.topicuszorg.formulieren2.api.resultaat.EnkelvoudigAntwoord;
import nl.topicuszorg.formulieren2.api.resultaat.FormulierResultaat;
import nl.topicuszorg.formulieren2.api.resultaat.MeervoudigAntwoord;
import nl.topicuszorg.formulieren2.api.service.HerhalingService;

import org.springframework.stereotype.Service;

@Service
public class FormulierHerhalingServiceImpl implements HerhalingService
{

	@SuppressWarnings("rawtypes")
	@Override
	public int getAantalHerhalingen(HerhalendBlok<?> blok, FormulierResultaat resultaat)
	{
		Antwoord<?> antwoord = null;

		VraagDefinitie<?> herhalingProvider = blok.getHerhalingProvider();
		Iterator<Antwoord<?>> iter = resultaat.getAntwoorden().iterator();
		while (iter.hasNext() && antwoord == null)
		{
			Antwoord<?> next = iter.next();
			if (herhalingProvider.equals(next.getVraagInstantie().getVraagDefinitie()))
			{
				antwoord = next;
			}
		}

		int aantal = 0;

		if (antwoord != null)
		{
			Object value = null;
			if (antwoord instanceof EnkelvoudigAntwoord)
			{
				value = ((EnkelvoudigAntwoord) antwoord).getValue();
			}
			else if (antwoord instanceof MeervoudigAntwoord)
			{
				value = ((MeervoudigAntwoord) antwoord).getValues();
			}

			if (value != null)
			{
				if (value instanceof Number)
				{
					aantal = ((Number) value).intValue();
				}
				else
				{
					aantal = Integer.parseInt(value.toString());
				}
			}
		}

		return aantal;
	}
}
