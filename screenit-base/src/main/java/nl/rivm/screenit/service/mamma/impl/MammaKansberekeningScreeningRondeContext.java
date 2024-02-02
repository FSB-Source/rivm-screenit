package nl.rivm.screenit.service.mamma.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.time.LocalDate;
import java.util.HashSet;
import java.util.NavigableMap;
import java.util.Set;
import java.util.TreeMap;

import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaMammografie;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.MammaUitnodiging;
import nl.rivm.screenit.model.mamma.enums.MammaAfspraakStatus;
import nl.rivm.screenit.util.DateUtil;

public class MammaKansberekeningScreeningRondeContext
{

	MammaScreeningRonde screeningRonde;

	LocalDate screeningRondeCreatieDatum;

	MammaUitnodiging uitnodiging;

	MammaDossier dossier;

	LocalDate geboorteDatum;

	NavigableMap<LocalDate, MammaScreeningRonde> screeningRondeNavigableMap = new TreeMap<>();

	NavigableMap<LocalDate, MammaUitnodiging> uitnodigingNavigableMap = new TreeMap<>();

	NavigableMap<LocalDate, MammaAfspraak> afspraakNavigableMap = new TreeMap<>();

	NavigableMap<LocalDate, MammaOnderzoek> onderzoekNavigableMap = new TreeMap<>();

	NavigableMap<LocalDate, MammaMammografie> mammografieNavigableMap = new TreeMap<>();

	NavigableMap<LocalDate, MammaBeoordeling> laatsteBeoordelingNavigableMap = new TreeMap<>();

	Set<MammaScreeningRonde> screeningRondeMetOnderzoekSet = new HashSet<>();

	Set<MammaAfspraak> afspraakMetOnderzoekSet = new HashSet<>();

	public MammaKansberekeningScreeningRondeContext(MammaScreeningRonde screeningRonde)
	{
		this.screeningRonde = screeningRonde;
		this.dossier = screeningRonde.getDossier();

		screeningRondeCreatieDatum = DateUtil.toLocalDate(screeningRonde.getCreatieDatum());

		init();
	}

	public MammaKansberekeningScreeningRondeContext(MammaDossier dossier)
	{
		this.dossier = dossier;

		init();
	}

	private void init()
	{
		geboorteDatum = DateUtil.toLocalDate(dossier.getClient().getPersoon().getGeboortedatum());

		for (MammaScreeningRonde ronde : dossier.getScreeningRondes())
		{
			screeningRondeNavigableMap.put(DateUtil.toLocalDate(ronde.getCreatieDatum()), ronde);

			for (MammaUitnodiging uitnodiging : ronde.getUitnodigingen())
			{
				uitnodigingNavigableMap.put(DateUtil.toLocalDate(uitnodiging.getCreatieDatum()), uitnodiging);

				for (MammaAfspraak afspraak : uitnodiging.getAfspraken())
				{
					if (MammaAfspraakStatus.NIET_GEANNULEERD.contains(afspraak.getStatus()))
					{
						afspraakNavigableMap.put(DateUtil.toLocalDate(afspraak.getVanaf()), afspraak);

						MammaOnderzoek onderzoek = afspraak.getOnderzoek();
						if (onderzoek != null && onderzoek.isDoorgevoerd())
						{
							onderzoekNavigableMap.put(DateUtil.toLocalDate(onderzoek.getAfgerondOp()), onderzoek);
							screeningRondeMetOnderzoekSet.add(ronde);
							afspraakMetOnderzoekSet.add(afspraak);

							MammaMammografie mammografie = onderzoek.getMammografie();
							if (mammografie != null && mammografie.getAfgerondOp() != null)
							{
								mammografieNavigableMap.put(DateUtil.toLocalDate(mammografie.getAfgerondOp()), mammografie);
							}

							MammaBeoordeling laatsteBeoordeling = onderzoek.getLaatsteBeoordeling();
							if (laatsteBeoordeling != null)
							{
								laatsteBeoordelingNavigableMap.put(DateUtil.toLocalDate(laatsteBeoordeling.getStatusDatum()), laatsteBeoordeling);
							}
						}
					}
				}
			}
		}
	}
}
