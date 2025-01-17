package nl.rivm.screenit.mamma.planning.model;

/*-
 * ========================LICENSE_START=================================
 * screenit-planning-bk
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

import java.time.LocalDate;
import java.util.Comparator;
import java.util.NavigableSet;
import java.util.TreeSet;

import lombok.Getter;
import lombok.Setter;

@Getter
public class PopulatieMetStreefDatum implements Comparable<PopulatieMetStreefDatum>
{
	@Getter
	private static final Comparator<PlanningClient> clientComparator = Comparator.comparing(PlanningClient::getHuidigeStreefDatum, Comparator.nullsFirst(Comparator.naturalOrder()))
		.thenComparing(PlanningClient::getGeboorteDatum).thenComparing(PlanningClient::getId);

	private final NavigableSet<PlanningClient> clienten = new TreeSet<>(clientComparator);

	@Setter
	private LocalDate uitnodigingStreefDatum;

	private boolean voorUitstelClient = false;

	public static PopulatieMetStreefDatum voorUitstelClient(PlanningClient client)
	{
		PopulatieMetStreefDatum populatieMetStreefDatum = new PopulatieMetStreefDatum();
		populatieMetStreefDatum.voorUitstelClient = true;
		populatieMetStreefDatum.clienten.add(client);
		populatieMetStreefDatum.uitnodigingStreefDatum = client.getUitstelStreefDatum();
		return populatieMetStreefDatum;
	}

	@Override
	public int compareTo(PopulatieMetStreefDatum populatieMetStreefDatum)
	{
		int compareTo = populatieMetStreefDatum.isUitgenodigd().compareTo(isUitgenodigd());
		if (compareTo != 0)
		{
			return compareTo;
		}

		compareTo = uitnodigingStreefDatum.compareTo(populatieMetStreefDatum.uitnodigingStreefDatum);
		if (compareTo != 0)
		{
			return compareTo;
		}
		return Integer.compare(hashCode(), populatieMetStreefDatum.hashCode());
	}

	private Boolean isUitgenodigd()
	{
		return clienten.stream().anyMatch(
			client -> client.getUitstelStandplaats() == null
				&& client.isUitgenodigdHuidigeStandplaatsRonde()
				&& !client.isUitgenodigdHuidigeStandplaatsRondeIsGeforceerd()
				|| Boolean.TRUE.equals(client.getUitgenodigdNaUitstel()));
	}
}
