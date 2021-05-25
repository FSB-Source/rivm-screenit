package nl.rivm.screenit.dto.mamma.planning;

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

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import nl.rivm.screenit.model.mamma.enums.MammaMeldingNiveau;

public class PlanningConceptMeldingenDto implements Serializable
{
	public MammaMeldingNiveau niveau;

	public Map<Long, PlanningMeldingenPerSeDto> seMeldingen = new HashMap<>();

	public Map<Long, List<Long>> afsprakenTeVerplaatsen = new HashMap<>();

	static public class PlanningMeldingDto implements Serializable
	{
		public String tekst;

		public MammaMeldingNiveau niveau;
	}

	static public class PlanningMeldingenPerSeDto implements Serializable
	{
		public Long screeningsEenheidId;

		public MammaMeldingNiveau niveau;

		public List<PlanningMeldingDto> meldingen = new ArrayList<>();
	}
}
