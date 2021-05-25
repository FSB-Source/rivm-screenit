package nl.rivm.screenit.mamma.se.dto;

/*-
 * ========================LICENSE_START=================================
 * screenit-se-rest-bk
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

import java.time.LocalDate;
import java.util.Map;

public class DagverslagDto
{
	private Map<String, DagProductieDto> Dagproductie;

	private DagSynchronisatieDto dagSynchronisatie;

	private DagAfsluitingDto dagafsluiting;

	private LocalDate nietAfgeslotenVanaf;

	public Map<String, DagProductieDto> getDagproductie()
	{
		return Dagproductie;
	}

	public void setDagproductie(Map<String, DagProductieDto> dagproductie)
	{
		Dagproductie = dagproductie;
	}

	public DagSynchronisatieDto getDagSynchronisatie()
	{
		return dagSynchronisatie;
	}

	public void setDagSynchronisatie(DagSynchronisatieDto dagSynchronisatie)
	{
		this.dagSynchronisatie = dagSynchronisatie;
	}

	public DagAfsluitingDto getDagafsluiting()
	{
		return dagafsluiting;
	}

	public void setDagafsluiting(DagAfsluitingDto dagafsluiting)
	{
		this.dagafsluiting = dagafsluiting;
	}

	public LocalDate getNietAfgeslotenVanaf()
	{
		return nietAfgeslotenVanaf;
	}

	public void setNietAfgeslotenVanaf(LocalDate nietAfgeslotenVanaf)
	{
		this.nietAfgeslotenVanaf = nietAfgeslotenVanaf;
	}
}
