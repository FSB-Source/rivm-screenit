package nl.rivm.screenit.model.colon;

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

import nl.rivm.screenit.model.colon.enums.ColonConclusieType;

public enum ConclusieTypeFilter
{
	GEEN_CONCLUSIE(),

	COLOSCOPIE(ColonConclusieType.COLOSCOPIE),

	CT_COLOGRAFIE(ColonConclusieType.CT_COLOGRAFIE),

	ON_HOLD(ColonConclusieType.ON_HOLD),

	CLIENT_WIL_ANDERE_INTAKELOKATIE(ColonConclusieType.CLIENT_WIL_ANDERE_INTAKELOKATIE),

	DOORVERWIJZEN_NAAR_ANDER_CENTRUM(ColonConclusieType.DOORVERWIJZEN_NAAR_ANDER_CENTRUM),

	NO_SHOW(ColonConclusieType.NO_SHOW),

	GEEN_VERVOLGONDERZOEK(ColonConclusieType.GEEN_VERVOLGONDERZOEK);

	private final List<ColonConclusieType> conclusieTypes;

	public static List<ConclusieTypeFilter> getOpenstaandeAfspraken()
	{
		return List.of(GEEN_CONCLUSIE, ON_HOLD, DOORVERWIJZEN_NAAR_ANDER_CENTRUM);
	}

	public static List<ConclusieTypeFilter> getAfgerondeAfspraken()
	{
		return List.of(CLIENT_WIL_ANDERE_INTAKELOKATIE, NO_SHOW, COLOSCOPIE, CT_COLOGRAFIE, GEEN_VERVOLGONDERZOEK, GEEN_CONCLUSIE, ON_HOLD);
	}

	ConclusieTypeFilter(ColonConclusieType... conclusieTypes)
	{
		this.conclusieTypes = List.of(conclusieTypes);
	}

	public List<ColonConclusieType> getConclusieTypes()
	{
		return conclusieTypes;
	}
}
