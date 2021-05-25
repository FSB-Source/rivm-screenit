package nl.rivm.screenit.model.colon;

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

import nl.rivm.screenit.model.colon.enums.ColonConclusieType;

public enum ConclusieTypeFilter
{
	GEEN_CONCLUSIE(),

	COLOSCOPIE(ColonConclusieType.COLOSCOPIE),

	CT_COLOGRAFIE(ColonConclusieType.CT_COLOGRAFIE),

	ON_HOLD(ColonConclusieType.ON_HOLD),

	ANDERE_BVO_INTAKELOCATIE(ColonConclusieType.CLIENT_WIL_ANDERE_INTAKELOKATIE, ColonConclusieType.DOORVERWIJZEN_NAAR_ANDER_CENTRUM),

	NO_SHOW(ColonConclusieType.NO_SHOW),

	GEEN_VERVOLGONDERZOEK(ColonConclusieType.GEEN_VERVOLGONDERZOEK);

	private ColonConclusieType[] conclusieTypes;

	public static ConclusieTypeFilter[] getOpenstaandeAfspraken()
	{
		return new ConclusieTypeFilter[] { GEEN_CONCLUSIE, ON_HOLD };
	}

	public static ConclusieTypeFilter[] getAfgerondeAfspraken()
	{
		return new ConclusieTypeFilter[] { ANDERE_BVO_INTAKELOCATIE, NO_SHOW, COLOSCOPIE, CT_COLOGRAFIE, GEEN_VERVOLGONDERZOEK, GEEN_CONCLUSIE, ON_HOLD };
	}

	ConclusieTypeFilter(ColonConclusieType... conclusieTypes)
	{
		this.conclusieTypes = conclusieTypes;
	}

	public ColonConclusieType[] getConclusieTypes()
	{
		return conclusieTypes;
	}
}
