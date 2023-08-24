package nl.rivm.screenit.model.mamma.enums;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

public enum MammaAnnotatieIcoonType
{
	BORSTVERGROTING("borstvergroting"),
	DUBBELZIJDIGE_BORSTVERKLEINING("dubbelzijdige-borstverkleining"),
	EENZIJDIGE_BORSTVERKLEINING("eenzijdige-borstverkleining"),
	GROTER_DAN("groter-dan"),
	INGETROKKEN_TEPEL("ingetrokken-tepel"),
	KLEINER_DAN("kleiner-dan"),
	LITTEKEN_HORIZONTAAL("litteken-horizontaal"),
	LITTEKEN_LBRO("litteken-lo-rb"),
	LITTEKEN_RBLO("litteken-rb-lo"),
	LITTEKEN_VERTICAAL("litteken-verticaal"),
	UITWENDIGE_AFWIJKING("uitwendige-afwijking"),
	WRAT("wrat"),

	LEGACY_PLUS(null),
	SIGNALERING_ARCHITECTUURVERSTORING(null),
	SIGNALERING_ASYMMETRIE(null),
	SIGNALERING_CALCIFICATIES(null),
	SIGNALERING_MASSA(null),
	LEGACY_ARCHITECTUURVERSTORING_MET_CALCIFICATIES(null),
	LEGACY_MASSA_MET_ARCHITECTUURVERSTORING(null),
	LEGACY_CONFORM(null),
	LEGACY_GEEN_BIJZONDERHEDEN(null),
	LEGACY_MASSA_MET_SPICULAE(null),
	LEGACY_PROJECTIE_NAAR_LINKS(null),
	LEGACY_PROJECTIE_NAAR_RECHTS(null),
	LEGACY_MASSA_MET_CALCIFICATIES(null),
	LEGACY_MASSA_MET_SPICULAE_EN_CALCIFICATIES(null),
	LEGACY_MARKERING(null),
	LEGACY_BENIGNE_KALK(null);

	final String svgFileName;

	MammaAnnotatieIcoonType(String svgFileName)
	{
		this.svgFileName = svgFileName;
	}

	public String getSvgFileName()
	{
		return svgFileName;
	}

}
