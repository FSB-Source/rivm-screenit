package nl.rivm.screenit.model.mamma.enums;

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

import nl.rivm.screenit.model.INaam;

public enum MammaLaesieType implements INaam
{

	MASSA("Massa", "MASSA"),
	CALCIFICATIES("Calcificaties", "CALCIFICATIES"),
	ARCHITECTUURVERSTORING("Architectuurverstoring", "ARCHITECTUURVERSTORING"),
	ASYMMETRIE("Asymmetrie", "ASYMMETRIE"),
	LEGACY_ARCHITECTUURVERSTORING_MET_CALCIFICATIES("Architectuurverstoring met calcificaties", "LEGACY_ARCHITECTUURVERSTORING_MET_CALCIFICATIES"),
	LEGACY_MASSA_MET_ARCHITECTUURVERSTORING("Massa met architectuurverstoring", "LEGACY_MASSA_MET_ARCHITECTUURVERSTORING"),
	LEGACY_CONFORM("Conform", "LEGACY_CONFORM"),
	LEGACY_GEEN_BIJZONDERHEDEN("Geen bijzonderheden", "LEGACY_GEEN_BIJZONDERHEDEN"),
	LEGACY_MASSA_MET_SPICULAE("Massa met spiculae", "LEGACY_MASSA_MET_SPICULAE"),
	LEGACY_PROJECTIE_NAAR_LINKS("Projectie naar links", "LEGACY_PROJECTIE_NAAR_LINKS"),
	LEGACY_PROJECTIE_NAAR_RECHTS("Projectie naar rechts", "LEGACY_PROJECTIE_NAAR_RECHTS"),
	LEGACY_MASSA_MET_CALCIFICATIES("Massa met calcificaties", "LEGACY_MASSA_MET_CALCIFICATIES"),
	LEGACY_MASSA_MET_SPICULAE_EN_CALCIFICATIES("Massa met spiculae en calcificaties", "LEGACY_MASSA_MET_SPICULAE_EN_CALCIFICATIES"),
	LEGACY_MARKERING("Markering", "LEGACY_MARKERING"),
	LEGACY_BENIGNE_KALK("Begine kalk", "LEGACY_BENIGNE_KALK"),
	LEGACY_PLUS("Annotatie", "LEGACY_PLUS");

	private final String naam;

	private final String svgFileName;

	MammaLaesieType(String naam, String svgFileName)
	{
		this.naam = naam;
		this.svgFileName = svgFileName;
	}

	@Override
	public String getNaam()
	{
		return this.naam;
	}

	public String getSvgFileName()
	{
		return svgFileName;
	}
}
