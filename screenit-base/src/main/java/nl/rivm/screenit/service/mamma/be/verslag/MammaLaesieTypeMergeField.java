package nl.rivm.screenit.service.mamma.be.verslag;

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

import nl.rivm.screenit.model.INaam;

public enum MammaLaesieTypeMergeField implements INaam
{
	TABLE_MASSA("Massa", "massa"),
	TABLE_CALCIFICATIES("Calificaties", "calcificaties"),
	TABLE_ARCHITECTUUR_VERSTORING("Architectuur verstoring", "architectuur_verstoring"),
	TABLE_ASYMMETRIE("Asymmetrie", "asymmetrie"),
	_BK_LAESIE_ZIJDE("Laesie zijde", "_BK_LAESIE_ZIJDE"),
	_BK_LAESIE_KWADRANT("Laesie kwadrant", "_BK_LAESIE_KWADRANT"),
	_BK_LAESIE_DIEPTE("Laesie diepte", "_BK_LAESIE_DIEPTE"),
	_BK_LAESIE_GROOTTE("Laesie grootte", "_BK_LAESIE_GROOTTE"),
	_BK_LAESIE_MASSA_VORM("Vorm", "_BK_LAESIE_MASSA_VORM"),
	_BK_LAESIE_MASSA_DENSITEIT("Densiteit", "_BK_LAESIE_MASSA_DENSITEIT"),
	_BK_LAESIE_MASSA_BEGRENZING("Massa begrenzing", "_BK_LAESIE_MASSA_BEGRENZING"),
	_BK_LAESIE_CALC_VERD_VORM("Verdachte vorm", "_BK_LAESIE_CALC_VERD_VORM"),
	_BK_LAESIE_CALC_DISTRIBUTIE("Distributie", "_BK_LAESIE_CALC_DISTRIBUTIE"),
	_BK_LAESIE_ASSYMETRIE_SPEC("Type asymmetrie", "_BK_LAESIE_ASSYMETRIE_SPEC"),
	_AFBEELDING_LINKERBORST_HORIZONTALE_DOORSNEDE("_AFBEELDING_LINKERBORST_HORIZONTALE_DOORSNEDE", "_AFBEELDING_LINKERBORST_HORIZONTALE_DOORSNEDE"),
	_AFBEELDING_LINKERBORST_VERTICALE_DOORSNEDE("_AFBEELDING_LINKERBORST_VERTICALE_DOORSNEDE", "_AFBEELDING_LINKERBORST_VERTICALE_DOORSNEDE"),
	_AFBEELDING_RECHTERBORST_HORIZONTALE_DOORSNEDE("_AFBEELDING_RECHTERBORST_HORIZONTALE_DOORSNEDE", "_AFBEELDING_RECHTERBORST_HORIZONTALE_DOORSNEDE"),
	_AFBEELDING_RECHTERBORST_VERTICALE_DOORSNEDE("_AFBEELDING_RECHTERBORST_VERTICALE_DOORSNEDE", "_AFBEELDING_RECHTERBORST_VERTICALE_DOORSNEDE"),
	_BK_LAESIE_VOLG_NR("Laesie volgnummer", "_BK_LAESIE_VOLG_NR");

	private String naam;

	private String mergeField;

	MammaLaesieTypeMergeField(String naam, String mergeField)
	{
		this.naam = naam;
		this.mergeField = mergeField;
	}

	@Override
	public String getNaam()
	{
		return naam;
	}

	public String getMergeField()
	{
		return mergeField;
	}

}
