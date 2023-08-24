
package nl.rivm.screenit.model.colon.enums;

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

import java.util.Arrays;

public enum MdlVervolgbeleid
{

	FOLLOW_UP("183851006", "2.16.840.1.113883.6.96", ColonUitnodigingsintervalType.ENDOSCOPIEVERSLAG_VERWIJZING_POLIKLINIEK),

	POLIEPECTOMIE("311774002", "2.16.840.1.113883.6.96", ColonUitnodigingsintervalType.ENDOSCOPIEVERSLAG_SCOPIE_BEOORDELING_RADICALITEIT),

	SURVEILLANCE("410410006", "2.16.840.1.113883.6.96", ColonUitnodigingsintervalType.ENDOSCOPIEVERSLAG_SURVEILLANCE_1_JAAR),

	SCREENING_CALL("308535007", "2.16.840.1.113883.6.96", ColonUitnodigingsintervalType.ENDOSCOPIEVERSLAG_TERUG_BVO),

	COLONOSCOPY("73761001", "2.16.840.1.113883.6.96", ColonUitnodigingsintervalType.ENDOSCOPIEVERSLAG_VERVOLGSCOPIE),

	COLONOSCOPY_NEW("73761001:260870009=64695001", "2.16.840.1.113883.6.96", ColonUitnodigingsintervalType.ENDOSCOPIEVERSLAG_NIEUWE_SCOPIE),

	CT_COLOGRAFY("418714002", "2.16.840.1.113883.6.96", ColonUitnodigingsintervalType.ENDOSCOPIEVERSLAG_CT_COLOGRAFIE),

	NO_SURVEILLANCE("428119001:363589002=73761001", "2.16.840.1.113883.6.96", ColonUitnodigingsintervalType.ENDOSCOPIEVERSLAG_GEEN_SURVEILLANCE);

	public static final MdlVervolgbeleid[] DEFINITIEF_DEFINITIEF_VERVOLGBELEID = new MdlVervolgbeleid[] { COLONOSCOPY, FOLLOW_UP, NO_SURVEILLANCE, SURVEILLANCE, CT_COLOGRAFY,
		SCREENING_CALL };

	private final String code;

	private final String codeSystem;

	private final ColonUitnodigingsintervalType uitnodigingsintervalType;

	MdlVervolgbeleid(String code, String codeSystem, ColonUitnodigingsintervalType uitnodigingsintervalType)
	{
		this.code = code;
		this.codeSystem = codeSystem;
		this.uitnodigingsintervalType = uitnodigingsintervalType;
	}

	public String getCode()
	{
		return code;
	}

	public String getCodeSystem()
	{
		return codeSystem;
	}

	public ColonUitnodigingsintervalType getUitnodigingsinterval()
	{
		return uitnodigingsintervalType;
	}

	public static boolean isDefinitief(MdlVervolgbeleid beleid)
	{
		return Arrays.asList(DEFINITIEF_DEFINITIEF_VERVOLGBELEID).contains(beleid);
	}
}
