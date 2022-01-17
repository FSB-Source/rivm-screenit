
package nl.rivm.screenit.model.berichten.cda;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

public final class CdaOID
{

	public static final String BSN = "2.16.840.1.113883.2.4.6.3";

	public static final String SECTION = "2.16.840.1.113883.6.1";

	public static final String PA_LAB_ROOT = "2.16.840.1.113883.2.4.3.23.3";

	public static final String T_NUMMER = "2.16.840.1.113883.2.4.99.3.20";

	public static final String TEMPLATEID_ROOT = "2.16.840.1.113883.2.4.3.36.10.";

	public static final String PLAN_OF_CARE_SECTIE = TEMPLATEID_ROOT + "207";

	public static final String ASSESSMENT_SECTIE = TEMPLATEID_ROOT + "205";

	public static final String SPECIMEN_COLLECTION = TEMPLATEID_ROOT + "333";

	public static final String UITSLAG_PATHOLOGIE = TEMPLATEID_ROOT + "336";

	public static final String VERSLAG_IDENTIFICATIE_PATHOLOGIE = TEMPLATEID_ROOT + "338";

	public static final String CONCEPTS_ROOT_OID = "2.16.840.1.113883.2.4.3.36.77.2";

	public static final String CONCEPTS_ROOT_OID_BK = "2.16.840.1.113883.2.4.3.36.77.0.2";

	public static final String CYTOLOGIE_VERSLAG_MONSTER_IDENTIFICATIE = "1.3.6.1.4.1.19376.1.8.9.7";

	private CdaOID()
	{

	}

}
