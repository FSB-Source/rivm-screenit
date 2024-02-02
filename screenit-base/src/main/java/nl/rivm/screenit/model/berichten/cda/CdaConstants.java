
package nl.rivm.screenit.model.berichten.cda;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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

public final class CdaConstants
{

	public static final String CUSTODIAN_URA_PATH = "custodian:assignedCustodian:representedCustodianOrganization:ids";

	public static final String PATIENT_IDS_PATH = "recordTargets:patientRole:ids";

	public static final String AANVANG_VERRICHTING_DATUM_MDL_PATH = "documentationOves:serviceEvent:effectiveTime:low:value";

	public static final String AANVANG_VERRICHTING_DATUM_PA_PATH = "effectiveTime:value";

	public static final String CYTOLOGIE_VERSLAG_MONSTER_IDENTIFICATIE_IDS = "component:structuredBody:components:section[2.16.840.1.113883.2.4.3.36.10.213]:entries:organizer:components:procedure[1.3.6.1.4.1.19376.1.3.1.2]:participants:participantRole:ids";

	private CdaConstants()
	{

	}

}
