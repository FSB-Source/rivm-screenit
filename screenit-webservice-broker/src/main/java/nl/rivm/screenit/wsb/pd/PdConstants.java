
package nl.rivm.screenit.wsb.pd;

/*-
 * ========================LICENSE_START=================================
 * screenit-webservice-broker
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

public final class PdConstants
{
	private PdConstants()
	{
	}

	public static final String OID_DOC_TYPE = "2.16.840.1.113883.6.1";

	public static final String DOC_TYPE_MDL = "18746-8";

	public static final String DOC_TYPE_PA = "11526-1";

	public static final String DOC_TYPE_PA_BK_FOLLOW_UP = "34819-3";

	public static final String TEMPLATE_ID_PA_CYTOLOGY = "2.16.840.1.113883.2.4.3.36.10.30";

	public static final String TEMPLATE_ID_PA_DK = "2.16.840.1.113883.2.4.3.36.10.23";

	public static final String TEMPLATE_ID_PA_FOLLOW_UP = "2.16.840.1.113883.2.4.3.36.10.31";
}
