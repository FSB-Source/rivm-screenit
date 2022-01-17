
package nl.rivm.screenit.model.colon.enums;

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

public enum ColonConclusieType
{
	COLOSCOPIE("Coloscopie", ColonUitnodigingsintervalType.INTAKE_COLOSCOPIE_GEPLAND),

	CT_COLOGRAFIE("CT-colografie", ColonUitnodigingsintervalType.INTAKE_CT_COLOGRAFIE),

	ON_HOLD("On hold", ColonUitnodigingsintervalType.INTAKE_ON_HOLD),

	CLIENT_WIL_ANDERE_INTAKELOKATIE("Cli\u00EBnt wil andere intakelokatie", ColonUitnodigingsintervalType.INTAKE_CLIENT_WIL_ANDERE_LOCATIE),

	DOORVERWIJZEN_NAAR_ANDER_CENTRUM("Doorverwijzen naar ander centrum", null),

	NO_SHOW("Intake heeft al plaatsgevonden", ColonUitnodigingsintervalType.INTAKE_NO_SHOW),

	GEEN_VERVOLGONDERZOEK("Geen vervolgonderzoek", null),
	;

	private final String omschrijving;

	private final ColonUitnodigingsintervalType uitnodigingsintervalType;

	private ColonConclusieType(String omschrijving, ColonUitnodigingsintervalType uitnodigingsintervalType)
	{
		this.omschrijving = omschrijving;
		this.uitnodigingsintervalType = uitnodigingsintervalType;
	}

	public ColonUitnodigingsintervalType getUitnodigingsintervalType()
	{
		return uitnodigingsintervalType;
	}

	public String getOmschrijving()
	{
		return omschrijving;
	}

}
