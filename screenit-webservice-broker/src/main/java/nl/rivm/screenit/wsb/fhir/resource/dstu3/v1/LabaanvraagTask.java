package nl.rivm.screenit.wsb.fhir.resource.dstu3.v1;

/*-
 * ========================LICENSE_START=================================
 * screenit-webservice-broker
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.Objects;

import org.hl7.fhir.dstu3.model.Identifier;
import org.hl7.fhir.dstu3.model.Task;

import ca.uhn.fhir.model.api.annotation.ResourceDef;

@ResourceDef()
public class LabaanvraagTask extends Task implements LabaanvraagResource
{

	public String getClientBsn()
	{
		if (getFor() != null
			&& getFor().getIdentifier() != null
			&& getFor().getIdentifier().getSystem() != null
			&& getFor().getIdentifier().getSystem().equals(CodeSystem.BSN.getUrl()))
		{
			return getFor().getIdentifier().getValue();
		}
		return null;
	}

	public String getIndividueleAgb()
	{
		if (getRequester() != null
			&& getRequester().getAgent() != null
			&& getRequester().getAgent().getIdentifier() != null
			&& getRequester().getAgent().getIdentifier().getSystem() != null
			&& getRequester().getAgent().getIdentifier().getSystem().equals(CodeSystem.AGB.getUrl()))
		{
			return getRequester().getAgent().getIdentifier().getValue();
		}
		return null;
	}

	public String getPraktijkAgb()
	{
		if (getRequester() != null
			&& getRequester().getOnBehalfOf() != null
			&& getRequester().getOnBehalfOf().getIdentifier() != null
			&& getRequester().getOnBehalfOf().getIdentifier().getSystem() != null
			&& getRequester().getOnBehalfOf().getIdentifier().getSystem().equals(CodeSystem.AGB.getUrl()))
		{
			return getRequester().getOnBehalfOf().getIdentifier().getValue();
		}
		return null;
	}

	public String getMonsterId()
	{
		return getIdentifierValue(CodeSystem.MONSTER_ID);
	}

	public String getControleLetters()
	{
		return getIdentifierValue(CodeSystem.CONTROLELETTERS);
	}

	private String getIdentifierValue(CodeSystem system)
	{
		return getIdentifier() != null
			? getIdentifier()
				.stream()
				.filter(identifier -> system.getUrl().equals(identifier.getSystem()))
				.map(Identifier::getValue)
				.filter(Objects::nonNull)
				.findFirst()
				.orElse(null)
			: null;
	}

}
