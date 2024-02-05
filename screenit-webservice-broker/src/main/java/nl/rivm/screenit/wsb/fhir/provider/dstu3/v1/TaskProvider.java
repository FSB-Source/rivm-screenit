package nl.rivm.screenit.wsb.fhir.provider.dstu3.v1;

/*-
 * ========================LICENSE_START=================================
 * screenit-webservice-broker
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

import nl.rivm.screenit.wsb.fhir.resource.dstu3.v1.LabaanvraagTask;
import nl.rivm.screenit.wsb.fhir.validator.LabaanvraagValidator;

import org.hl7.fhir.instance.model.api.IBaseResource;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.rest.annotation.Create;
import ca.uhn.fhir.rest.annotation.ResourceParam;
import ca.uhn.fhir.rest.api.MethodOutcome;
import ca.uhn.fhir.rest.server.exceptions.UnprocessableEntityException;
import ca.uhn.fhir.validation.ValidationResult;

public class TaskProvider extends BaseResourceProvider
{

	@Override
	public Class<? extends IBaseResource> getResourceType()
	{
		return LabaanvraagTask.class;
	}

	@Create()
	public MethodOutcome create(@ResourceParam LabaanvraagTask task)
	{
		FhirContext context = FhirContext.forDstu3();
		ValidationResult result = getFhirValidator(context)
			.validateWithResult(task);

		if (result.isSuccessful()) 
		{
			LabaanvraagValidator labAanvraagValidator = new LabaanvraagValidator();
			if (labAanvraagValidator.validate(task).isSuccesvol()) 
			{
				return new MethodOutcome()
					.setOperationOutcome(result.toOperationOutcome());
			}
			throw new UnprocessableEntityException(context, labAanvraagValidator.toOperationOutcome());
		}
		throw new UnprocessableEntityException(context, result.toOperationOutcome());
	}

}
