package nl.rivm.screenit.main.web.component.validator;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.io.Serializable;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.apache.commons.lang.StringUtils;
import org.apache.wicket.validation.IErrorMessageSource;
import org.apache.wicket.validation.IValidatable;
import org.apache.wicket.validation.IValidationError;
import org.apache.wicket.validation.IValidator;

public class CohortJaarValidator implements IValidator<List<Integer>>
{
	private int jaar;

	public CohortJaarValidator(int jaar)
	{
		this.jaar = jaar;
	}

	@Override
	public void validate(IValidatable<List<Integer>> iValidatable)
	{
		List<Integer> cohortJaren = iValidatable.getValue();
		Set<Integer> vakerVoorkomendeJaren = getJarenVakerVoorkomenInCohort(cohortJaren);
		if (!vakerVoorkomendeJaren.isEmpty())
		{
			iValidatable.error(new IValidationError()
			{

				private static final long serialVersionUID = 1L;

				@Override
				public Serializable getErrorMessage(IErrorMessageSource messageSource)
				{
					String vakerJaren = StringUtils.join(vakerVoorkomendeJaren, ",");
					return (vakerVoorkomendeJaren.size() > 1 ? "Cohort-jaren " : "Cohort-jaar ") + vakerJaren + (vakerVoorkomendeJaren.size() > 1 ? " komen" : " komt")
						+ " vaker voor in jaar: " + jaar;
				}
			});
		}
	}

	public Set<Integer> getJarenVakerVoorkomenInCohort(List<Integer> cohortJaren)
	{
		Set<Integer> gevondenNummers = new HashSet<>();
		Set<Integer> vakerVoorkomendeJaren = new HashSet<>();
		for (int num : cohortJaren)
		{
			if (gevondenNummers.contains(num))
			{
				vakerVoorkomendeJaren.add(num);
			}
			else
			{
				gevondenNummers.add(num);
			}
		}
		return vakerVoorkomendeJaren;
	}
}
