package nl.rivm.screenit.huisartsenportaal.validator;

/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
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

import java.lang.reflect.ParameterizedType;
import java.util.Arrays;

import nl.rivm.screenit.huisartsenportaal.model.Huisarts;
import nl.rivm.screenit.huisartsenportaal.repository.HuisartsRepository;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

public abstract class BaseValidator<T> implements Validator
{

	@Autowired
	private HuisartsRepository huisartsRepository;

	private Class[] excludeClasses;

	protected void setExcludeClasses(Class... excludeClasses)
	{
		this.excludeClasses = excludeClasses;
	}

	@Override
	public boolean supports(Class<?> clazz)
	{
		return excludeClasses != null && Arrays.asList(excludeClasses).contains(clazz) || getValidatorClass().equals(clazz);
	}

	@Override
	@SuppressWarnings("unchecked")
	public void validate(Object target, Errors errors)
	{
		validateTarget((T) target, errors);
	}

	public abstract void validateTarget(T target, Errors errors);

	@SuppressWarnings("unchecked")
	private Class<T> getValidatorClass()
	{
		return (Class<T>) ((ParameterizedType) getClass().getGenericSuperclass()).getActualTypeArguments()[0];
	}

	protected Huisarts getIngelogdeHuisarts()
	{
		SecurityContext context = SecurityContextHolder.getContext();

		if (context != null && context.getAuthentication() != null && context.getAuthentication().getPrincipal() instanceof Huisarts)
		{

			Huisarts huisarts = (Huisarts) context.getAuthentication().getPrincipal();
			return huisartsRepository.findByHuisartsportaalId(huisarts.getHuisartsportaalId());
		}
		return null;
	}
}
