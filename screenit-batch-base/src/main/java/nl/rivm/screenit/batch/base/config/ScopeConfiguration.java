package nl.rivm.screenit.batch.base.config;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-base
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

import org.springframework.batch.core.scope.JobScope;
import org.springframework.batch.core.scope.StepScope;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration(
	proxyBeanMethods = false
)
public class ScopeConfiguration
{
	private static final StepScope stepScope;

	private static final JobScope jobScope = new JobScope();

	ScopeConfiguration()
	{
	}

	@Bean
	public static StepScope stepScope()
	{
		return stepScope;
	}

	@Bean
	public static JobScope jobScope()
	{
		return jobScope;
	}

	static
	{
		jobScope.setAutoProxy(false);
		stepScope = new StepScope();
		stepScope.setAutoProxy(false);
	}
}
