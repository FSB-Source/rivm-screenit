package nl.rivm.screenit.main.model.formulieren;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.model.verslag.DSValue;
import nl.rivm.screenit.service.BaseVerslagService;
import nl.topicuszorg.formulieren2.beanantwoord.EnkelvoudigBeanAntwoord;
import nl.topicuszorg.hibernate.spring.util.ApplicationContextProvider;

import org.apache.commons.lang3.StringUtils;

public class DSValueEnkelvoudigBeanAntwoord extends EnkelvoudigBeanAntwoord<DSValue>
{

	private static final long serialVersionUID = 1L;

	@Override
	public void setValue(DSValue value)
	{
		if (value != null && value.getId() == null && StringUtils.isNotBlank(value.getCode()) && StringUtils.isNotBlank(value.getCodeSystem())
			&& StringUtils.isNotBlank(value.getValueSetName()))
		{
			BaseVerslagService verslagService = ApplicationContextProvider.getApplicationContext().getBean(BaseVerslagService.class);

			innerSetValue(verslagService.getDsValue(value.getCode(), value.getCodeSystem(), value.getValueSetName()));
		}
		else
		{
			innerSetValue(value);
		}
	}
}
