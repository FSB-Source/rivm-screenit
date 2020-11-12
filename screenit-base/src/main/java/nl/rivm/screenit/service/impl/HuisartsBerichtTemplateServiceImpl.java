package nl.rivm.screenit.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import nl.rivm.screenit.dao.HuisartsBerichtTemplateDao;
import nl.rivm.screenit.model.HuisartsBerichtTemplate;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.HuisartsBerichtType;
import nl.rivm.screenit.service.HuisartsBerichtTemplateService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS)
public class HuisartsBerichtTemplateServiceImpl implements HuisartsBerichtTemplateService
{
	@Autowired
	private HuisartsBerichtTemplateDao templateDao;

	@Override
	@Transactional(propagation = Propagation.SUPPORTS)
	public HuisartsBerichtTemplate getTemplateByType(HuisartsBerichtType type)
	{
		return getTemplateDao().getTemplateByType(type);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void saveOrUpdate(HuisartsBerichtTemplate template)
	{
		getTemplateDao().saveOrUpdate(template);
	}

	@Override
	public List<HuisartsBerichtType> getTemplateFromBevolkingsonderzoek(List<Bevolkingsonderzoek> onderzoeken)
	{
		return Arrays.stream(HuisartsBerichtType.values())
			.filter(t -> onderzoeken.contains(t.getOnderzoek()))
			.collect(Collectors.toList());
	}

	public HuisartsBerichtTemplateDao getTemplateDao()
	{
		return templateDao;
	}

	public void setTemplateDao(HuisartsBerichtTemplateDao templateDao)
	{
		this.templateDao = templateDao;
	}

}
