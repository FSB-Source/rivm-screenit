package nl.rivm.screenit.huisartsenportaal.controller;

/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
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

import nl.rivm.screenit.huisartsenportaal.model.Huisarts;
import nl.rivm.screenit.huisartsenportaal.repository.HuisartsRepository;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.core.context.SecurityContextHolder;

public class BaseController
{
	@Autowired
	private HuisartsRepository huisartsRepository;

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
