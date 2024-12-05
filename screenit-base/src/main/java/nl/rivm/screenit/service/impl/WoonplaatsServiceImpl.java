package nl.rivm.screenit.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.huisartsenportaal.dto.WoonplaatsDto;
import nl.rivm.screenit.model.Gemeente;
import nl.rivm.screenit.model.Woonplaats;
import nl.rivm.screenit.model.Woonplaats_;
import nl.rivm.screenit.repository.algemeen.WoonplaatsRepository;
import nl.rivm.screenit.service.HuisartsenportaalSyncService;
import nl.rivm.screenit.service.WoonplaatsService;
import nl.rivm.screenit.specification.algemeen.GemeenteSpecification;
import nl.rivm.screenit.specification.algemeen.WoonplaatsSpecification;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject_;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;

@Service
@Slf4j
public class WoonplaatsServiceImpl implements WoonplaatsService
{
	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private HuisartsenportaalSyncService huisartsenportaalSyncService;

	@Autowired
	private WoonplaatsRepository woonplaatsRepository;

	@Override
	public void saveOrUpdateWoonplaats(String plaatscode, String plaatsnaam, String gemcode)
	{
		Map<String, String> parameters = new HashMap<>();
		parameters.put("code", plaatscode);
		List<Woonplaats> results = hibernateService.getByParameters(Woonplaats.class, parameters);

		if (plaatsnaam.contains("\""))
		{
			plaatsnaam = plaatsnaam.replace("\"", "");
			plaatsnaam = plaatsnaam.trim();
		}

		Woonplaats woonplaats = null;
		if (results != null && results.size() == 1)
		{
			woonplaats = results.get(0);
		}
		else
		{
			woonplaats = new Woonplaats();
			woonplaats.setCode(plaatscode);
		}
		woonplaats.setNaam(plaatsnaam);

		parameters.put("code", StringUtils.leftPad(gemcode.trim(), 4, '0'));
		List<Gemeente> gemeentes = hibernateService.getByParameters(Gemeente.class, parameters);
		if (gemeentes != null && gemeentes.size() == 1)
		{
			woonplaats.setGemeente(gemeentes.get(0));
			hibernateService.saveOrUpdate(woonplaats);

			WoonplaatsDto dto = new WoonplaatsDto();
			dto.setScreenitId(woonplaats.getId());
			dto.setNaam(plaatsnaam);
			dto.setCode(plaatscode);
			dto.setGemeente(gemeentes.get(0).getNaam());
			huisartsenportaalSyncService.sendJmsBericht(dto);
		}
		else
		{
			LOG.error("Geen gemeente gevonden voor code {}", gemcode);
		}
	}

	@Override
	public Woonplaats getWoonplaats(String woonplaatsnaam)
	{
		return getWoonplaats(woonplaatsnaam, null);
	}

	@Override
	public Woonplaats getWoonplaats(String woonplaatsnaam, String gemeenteNaam)
	{
		var specification = WoonplaatsSpecification.heeftNaam(woonplaatsnaam).or(WoonplaatsSpecification.heeftNaam("\"" + woonplaatsnaam + "\""));
		if (gemeenteNaam != null)
		{
			specification = specification.and(GemeenteSpecification.heeftNaam(gemeenteNaam).with(Woonplaats_.gemeente)
				.or(GemeenteSpecification.heeftNaam("\"" + gemeenteNaam + "\"").with(Woonplaats_.gemeente)));
		}
		return woonplaatsRepository.findFirst(specification, Sort.by(Sort.Order.asc(AbstractHibernateObject_.ID))).orElse(null);
	}
}
