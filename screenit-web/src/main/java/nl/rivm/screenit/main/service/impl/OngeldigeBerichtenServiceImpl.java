package nl.rivm.screenit.main.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.util.ArrayList;
import java.util.EnumMap;
import java.util.List;
import java.util.Map;

import nl.rivm.screenit.main.dao.OngeldigeBerichtenDao;
import nl.rivm.screenit.main.service.BerichtenZoekFilter;
import nl.rivm.screenit.main.service.OngeldigeBerichtenService;
import nl.rivm.screenit.main.service.VerslagService;
import nl.rivm.screenit.model.BerichtZoekFilter;
import nl.rivm.screenit.model.berichten.cda.MeldingOngeldigCdaBericht;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS)
public class OngeldigeBerichtenServiceImpl implements OngeldigeBerichtenService
{

	private static final Logger LOG = LoggerFactory.getLogger(OngeldigeBerichtenServiceImpl.class);

	@Autowired
	private OngeldigeBerichtenDao ongeldigeBerichtenDao;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private VerslagService verslagService;

	@Override
	public List<MeldingOngeldigCdaBericht> searchOngeldigeBerichten(BerichtZoekFilter filter, long first, long count, String property, boolean ascending)
	{
		return ongeldigeBerichtenDao.searchOngeldigeBerichten(filter, first, count, property, ascending);
	}

	@Override
	public long countOngeldigeBerichten(BerichtZoekFilter filter)
	{
		return ongeldigeBerichtenDao.countOngeldigeBerichten(filter);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void berichtOpnieuwAanbieden(MeldingOngeldigCdaBericht melding)
	{
		LOG.info("CDA bericht " + melding.getOntvangenCdaBericht().getId() + " in MeldingOngeldigCdaBericht " + melding.getId() + " opnieuw aangeboden aan batch");
		verwijderenOngeldigBericht(melding);
		verslagService.berichtOpnieuwVerwerken(melding.getOntvangenCdaBericht());
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void verwijderenOngeldigBericht(MeldingOngeldigCdaBericht meldingOngeldigCdaBericht)
	{
		meldingOngeldigCdaBericht.setActief(false);
		hibernateService.saveOrUpdate(meldingOngeldigCdaBericht);
		LOG.info("MeldingOngeldigCdaBericht " + meldingOngeldigCdaBericht.getId() + " gedeactiveerd");
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void herverwerkAlleBerichten(BerichtenZoekFilter berichtFilter)
	{
		List<MeldingOngeldigCdaBericht> ongeldigeBerichten = ongeldigeBerichtenDao.searchOngeldigeBerichten(berichtFilter, -1, -1, "datum", false);

		Map<Bevolkingsonderzoek, List<Long>> ongeldigeBerichtenIdsPerBvo = new EnumMap<>(Bevolkingsonderzoek.class);
		for (MeldingOngeldigCdaBericht ongeldigCdaBericht : ongeldigeBerichten)
		{
			if (Boolean.TRUE.equals(ongeldigCdaBericht.getActief()) && Boolean.TRUE.equals(ongeldigCdaBericht.getHerstelbaar()))
			{
				verwijderenOngeldigBericht(ongeldigCdaBericht);
				Bevolkingsonderzoek bevolkingsonderzoek = ongeldigCdaBericht.getOntvangenCdaBericht().getBerichtType().getBevolkingsonderzoek();
				List<Long> ongeldigeBerichtenIds = ongeldigeBerichtenIdsPerBvo.get(bevolkingsonderzoek);
				if (ongeldigeBerichtenIds == null)
				{
					ongeldigeBerichtenIds = new ArrayList<>();
					ongeldigeBerichtenIdsPerBvo.put(bevolkingsonderzoek, ongeldigeBerichtenIds);
				}
				ongeldigeBerichtenIds.add(ongeldigCdaBericht.getOntvangenCdaBericht().getId());
			}
		}

		ongeldigeBerichtenIdsPerBvo.forEach((bvo, ids) -> verslagService.berichtenOpnieuwVerwerken(ids, bvo));
	}
}
