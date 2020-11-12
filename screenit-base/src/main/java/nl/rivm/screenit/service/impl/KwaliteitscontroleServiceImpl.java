
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

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.colon.SKMLInterneControleSet;
import nl.rivm.screenit.model.colon.SKMLSentineelControleBarcode;
import nl.rivm.screenit.model.colon.enums.SKMLSentineelControleType;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.service.KwaliteitscontroleService;
import nl.rivm.screenit.service.LogService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS)
public class KwaliteitscontroleServiceImpl implements KwaliteitscontroleService
{

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private LogService logService;

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public List<SKMLSentineelControleBarcode> createOrGetAllSentineelControles()
	{
		List<SKMLSentineelControleBarcode> cups = hibernateService.loadAll(SKMLSentineelControleBarcode.class);
		if (cups.size() < SKMLSentineelControleType.values().length)
		{
			for (SKMLSentineelControleType typeCup : SKMLSentineelControleType.values())
			{
				SKMLSentineelControleBarcode foundCup = null;
				for (SKMLSentineelControleBarcode cup : cups)
				{
					if (typeCup.equals(cup.getSentineelType()))
					{
						foundCup = cup;
						break;
					}
				}
				if (foundCup == null)
				{
					foundCup = new SKMLSentineelControleBarcode();
					foundCup.setSentineelType(typeCup);
					cups.add(foundCup);
					hibernateService.saveOrUpdate(foundCup);
				}
			}
		}
		List<SKMLSentineelControleBarcode> tempCups = new ArrayList<>(cups);
		Collections.sort(tempCups);
		return cups;
	}

	@Override
	public List<SKMLInterneControleSet> createOrGetAllInterneControleSets()
	{
		List<SKMLInterneControleSet> interneControleSets = hibernateService.loadAll(SKMLInterneControleSet.class);
		if (interneControleSets.size() < 4)
		{
			for (int i = 1; i <= 4; i++)
			{
				SKMLInterneControleSet foundSetConfig = null;
				for (SKMLInterneControleSet interneControle : interneControleSets)
				{
					if (Integer.valueOf(i).equals(interneControle.getVolgorde()))
					{
						foundSetConfig = interneControle;
						break;
					}
				}
				if (foundSetConfig == null)
				{
					foundSetConfig = new SKMLInterneControleSet();
					foundSetConfig.setVolgorde(i);
					interneControleSets.add(foundSetConfig);
					hibernateService.saveOrUpdate(foundSetConfig);
				}
			}
		}
		List<SKMLInterneControleSet> tempInterneControleSets = new ArrayList<>(interneControleSets);
		Collections.sort(tempInterneControleSets);
		return tempInterneControleSets;
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void saveOrUpdateInterneIdSets(List<SKMLInterneControleSet> list, InstellingGebruiker instellingGebruiker)
	{
		boolean succes = true;
		for (SKMLInterneControleSet set : list)
		{
			if (StringUtils.isNotBlank(set.getControleTekst()) && set.getQbaseId() == null)
			{
				succes = false;
			}
			else if (StringUtils.isBlank(set.getControleTekst()) && set.getQbaseId() != null)
			{
				succes = false;
			}
			else
			{
				hibernateService.saveOrUpdate(set);
			}
		}
		if (succes)
		{
			logService.logGebeurtenis(LogGebeurtenis.SKML_INTERN_CONTROLE_SET_GEWIJZIGD, instellingGebruiker, Bevolkingsonderzoek.COLON);
		}
		else
		{
			throw new IllegalArgumentException();
		}
	}
}
