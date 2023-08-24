package nl.rivm.screenit.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import nl.rivm.screenit.dao.GbaBaseDao;
import nl.rivm.screenit.dao.GemeenteDao;
import nl.rivm.screenit.model.BMHKLaboratorium;
import nl.rivm.screenit.model.Gemeente;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.UitnodigingsGebied;
import nl.rivm.screenit.model.colon.ColoscopieCentrum;
import nl.rivm.screenit.model.colon.ColoscopieCentrumColonCapaciteitVerdeling;
import nl.rivm.screenit.service.GemeenteService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS)
public class GemeenteServiceImpl implements GemeenteService
{

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private GemeenteDao gemeenteDao;

	@Autowired
	private GbaBaseDao gbaBaseDao;

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void voegGemeenteToe(Gemeente gemeente)
	{
		UitnodigingsGebied uitnodigingsGebied = new UitnodigingsGebied();
		uitnodigingsGebied.setGemeente(gemeente);
		uitnodigingsGebied.setNaam(gemeente.getNaam());

		hibernateService.saveOrUpdate(gemeente);
		hibernateService.saveOrUpdate(uitnodigingsGebied);
	}

	@Override
	public List<Gemeente> getAllGekoppeldeGemeentes()
	{
		return gemeenteDao.getAllGekoppeldeGemeentes();
	}

	@Override
	public List<Gemeente> getGemeentesZonderScreeningOrganisatie(ScreeningOrganisatie screeningOrganisatie)
	{
		return gemeenteDao.getGemeentesZonderScreeningOrganisatie(screeningOrganisatie);
	}

	@Override
	public List<Gemeente> getGemeentesZonderBMHKLaboratorium(BMHKLaboratorium bmhkLaboratorium)
	{
		return gemeenteDao.getGemeentesZonderBMHKLaboratorium(bmhkLaboratorium);
	}

	@Override
	@Transactional(propagation = Propagation.SUPPORTS)
	public List<Gemeente> zoekGemeentes(Gemeente zoekObject, long first, long count, String property, boolean ascending)
	{
		return gemeenteDao.zoekGemeentes(zoekObject, first, count, property, ascending);
	}

	@Override
	@Transactional(propagation = Propagation.SUPPORTS)
	public long countGemeentes(Gemeente zoekObject)
	{
		return gemeenteDao.countGemeentes(zoekObject);
	}

	@Override
	@Transactional(propagation = Propagation.SUPPORTS)
	public Boolean getGesplitsOpPostcode(Gemeente gemeente)
	{
		int aantal = 0;
		if (gemeente.getUitnodigingsGebieden() != null)
		{
			aantal = gemeente.getUitnodigingsGebieden().size();
		}
		Boolean gesplitsOpPostcode = null;
		if (aantal > 1)
		{

			for (UitnodigingsGebied gebied : gemeente.getUitnodigingsGebieden())
			{
				if (gebied.getPostcodeGebied() != null)
				{
					gesplitsOpPostcode = Boolean.TRUE;
				}
				else if (StringUtils.isNotBlank(gebied.getWoonplaats()))
				{
					gesplitsOpPostcode = Boolean.FALSE;
				}
				if (gesplitsOpPostcode != null)
				{
					break;
				}
			}
		}
		return gesplitsOpPostcode;
	}

	@Override
	public List<String> getWoonplaatsen(UitnodigingsGebied gebied)
	{
		List<String> woonplaatsen = gemeenteDao.getWoonplaatsen(gebied);
		for (UitnodigingsGebied andereGebied : gebied.getGemeente().getUitnodigingsGebieden())
		{
			if (StringUtils.isNotBlank(andereGebied.getWoonplaats()))
			{
				woonplaatsen.remove(andereGebied.getWoonplaats());
			}
		}
		return woonplaatsen;
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void verwijderAlleGebieden(Gemeente gemeente)
	{
		List<UitnodigingsGebied> toDeleteGebieden = new ArrayList<>();
		for (UitnodigingsGebied gebied : gemeente.getUitnodigingsGebieden())
		{
			if (gebied.getPostcodeGebied() != null || StringUtils.isNotBlank(gebied.getWoonplaats()))
			{
				if (gebied.getVerdeling() != null)
				{
					hibernateService.deleteAll(gebied.getVerdeling());
				}
				toDeleteGebieden.add(gebied);
			}
		}
		for (UitnodigingsGebied toDeleteGebied : toDeleteGebieden)
		{
			gemeente.getUitnodigingsGebieden().remove(toDeleteGebied);
			hibernateService.delete(toDeleteGebied);
		}
		hibernateService.saveOrUpdate(gemeente);
	}

	@Override
	public Iterator<? extends UitnodigingsGebied> getGebieden(UitnodigingsGebied zoekObject, ColoscopieCentrum coloscopieCentrum, long first, long count, String property,
		boolean ascending)
	{
		return gemeenteDao.getGebieden(zoekObject, coloscopieCentrum, first, count, property, ascending);
	}

	@Override
	public long getCountGebieden(UitnodigingsGebied zoekObject, ColoscopieCentrum coloscopieCentrum)
	{
		return gemeenteDao.getCountGebieden(zoekObject, coloscopieCentrum);
	}

	@Override
	public boolean magAlleGebiedenVerwijderen(Gemeente gemeente)
	{
		for (UitnodigingsGebied gebied : gemeente.getUitnodigingsGebieden())
		{
			if (gebied.getPostcodeGebied() != null || StringUtils.isNotBlank(gebied.getWoonplaats()))
			{
				for (ColoscopieCentrumColonCapaciteitVerdeling verdeling : gebied.getVerdeling())
				{
					if (verdeling.getPercentageAdherentie() != null && verdeling.getPercentageAdherentie() > 0)
					{
						return false;
					}
				}
			}
		}
		return true;
	}

	@Override
	public Gemeente getGemeenteByCode(String code)
	{
		return gbaBaseDao.getStamtabelByCode(Gemeente.class, code);
	}
}
