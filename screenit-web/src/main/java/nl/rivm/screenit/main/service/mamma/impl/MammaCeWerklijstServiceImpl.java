package nl.rivm.screenit.main.service.mamma.impl;

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

import java.util.List;

import nl.rivm.screenit.main.dao.mamma.MammaBeoordelingDao;
import nl.rivm.screenit.main.dao.mamma.MammaOnderzoekDao;
import nl.rivm.screenit.main.model.mamma.beoordeling.MammaCeWerklijstZoekObject;
import nl.rivm.screenit.main.service.mamma.MammaCeWerklijstService;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class MammaCeWerklijstServiceImpl implements MammaCeWerklijstService
{
	@Autowired
	private MammaBeoordelingDao beoordelingDao;

	@Autowired
	private MammaOnderzoekDao onderzoekDao;

	@Override
	public long countOnderzoeken(MammaCeWerklijstZoekObject zoekObject)
	{
		return beoordelingDao.countCeWerklijstBeoordelingen(zoekObject);
	}

	@Override
	public List<MammaBeoordeling> zoekOnderzoeken(MammaCeWerklijstZoekObject zoekObject, int first, int count, String sortProperty, boolean ascending)
	{
		return beoordelingDao.zoekCeBeoordelingen(zoekObject, first, count, sortProperty, ascending);
	}

	@Override
	public List<MammaOnderzoek> zoekOnderbrokenOnderzoeken(MammaCeWerklijstZoekObject zoekObject, int first, int count, String sortProperty, boolean ascending)
	{
		return onderzoekDao.zoekOnderbrokenOnderzoeken(zoekObject, first, count, sortProperty, ascending);
	}

	@Override
	public long countOnderbrokenOnderzoeken(MammaCeWerklijstZoekObject zoekObject)
	{
		return onderzoekDao.countOnderbrokenOnderzoeken(zoekObject);
	}

	@Override
	public List<MammaBeoordeling> zoekProcessmonitoringBeoordelingen(MammaCeWerklijstZoekObject zoekObject, int first, int count, String sortProperty, boolean ascending)
	{
		return beoordelingDao.zoekCeWerklijstProcesmonitoringBeoordelingen(zoekObject, first, count, sortProperty, ascending);
	}

	@Override
	public long countProcessmonitoringBeoordelingen(MammaCeWerklijstZoekObject zoekObject)
	{
		return beoordelingDao.countCeWerklijstProcesmonitoringBeoordelingen(zoekObject);
	}

	@Override
	public List<MammaBeoordeling> zoekGeenBeoordelingMogelijk(MammaCeWerklijstZoekObject zoekObject, int first, int count, String sortProperty, boolean ascending)
	{
		return beoordelingDao.zoekGeenBeoordelingMogelijk(zoekObject, first, count, sortProperty, ascending);
	}

	@Override
	public long countGeenBeoordelingMogelijk(MammaCeWerklijstZoekObject zoekObject)
	{
		return beoordelingDao.countGeenBeoordelingMogelijk(zoekObject);
	}

	@Override
	public List<MammaBeoordeling> zoekFollowUpBeoordelingen(MammaCeWerklijstZoekObject zoekObject, int first, int count, String sortProperty, boolean ascending)
	{
		return beoordelingDao.zoekFollowUpNietGedownloadBeoordelingen(zoekObject, first, count, sortProperty, ascending);
	}

	@Override
	public long countFollowUpBeoordelingen(MammaCeWerklijstZoekObject zoekObject)
	{
		return beoordelingDao.countFollowUpNietGedownloadBeoordelingen(zoekObject);
	}
}
