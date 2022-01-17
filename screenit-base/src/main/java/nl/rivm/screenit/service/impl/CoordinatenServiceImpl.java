package nl.rivm.screenit.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import nl.rivm.screenit.dao.CoordinatenDao;
import nl.rivm.screenit.model.BagAdres;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.TijdelijkAdres;
import nl.rivm.screenit.service.CoordinatenService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.AdresUtil;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class CoordinatenServiceImpl implements CoordinatenService
{
	@Autowired
	private ICurrentDateSupplier dateSupplier;

	@Autowired
	private CoordinatenDao coordinatenDao;

	@Override
	public PersoonCoordinaten getCoordinatenVanPersoon(GbaPersoon persoon)
	{
		BagAdres gbaAdres = persoon.getGbaAdres();
		TijdelijkAdres tijdelijkAdres = persoon.getTijdelijkAdres();
		PersoonCoordinaten coordinatenResults = initEnFillAdres(gbaAdres);
		if (AdresUtil.isTijdelijkAdres(persoon, dateSupplier.getDateTime()))
		{
			coordinatenResults.vanAdres = coordinatenDao.getCoordinaten(tijdelijkAdres);
		}
		return coordinatenResults;
	}

	private PersoonCoordinaten initEnFillAdres(BagAdres gbaAdres)
	{
		PersoonCoordinaten coordinatenResults = new PersoonCoordinaten();
		if (gbaAdres != null)
		{
			coordinatenResults.vanAdres = gbaAdres.getPostcodeCoordinaten();
			if (coordinatenResults.vanAdres == null && gbaAdres.getGbaGemeente() != null && gbaAdres.getGbaGemeente().getLatitude() != null
				&& gbaAdres.getGbaGemeente().getLongitude() != null)
			{
				coordinatenResults.vanAdres = gbaAdres.getGbaGemeente();
			}
		}
		return coordinatenResults;
	}

	@Override
	public PersoonCoordinaten getAdresEnTijdelijkAdresCoordinatenVanPersoon(GbaPersoon persoon)
	{
		BagAdres gbaAdres = persoon.getGbaAdres();
		TijdelijkAdres tijdelijkAdres = persoon.getTijdelijkAdres();
		PersoonCoordinaten coordinatenResults = initEnFillAdres(gbaAdres);
		if (tijdelijkAdres != null)
		{

			coordinatenResults.vanTijdelijkAdres = coordinatenDao.getCoordinaten(tijdelijkAdres);
		}
		return coordinatenResults;
	}

}
