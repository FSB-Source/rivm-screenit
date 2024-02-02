package nl.rivm.screenit.service.mamma.impl;

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

import nl.rivm.screenit.dao.mamma.MammaBaseScreeningrondeDao;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.enums.BezwaarType;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.mamma.MammaBasePaVerslagService;
import nl.rivm.screenit.util.BezwaarUtil;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class MammaBasePaVerslagServiceImpl implements MammaBasePaVerslagService
{

	@Autowired
	private MammaBaseScreeningrondeDao screeningrondeDao;

	@Autowired
	private ClientService clientService;

	@Override
	public boolean verwachtGegevensVoor(String bsn)
	{
		Client client = clientService.getClientZonderBezwaar(bsn);
		if (client != null)
		{
			client = screeningrondeDao.getLaatsteScreeningRondeMetUitslag(client, null) != null ? client : null;
		}
		return client != null && !BezwaarUtil.isBezwaarActiefVoor(client, BezwaarType.GEEN_DIGITALE_UITWISSELING_MET_HET_ZIEKENHUIS);
	}
}
