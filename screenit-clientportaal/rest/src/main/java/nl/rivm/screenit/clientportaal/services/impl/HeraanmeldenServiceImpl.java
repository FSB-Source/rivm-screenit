package nl.rivm.screenit.clientportaal.services.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal
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

import nl.rivm.screenit.clientportaal.services.HeraanmeldenService;
import nl.rivm.screenit.model.Afmelding;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.service.BaseAfmeldService;
import nl.rivm.screenit.util.AfmeldingUtil;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class HeraanmeldenServiceImpl implements HeraanmeldenService
{

	@Autowired
	private BaseAfmeldService baseAfmeldService;

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void saveHeraanmeldenVerzoek(Client client, Bevolkingsonderzoek bevolkingsonderzoek, boolean clientWilNieuweUitnodiging)
	{
		Afmelding<?, ?, ?> afmelding;

		switch (bevolkingsonderzoek)
		{
		case COLON:
			afmelding = AfmeldingUtil.getVerwerkteAfmelding(client.getColonDossier());
			afmelding.setClientWilNieuweUitnodiging(clientWilNieuweUitnodiging);
			break;
		case MAMMA:
			afmelding = AfmeldingUtil.getVerwerkteAfmelding(client.getMammaDossier());
			break;
		case CERVIX:
			afmelding = AfmeldingUtil.getVerwerkteAfmelding(client.getCervixDossier());
			break;
		default:
			throw new IllegalStateException("Unexpected value: " + bevolkingsonderzoek);

		}

		baseAfmeldService.heraanmelden(afmelding, client);
	}
}
