package nl.rivm.screenit.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.model.AanvraagBriefStatus;
import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.Afmelding;
import nl.rivm.screenit.model.ScreeningRonde;
import nl.rivm.screenit.model.colon.ColonAfmelding;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.colon.OpenUitnodiging;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.enums.OpenUitnodigingUitslag;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.OpenUitnodigingService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS)
public class OpenUitnodigingServiceImpl implements OpenUitnodigingService
{
	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private LogService logService;

	@Override
	public OpenUitnodiging afmeldingHeraanmeldingReactieOpOpenUitnodiging(Afmelding afmelding, ScreeningRonde ronde, Account account)
	{
		if (ronde != null && Bevolkingsonderzoek.COLON.equals(ronde.getDossier().getBevolkingsonderzoek())
			&& (AanvraagBriefStatus.VERWERKT.equals(afmelding.getAfmeldingStatus()) || AanvraagBriefStatus.VERWERKT.equals(afmelding.getHeraanmeldStatus())))
		{
			ColonScreeningRonde colonRonde = (ColonScreeningRonde) ronde;
			if (colonRonde.getOpenUitnodiging() != null)
			{
				OpenUitnodiging ou = colonRonde.getOpenUitnodiging();
				if (ou.getUitslag() == null || OpenUitnodigingUitslag.INTAKE_AFSPRAAK.equals(ou.getUitslag()))
				{
					ou.setUitslag(OpenUitnodigingUitslag.AFMELDING);
					ou.setAfmelding((ColonAfmelding) afmelding);
					hibernateService.saveOrUpdate(afmelding);
					logService.logGebeurtenis(LogGebeurtenis.OPEN_UITNODIGING_REACTIE, account, colonRonde.getDossier().getClient(), "Client is afgemeld",
						Bevolkingsonderzoek.COLON);
				}
				else if (OpenUitnodigingUitslag.AFMELDING.equals(ou.getUitslag()))
				{
					ou.setUitslag(null);
					ou.setAfmelding(null);
					hibernateService.saveOrUpdate(afmelding);
				}
				return ou;
			}
		}
		return null;
	}

}
