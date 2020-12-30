package nl.rivm.screenit.main.service.mamma.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import nl.rivm.screenit.main.service.RondeNummerService;
import nl.rivm.screenit.main.service.mamma.MammaHuisartsService;
import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.EnovationHuisarts;
import nl.rivm.screenit.model.ScreeningRondeStatus;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.util.EntityAuditUtil;
import nl.rivm.screenit.util.NaamUtil;
import nl.rivm.screenit.util.mamma.MammaScreeningRondeUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.REQUIRED)
public class MammaHuisartsServiceImpl implements MammaHuisartsService
{

	@Autowired
	private LogService logService;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private RondeNummerService rondeNummerService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Override
	public boolean koppelHuisarts(EnovationHuisarts huisarts, MammaScreeningRonde ronde, Account account)
	{
		String melding;

		Client client = ronde.getDossier().getClient();
		if (huisarts != null)
		{
			melding = "Huisarts: " + NaamUtil.getNaamHuisarts(huisarts);
		}
		else if (ronde.getGeenHuisartsOptie() != null)
		{
			melding = "Geen huisarts optie: " + ronde.getGeenHuisartsOptie();
		}
		else
		{
			melding = "Huisarts verwijderd";
		}

		ronde.setHuisarts(huisarts);

		boolean diffHuisarts = StringUtils.isNotBlank(EntityAuditUtil.getDiffFieldToLatestVersion(ronde, "huisarts", hibernateService.getHibernateSession()));
		boolean diffGeenHuisartsOptie = StringUtils.isNotBlank(EntityAuditUtil.getDiffFieldToLatestVersion(ronde, "geenHuisartsOptie", hibernateService.getHibernateSession()));

		if (diffHuisarts || diffGeenHuisartsOptie)
		{
			ronde.setDatumVastleggenHuisarts(currentDateSupplier.getDate());
			hibernateService.saveOrUpdate(ronde);
			logService.logGebeurtenis(LogGebeurtenis.HUISARTS_GEWIJZIGD, account, client, melding, Bevolkingsonderzoek.MAMMA);
			return true;
		}
		return false;
	}

	@Override
	@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
	public EnovationHuisarts getHuisartsVanVorigeRonde(MammaScreeningRonde ronde)
	{
		MammaScreeningRonde vorigeRonde = rondeNummerService.getVorigeRonde(ronde);
		EnovationHuisarts huisarts = vorigeRonde != null ? vorigeRonde.getHuisarts() : null;
		return huisarts != null && !huisarts.isVerwijderd() ? huisarts : null;
	}

	@Override
	@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
	public boolean magHuisartsVerwijderen(MammaScreeningRonde laatsteRonde)
	{
		return MammaScreeningRondeUtil.getOnderzoekVanLaatsteAfspraak(laatsteRonde) == null || (laatsteRonde != null && laatsteRonde.getStatus() == ScreeningRondeStatus.AFGEROND);
	}
}
