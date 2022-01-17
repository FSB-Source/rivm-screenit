package nl.rivm.screenit.service.colon.impl;

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

import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.EnovationHuisarts;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.RondeNummerService;
import nl.rivm.screenit.service.colon.ColonHuisartsBerichtService;
import nl.rivm.screenit.service.colon.ColonHuisartsService;
import nl.rivm.screenit.util.EntityAuditUtil;
import nl.rivm.screenit.util.NaamUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS)
public class ColonHuisartsServiceImpl implements ColonHuisartsService
{

	@Autowired
	private ColonHuisartsBerichtService huisartsBerichtService;

	@Autowired
	private LogService logService;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private RondeNummerService rondeNummerService;

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public boolean koppelHuisarts(EnovationHuisarts huisarts, ColonScreeningRonde ronde, Account account)
	{
		if (ronde != null)
		{
			String melding;
			Client client = ronde.getDossier().getClient();
			if (huisarts != null)
			{
				melding = "Huisarts: " + NaamUtil.getNaamHuisarts(huisarts);
			}
			else
			{
				melding = "Huisarts verwijderd";
			}

			ronde.setColonHuisarts(huisarts);
			boolean diffColonHuisarts = StringUtils
				.isNotBlank(EntityAuditUtil.getDiffFieldToLatestVersion(ronde, "colonHuisarts", hibernateService.getHibernateSession()));

			if (diffColonHuisarts)
			{
				ronde.setDatumVastleggenHuisarts(currentDateSupplier.getDate());
				hibernateService.saveOrUpdate(ronde);
				logService.logGebeurtenis(LogGebeurtenis.HUISARTS_GEWIJZIGD, account, client, melding, Bevolkingsonderzoek.COLON);
				return true;
			}
		}
		return false;
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public boolean ontkoppelHuisarts(ColonScreeningRonde ronde, Account account)
	{
		if (ronde != null)
		{
			return koppelHuisarts(null, ronde, account);
		}
		return false;
	}

	@Override
	@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
	public EnovationHuisarts getActieveHuisartsVanVorigeRonde(ColonScreeningRonde ronde)
	{
		if (ronde != null)
		{
			ColonScreeningRonde vorigeRonde = rondeNummerService.getVorigeRonde(ronde);
			if (vorigeRonde != null)
			{
				return getActieveHuisartsVanRonde(vorigeRonde);
			}
		}
		return null;
	}

	@Override
	@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
	public EnovationHuisarts getActieveHuisartsVanRonde(ColonScreeningRonde ronde)
	{
		if (ronde != null)
		{
			EnovationHuisarts huisarts = ronde.getColonHuisarts();
			if (huisarts != null && !huisarts.isVerwijderd())
			{
				return huisarts;
			}
		}
		return null;
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public boolean bevestigVorigeColonHuisarts(Client client, ColonScreeningRonde ronde)
	{
		if (ronde != null)
		{
			EnovationHuisarts vorigeHuisarts = getActieveHuisartsVanVorigeRonde(ronde);
			if (vorigeHuisarts != null)
			{
				return koppelHuisarts(vorigeHuisarts, ronde, client);
			}
		}
		return false;
	}
}
