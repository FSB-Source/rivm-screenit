package nl.rivm.screenit.service.mamma.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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
import nl.rivm.screenit.model.ScreeningRondeStatus;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.enums.MammaGeenHuisartsOption;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.RondeNummerService;
import nl.rivm.screenit.service.mamma.MammaHuisartsService;
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
	public boolean ontkoppelHuisarts(MammaScreeningRonde ronde, Account account)
	{
		if (ronde != null)
		{
			ronde.setGeenHuisartsOptie(null);
			return koppelHuisarts(null, ronde, account);
		}
		return false;
	}

	@Override
	public boolean koppelHuisarts(EnovationHuisarts huisarts, MammaScreeningRonde ronde, Account account)
	{
		if (ronde != null)
		{
			String melding;
			Client client = ronde.getDossier().getClient();
			if (huisarts != null)
			{
				ronde.setGeenHuisartsOptie(null);
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

			var isHuisartsGewijzigd = StringUtils.isNotBlank(
				EntityAuditUtil.getDiffFieldsToLatestVersion(ronde, hibernateService.getHibernateSession(), "geenHuisartsOptie", "huisarts"));

			if (isHuisartsGewijzigd)
			{
				ronde.setDatumVastleggenHuisarts(currentDateSupplier.getDate());
				hibernateService.saveOrUpdate(ronde);
				logService.logGebeurtenis(LogGebeurtenis.HUISARTS_GEWIJZIGD, account, client, melding, Bevolkingsonderzoek.MAMMA);
				return true;
			}
		}
		return false;
	}

	@Override
	@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
	public EnovationHuisarts getActieveHuisartsVanVorigeRonde(MammaScreeningRonde ronde)
	{
		if (ronde != null)
		{
			MammaScreeningRonde vorigeRonde = rondeNummerService.getVorigeRonde(ronde);
			if (vorigeRonde != null)
			{
				return getActieveHuisartsVanRonde(vorigeRonde);
			}
		}
		return null;
	}

	@Override
	@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
	public EnovationHuisarts getActieveHuisartsVanRonde(MammaScreeningRonde ronde)
	{
		if (ronde != null)
		{
			EnovationHuisarts huisarts = ronde.getHuisarts();
			if (huisarts != null && !huisarts.isVerwijderd())
			{
				return huisarts;
			}
		}
		return null;
	}

	@Override
	@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
	public boolean magHuisartsVerwijderen(MammaScreeningRonde laatsteRonde)
	{
		if (laatsteRonde != null)
		{
			return MammaScreeningRondeUtil.getOnderzoekVanLaatsteAfspraak(laatsteRonde) == null || laatsteRonde.getStatus() == ScreeningRondeStatus.AFGEROND;
		}
		return false;
	}

	@Override
	@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
	public MammaGeenHuisartsOption getMammaGeenHuisartsOptieVorigeRonde(MammaScreeningRonde ronde)
	{
		if (ronde != null)
		{
			MammaScreeningRonde vorigeRonde = rondeNummerService.getVorigeRonde(ronde);
			if (vorigeRonde != null)
			{
				return getMammaGeenHuisartsOptieVanRonde(vorigeRonde);
			}
		}
		return null;
	}

	@Override
	@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
	public MammaGeenHuisartsOption getMammaGeenHuisartsOptieVanRonde(MammaScreeningRonde ronde)
	{
		if (ronde != null)
		{
			return ronde.getGeenHuisartsOptie();
		}
		return null;
	}

	private boolean setMammaGeenHuisartsOption(MammaScreeningRonde ronde, MammaGeenHuisartsOption optie, Client client)
	{
		if (ronde != null)
		{
			ronde.setGeenHuisartsOptie(optie);
			return koppelHuisarts(null, ronde, client);
		}
		return false;
	}

	@Override
	public boolean bevestigVorigeMammaHuisartsKeuze(Client client, MammaScreeningRonde ronde)
	{
		if (ronde != null)
		{
			EnovationHuisarts vorigeHuisarts = getActieveHuisartsVanVorigeRonde(ronde);
			MammaGeenHuisartsOption vorigeGeenHuisartsOptie = getMammaGeenHuisartsOptieVorigeRonde(ronde);
			if (vorigeGeenHuisartsOptie != null && vorigeHuisarts == null)
			{
				return setMammaGeenHuisartsOption(ronde, vorigeGeenHuisartsOptie, client);
			}
			else
			{
				return koppelHuisarts(vorigeHuisarts, ronde, client);
			}
		}
		return false;
	}
}
