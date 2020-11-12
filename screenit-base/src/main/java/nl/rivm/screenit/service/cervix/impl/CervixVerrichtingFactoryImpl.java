package nl.rivm.screenit.service.cervix.impl;

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

import java.util.Date;

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Gemeente;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.cervix.CervixHuisartsLocatie;
import nl.rivm.screenit.model.cervix.CervixMonster;
import nl.rivm.screenit.model.cervix.enums.CervixTariefType;
import nl.rivm.screenit.model.cervix.facturatie.CervixBoekRegel;
import nl.rivm.screenit.model.cervix.facturatie.CervixTarief;
import nl.rivm.screenit.model.cervix.facturatie.CervixVerrichting;
import nl.rivm.screenit.service.HuisartsenportaalSyncService;
import nl.rivm.screenit.service.cervix.CervixVerrichtingFactory;
import nl.rivm.screenit.service.cervix.CervixVerrichtingService;
import nl.rivm.screenit.util.cervix.CervixHuisartsToDtoUtil;
import nl.rivm.screenit.util.cervix.CervixMonsterUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.config.ConfigurableBeanFactory;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Scope(value = ConfigurableBeanFactory.SCOPE_PROTOTYPE)
@Transactional(propagation = Propagation.REQUIRED)
public class CervixVerrichtingFactoryImpl implements CervixVerrichtingFactory
{
	@Autowired
	private CervixVerrichtingService verrichtingService;

	@Autowired
	private HuisartsenportaalSyncService huisartsenportaalSyncService;

	@Autowired
	private HibernateService hibernateService;

	@Override
	public void maakLabVerrichting(CervixMonster monster, CervixTariefType tariefType, Date verrichtingsDatum)
	{
		maakVerrichting(monster, tariefType, verrichtingsDatum, null);
	}

	private CervixVerrichting maakVerrichting(CervixMonster monster, CervixTariefType tariefType, Date verrichtingsDatum, CervixHuisartsLocatie huisartsLocatie)
	{
		Client client = monster.getOntvangstScreeningRonde().getDossier().getClient();

		ScreeningOrganisatie so = null;
		Gemeente gemeente = client.getPersoon().getGbaAdres().getGbaGemeente();
		if (gemeente == null)
		{
			throw new IllegalStateException("De gemeente is onbekend voor cliënt met id " + client.getId());
		}
		else if (gemeente.getCode().equals(Gemeente.RNI_CODE))
		{
			if (CervixMonsterUtil.isUitstrijkje(monster))
			{
				so = monster.getUitnodiging().getBrief().getMergedBrieven().getScreeningOrganisatie();
			}
			else
			{
				so = monster.getUitnodiging().getScreeningRonde().getEersteUitnodiging().getBrief().getMergedBrieven().getScreeningOrganisatie();
			}

		}
		else
		{
			so = gemeente.getScreeningOrganisatie();
		}

		CervixVerrichting verrichting = new CervixVerrichting();
		verrichting.setMonster(monster);
		verrichting.setVerrichtingsDatum(verrichtingsDatum);
		verrichting.setHuisartsLocatie(huisartsLocatie);
		verrichting.setRegio(so);
		verrichting.setClient(client);
		verrichting.setType(tariefType);
		hibernateService.saveOrUpdate(verrichting);
		CervixBoekRegel boekRegel = new CervixBoekRegel();
		CervixTarief tarief = verrichtingService.getTariefVoorDatum(tariefType, verrichtingsDatum, monster.getLaboratorium());
		boekRegel.setTarief(tarief);
		boekRegel.setDebet(false);
		boekRegel.setVerrichting(verrichting);
		verrichting.getBoekRegels().add(boekRegel);
		verrichting.setLaatsteBoekRegel(boekRegel);
		hibernateService.saveOrUpdateAll(boekRegel, verrichting);
		return verrichting;
	}

	@Override
	public void maakHuisartsVerrichting(CervixMonster monster, CervixTariefType tariefType, Date verrichtingsDatum, CervixHuisartsLocatie huisartsLocatie)
	{
		CervixVerrichting verrichting = maakVerrichting(monster, tariefType, verrichtingsDatum, huisartsLocatie);
		huisartsenportaalSyncService.sendJmsBericht(CervixHuisartsToDtoUtil.getVerrichtingDto(verrichting));
	}
}
