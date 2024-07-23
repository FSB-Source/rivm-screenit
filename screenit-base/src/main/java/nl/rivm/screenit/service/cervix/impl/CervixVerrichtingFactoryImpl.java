package nl.rivm.screenit.service.cervix.impl;

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

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Optional;

import nl.rivm.screenit.model.BMHKLaboratorium;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Gemeente;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.cervix.CervixHuisartsLocatie;
import nl.rivm.screenit.model.cervix.CervixMonster;
import nl.rivm.screenit.model.cervix.CervixUitstrijkje;
import nl.rivm.screenit.model.cervix.enums.CervixTariefType;
import nl.rivm.screenit.model.cervix.facturatie.CervixBoekRegel;
import nl.rivm.screenit.model.cervix.facturatie.CervixVerrichting;
import nl.rivm.screenit.service.HuisartsenportaalSyncService;
import nl.rivm.screenit.service.cervix.Cervix2023StartBepalingService;
import nl.rivm.screenit.service.cervix.CervixBaseVerrichtingService;
import nl.rivm.screenit.service.cervix.CervixVerrichtingFactory;
import nl.rivm.screenit.util.BriefUtil;
import nl.rivm.screenit.util.DateUtil;
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
	private CervixBaseVerrichtingService verrichtingService;

	@Autowired
	private HuisartsenportaalSyncService huisartsenportaalSyncService;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private Cervix2023StartBepalingService cervix2023StartBepalingService;

	@Override
	public List<CervixVerrichting> maakLabVerrichting(CervixMonster monster, CervixTariefType tariefType, Date verrichtingsDatum)
	{
		var laboratorium = getLaboratoriumVoorVerrichting(tariefType, monster, verrichtingsDatum);
		var bmhk2023Lab = cervix2023StartBepalingService.isBmhk2023Laboratorium(laboratorium);
		var verrichtingen = new ArrayList<CervixVerrichting>();

		if (!Boolean.TRUE.equals(bmhk2023Lab))
		{
			verrichtingen.add(maakVerrichting(monster, tariefType, verrichtingsDatum, null, laboratorium));
		}
		else
		{

			switch (tariefType)
			{
			case LAB_HPV_ANALYSE_UITSTRIJKJE:
				verrichtingen.add(maakVerrichting(monster, CervixTariefType.LAB_LOGISTIEK, verrichtingsDatum, null, laboratorium));
				verrichtingen.add(maakVerrichting(monster, CervixTariefType.LAB_HPV_ANALYSE_KLINISCH_EN_ZELF_AFGENOMEN, verrichtingsDatum, null, laboratorium));
				break;

			case LAB_HPV_ANALYSE_ZAS:
				verrichtingen.add(maakVerrichting(monster, CervixTariefType.LAB_MONSTERONTVANGST_EN_MONSTEROPWERKING_ZAS, verrichtingsDatum, null, laboratorium));
				verrichtingen.add(maakVerrichting(monster, CervixTariefType.LAB_HPV_ANALYSE_KLINISCH_EN_ZELF_AFGENOMEN, verrichtingsDatum, null, laboratorium));
				break;

			case LAB_CYTOLOGIE_NA_HPV_UITSTRIJKJE:
				verrichtingen.add(maakCervixcytologieVerrichting(CervixMonsterUtil.getUitstrijkje(monster), verrichtingsDatum, laboratorium));
				break;

			case LAB_CYTOLOGIE_NA_HPV_ZAS:
			case LAB_CYTOLOGIE_VERVOLGUITSTRIJKJE:
				verrichtingen.add(maakVerrichting(monster, CervixTariefType.LAB_LOGISTIEK, verrichtingsDatum, null, laboratorium));
				verrichtingen.add(maakCervixcytologieVerrichting(CervixMonsterUtil.getUitstrijkje(monster), verrichtingsDatum, laboratorium));
				break;

			default:
				throw new IllegalStateException(String.format("Verrichting %s kan niet verwerkt worden door lab", tariefType.name()));
			}
		}

		return verrichtingen;
	}

	@Override
	public void maakHuisartsVerrichting(CervixMonster monster, Date verrichtingsDatum, CervixHuisartsLocatie huisartsLocatie)
	{
		var verrichting = maakVerrichting(monster, CervixTariefType.HUISARTS_UITSTRIJKJE, verrichtingsDatum, huisartsLocatie, null);

		huisartsenportaalSyncService.sendJmsBericht(CervixHuisartsToDtoUtil.getVerrichtingDto(verrichting));
	}

	private CervixVerrichting maakCervixcytologieVerrichting(CervixUitstrijkje uitstrijkje, Date verrichtingsDatum, BMHKLaboratorium laboratorium)
	{
		if (isCos(uitstrijkje))
		{
			return maakVerrichting(uitstrijkje, CervixTariefType.LAB_CERVIXCYTOLOGIE_MET_COS, verrichtingsDatum, null, laboratorium);
		}
		else
		{
			return maakVerrichting(uitstrijkje, CervixTariefType.LAB_CERVIXCYTOLOGIE_MANUEEL_SCREENEN, verrichtingsDatum, null, laboratorium);
		}
	}

	private boolean isCos(CervixUitstrijkje uitstrijkje)
	{
		return Optional
			.ofNullable(uitstrijkje.getCytologieVerslag().getVerslagContent().getCytologieUitslagBvoBmhk().getCos())
			.orElse(false);
	}

	private CervixVerrichting maakVerrichting(CervixMonster monster, CervixTariefType tariefType, Date verrichtingsDatum, CervixHuisartsLocatie huisartsLocatie,
		BMHKLaboratorium laboratorium)
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
				so = BriefUtil.getMergedBrieven(monster.getUitnodiging().getBrief()).getScreeningOrganisatie();
			}
			else
			{
				so = BriefUtil.getMergedBrieven(monster.getUitnodiging().getScreeningRonde().getEersteUitnodiging().getBrief()).getScreeningOrganisatie();
			}

		}
		else
		{
			so = gemeente.getScreeningOrganisatie();
		}

		var verrichting = new CervixVerrichting();
		verrichting.setMonster(monster);
		verrichting.setVerrichtingsDatum(verrichtingsDatum);
		verrichting.setHuisartsLocatie(huisartsLocatie);
		verrichting.setRegio(so);
		verrichting.setClient(client);
		verrichting.setType(tariefType);
		hibernateService.saveOrUpdate(verrichting);
		var boekRegel = new CervixBoekRegel();
		var tarief = verrichtingService.getTariefVoorDatum(verrichtingsDatum, laboratorium);
		boekRegel.setTarief(tarief);
		boekRegel.setDebet(false);
		boekRegel.setVerrichting(verrichting);
		verrichting.getBoekRegels().add(boekRegel);
		verrichting.setLaatsteBoekRegel(boekRegel);
		hibernateService.saveOrUpdateAll(boekRegel, verrichting);
		return verrichting;
	}

	private BMHKLaboratorium getLaboratoriumVoorVerrichting(CervixTariefType tariefType, CervixMonster monster, Date verrichtingsDatum)
	{
		if (tariefType == CervixTariefType.LAB_HPV_ANALYSE_UITSTRIJKJE || tariefType == CervixTariefType.LAB_HPV_ANALYSE_ZAS)
		{
			return monster.getLaboratorium();
		}

		if (cervix2023StartBepalingService.datumValtBinnenBmhk2023(DateUtil.toLocalDate(verrichtingsDatum)))
		{
			return CervixMonsterUtil.getUitstrijkje(monster).getCytologieVerslag().getLaboratorium();
		}

		return monster.getLaboratorium();
	}
}
