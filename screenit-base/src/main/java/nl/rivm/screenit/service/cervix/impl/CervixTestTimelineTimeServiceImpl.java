package nl.rivm.screenit.service.cervix.impl;

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

import java.time.temporal.ChronoUnit;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.model.berichten.cda.OntvangenCdaBericht;
import nl.rivm.screenit.model.cervix.CervixAfmelding;
import nl.rivm.screenit.model.cervix.CervixBrief;
import nl.rivm.screenit.model.cervix.CervixCytologieOrder;
import nl.rivm.screenit.model.cervix.CervixCytologieVerslag;
import nl.rivm.screenit.model.cervix.CervixDossier;
import nl.rivm.screenit.model.cervix.CervixHpvBeoordeling;
import nl.rivm.screenit.model.cervix.CervixHpvBericht;
import nl.rivm.screenit.model.cervix.CervixHuisartsBericht;
import nl.rivm.screenit.model.cervix.CervixLabformulier;
import nl.rivm.screenit.model.cervix.CervixMergedBrieven;
import nl.rivm.screenit.model.cervix.CervixMonster;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde;
import nl.rivm.screenit.model.cervix.CervixUitnodiging;
import nl.rivm.screenit.model.cervix.CervixUitstel;
import nl.rivm.screenit.model.cervix.CervixUitstrijkje;
import nl.rivm.screenit.model.cervix.CervixZas;
import nl.rivm.screenit.model.cervix.verslag.CervixVerslag;
import nl.rivm.screenit.service.BaseTestTimelineService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.cervix.CervixTestTimelineTimeService;
import nl.rivm.screenit.service.cervix.enums.CervixTestTimeLineDossierTijdstip;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.REQUIRED)
@Slf4j
public class CervixTestTimelineTimeServiceImpl implements CervixTestTimelineTimeService
{
	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private BaseTestTimelineService baseTestTimelineService;

	@Autowired
	private SimplePreferenceService preferenceService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Override
	public boolean rekenDossierTerug(CervixDossier dossier, CervixTestTimeLineDossierTijdstip tijdstip)
	{
		int dagen = aantalDagenCalculator(tijdstip);
		rekenDossierTerug(dossier, dagen);
		return true;
	}

	@Override
	public boolean rekenDossierTerug(CervixDossier dossier, int aantalDagen)
	{
		LOG.debug("Dossier aantal dagen terug gezet: " + aantalDagen);
		baseTestTimelineService.rekenObjectTerug(dossier, aantalDagen);
		hibernateService.saveOrUpdate(dossier);

		for (CervixScreeningRonde ronde : dossier.getScreeningRondes())
		{
			rekenRondeTerug(ronde, aantalDagen);
		}
		for (CervixAfmelding afmelding : dossier.getAfmeldingen())
		{
			rekenAfmeldingTerug(afmelding, aantalDagen);
		}

		baseTestTimelineService.rekenAllePersoonsDatumTerug(dossier.getClient().getPersoon(), aantalDagen);
		return true;
	}

	private void rekenRondeTerug(CervixScreeningRonde ronde, int aantalDagen)
	{
		baseTestTimelineService.rekenObjectTerug(ronde, aantalDagen);
		hibernateService.saveOrUpdate(ronde);

		for (CervixUitnodiging uitnodiging : ronde.getUitnodigingen())
		{
			rekenUitnodigingTerug(uitnodiging, aantalDagen);
		}
		for (CervixBrief brief : ronde.getBrieven())
		{
			rekenBriefTerug(brief, aantalDagen);
		}
		for (CervixVerslag verslag : ronde.getVerslagen())
		{
			rekenVerslagTerug(verslag, aantalDagen);
		}
		for (CervixHuisartsBericht huisartsBericht : ronde.getHuisartsBerichten())
		{
			rekenHuisartsberichtTerug(huisartsBericht, aantalDagen);
		}
		for (CervixAfmelding afmelding : ronde.getAfmeldingen())
		{
			rekenAfmeldingTerug(afmelding, aantalDagen);
		}

		CervixUitstel uitstel = ronde.getUitstel();
		if (uitstel != null)
		{
			rekenUitstelTerug(uitstel, aantalDagen);
		}
	}

	private void rekenUitnodigingTerug(CervixUitnodiging uitnodiging, int aantalDagen)
	{
		baseTestTimelineService.rekenObjectTerug(uitnodiging, aantalDagen);
		hibernateService.saveOrUpdate(uitnodiging);

		rekenMonsterTerug(uitnodiging.getMonster(), aantalDagen);
	}

	private void rekenBriefTerug(CervixBrief brief, int aantalDagen)
	{
		baseTestTimelineService.rekenObjectTerug(brief, aantalDagen);
		hibernateService.saveOrUpdate(brief);

		CervixMergedBrieven mergedBrieven = brief.getMergedBrieven();
		if (mergedBrieven != null)
		{
			rekenMergedBrievenTerug(mergedBrieven, aantalDagen);
		}

	}

	private void rekenMergedBrievenTerug(CervixMergedBrieven mergedBrieven, int aantalDagen)
	{
		baseTestTimelineService.rekenObjectTerug(mergedBrieven, aantalDagen);
		hibernateService.saveOrUpdate(mergedBrieven);
	}

	private void rekenHuisartsberichtTerug(CervixHuisartsBericht huisartsBericht, int aantalDagen)
	{
		baseTestTimelineService.rekenObjectTerug(huisartsBericht, aantalDagen);
		hibernateService.saveOrUpdate(huisartsBericht);
	}

	private void rekenMonsterTerug(CervixMonster monster, int aantalDagen)
	{
		if (monster != null)
		{
			for (CervixHpvBeoordeling hpvBeoordeling : monster.getHpvBeoordelingen())
			{
				rekenHpvBeoordelingTerug(hpvBeoordeling, aantalDagen);
			}

			switch (monster.getUitnodiging().getMonsterType())
			{
			case UITSTRIJKJE:
				rekenUitstrijkjeTerug((CervixUitstrijkje) monster, aantalDagen);
				break;
			case ZAS:
				rekenZasTerug((CervixZas) monster, aantalDagen);
				break;
			}

		}
	}

	private void rekenHpvBeoordelingTerug(CervixHpvBeoordeling hpvBeoordeling, int aantalDagen)
	{
		baseTestTimelineService.rekenObjectTerug(hpvBeoordeling, aantalDagen);
		hibernateService.saveOrUpdate(hpvBeoordeling);
		rekenHpvBerichtTerug(hpvBeoordeling.getHpvBericht(), aantalDagen);
	}

	private void rekenHpvBerichtTerug(CervixHpvBericht hpvBericht, int aantalDagen)
	{
		baseTestTimelineService.rekenObjectTerug(hpvBericht, aantalDagen);
		hibernateService.saveOrUpdate(hpvBericht);

	}

	private void rekenZasTerug(CervixZas zas, int aantalDagen)
	{
		baseTestTimelineService.rekenObjectTerug(zas, aantalDagen);
		hibernateService.saveOrUpdate(zas);
	}

	private void rekenUitstrijkjeTerug(CervixUitstrijkje uitstrijkje, int aantalDagen)
	{
		baseTestTimelineService.rekenObjectTerug(uitstrijkje, aantalDagen);
		hibernateService.saveOrUpdate(uitstrijkje);

		CervixLabformulier labformulier = uitstrijkje.getLabformulier();
		if (labformulier != null)
		{
			rekenLabformulierTerug(labformulier, aantalDagen);
		}
		CervixCytologieOrder cytologieOrder = uitstrijkje.getCytologieOrder();
		if (cytologieOrder != null)
		{
			rekenCytologieOrderTerug(cytologieOrder, aantalDagen);
		}
	}

	private void rekenLabformulierTerug(CervixLabformulier labformulier, int aantalDagen)
	{
		baseTestTimelineService.rekenObjectTerug(labformulier, aantalDagen);
		hibernateService.saveOrUpdate(labformulier);
	}

	private void rekenCytologieOrderTerug(CervixCytologieOrder cytologieOrder, int aantalDagen)
	{
		baseTestTimelineService.rekenObjectTerug(cytologieOrder, aantalDagen);
		hibernateService.saveOrUpdate(cytologieOrder);
	}

	private void rekenVerslagTerug(CervixVerslag verslag, int aantalDagen)
	{
		rekenCytologieVerslagTerug((CervixCytologieVerslag) verslag, aantalDagen);
		rekenCdaBerichtTerug(verslag.getOntvangenCdaBericht(), aantalDagen);
	}

	private void rekenCdaBerichtTerug(OntvangenCdaBericht cdaBericht, int aantalDagen)
	{
		baseTestTimelineService.rekenObjectTerug(cdaBericht, aantalDagen);
		hibernateService.saveOrUpdate(cdaBericht);
	}

	private void rekenCytologieVerslagTerug(CervixCytologieVerslag cytologieVerslag, int aantalDagen)
	{
		baseTestTimelineService.rekenObjectTerug(cytologieVerslag, aantalDagen);
		hibernateService.saveOrUpdate(cytologieVerslag);
	}

	private void rekenAfmeldingTerug(CervixAfmelding afmelding, int aantalDagen)
	{
		baseTestTimelineService.rekenObjectTerug(afmelding, aantalDagen);
		hibernateService.saveOrUpdate(afmelding);
	}

	private void rekenUitstelTerug(CervixUitstel uitstel, int aantalDagen)
	{
		baseTestTimelineService.rekenObjectTerug(uitstel, aantalDagen);
		hibernateService.saveOrUpdate(uitstel);
	}

	private int aantalDagenCalculator(CervixTestTimeLineDossierTijdstip tijdstip)
	{
		switch (tijdstip)
		{
		case ONTVANGEN:
			return 14;
		case VERVOLGONDERZOEK_BRIEF:
			return Math.toIntExact(ChronoUnit.DAYS.between(currentDateSupplier.getLocalDate(),
				currentDateSupplier.getLocalDate().plusMonths(preferenceService.getInteger(PreferenceKey.CERVIX_INTERVAL_CONTROLE_UITSTRIJKJE.name()))));
		case NIEUWE_RONDE:
			return 1825;
		default:
			return 1;
		}
	}

}
