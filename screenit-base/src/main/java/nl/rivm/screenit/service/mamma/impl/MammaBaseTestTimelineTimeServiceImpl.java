package nl.rivm.screenit.service.mamma.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.model.mamma.MammaAfmelding;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaBrief;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaIlmBezwaarPoging;
import nl.rivm.screenit.model.mamma.MammaMergedBrieven;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.MammaUitnodiging;
import nl.rivm.screenit.model.mamma.MammaUitstel;
import nl.rivm.screenit.service.BaseTestTimelineService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.mamma.MammaBaseTestTimelineTimeService;
import nl.rivm.screenit.service.mamma.enums.MammaTestTimeLineDossierTijdstip;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.google.common.primitives.Ints;

@Service
@Transactional(propagation = Propagation.REQUIRED)
@Slf4j
public class MammaBaseTestTimelineTimeServiceImpl implements MammaBaseTestTimelineTimeService
{
	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private ICurrentDateSupplier dateSupplier;

	@Autowired
	private BaseTestTimelineService baseTestTimelineService;

	@Override
	public boolean rekenDossierTerug(MammaDossier dossier, MammaTestTimeLineDossierTijdstip tijdstip)
	{
		int dagen = aantalDagenCalculator(dossier, tijdstip);
		rekenDossierTerug(dossier, dagen);
		return true;
	}

	@Override
	public boolean rekenDossierTerug(MammaDossier dossier, int aantalDagen)
	{
		LOG.debug("Dossier aantal dagen terug gezet: {} ", aantalDagen);
		baseTestTimelineService.rekenObjectTerug(dossier, aantalDagen);
		hibernateService.saveOrUpdate(dossier);

		if (dossier.getVolgendeUitnodiging() != null)
		{
			baseTestTimelineService.rekenObjectTerug(dossier.getVolgendeUitnodiging(), aantalDagen);
			hibernateService.saveOrUpdate(dossier.getVolgendeUitnodiging());
		}

		for (MammaIlmBezwaarPoging bezwaarPoging : dossier.getIlmBezwaarPogingen())
		{
			baseTestTimelineService.rekenObjectTerug(bezwaarPoging, aantalDagen);
		}
		for (MammaScreeningRonde ronde : dossier.getScreeningRondes())
		{
			rekenRondeTerug(ronde, aantalDagen);
		}
		for (MammaAfmelding afmelding : dossier.getAfmeldingen())
		{
			rekenAfmeldingTerug(afmelding, aantalDagen);
		}

		baseTestTimelineService.rekenAllePersoonsDatumTerug(dossier.getClient().getPersoon(), aantalDagen);
		return true;
	}

	private void rekenRondeTerug(MammaScreeningRonde ronde, int aantalDagen)
	{
		baseTestTimelineService.rekenObjectTerug(ronde, aantalDagen);
		hibernateService.saveOrUpdate(ronde);

		ronde.getFollowUpRadiologieVerslagen().forEach(mammaFollowUpRadiologieVerslag -> baseTestTimelineService.rekenObjectTerug(mammaFollowUpRadiologieVerslag, aantalDagen));

		for (MammaUitstel uitstel : ronde.getUitstellen())
		{
			rekenUitstelTerug(uitstel, aantalDagen);
		}

		for (MammaUitnodiging uitnodiging : ronde.getUitnodigingen())
		{
			rekenUitnodigingTerug(uitnodiging, aantalDagen);
		}
		for (MammaBrief brief : ronde.getBrieven())
		{
			rekenBriefTerug(brief, aantalDagen);
		}

		ronde.getFollowUpVerslagen().forEach(verslag -> baseTestTimelineService.rekenObjectTerug(verslag, aantalDagen));

		for (MammaAfmelding afmelding : ronde.getAfmeldingen())
		{
			rekenAfmeldingTerug(afmelding, aantalDagen);
		}
	}

	private void rekenUitstelTerug(MammaUitstel uitstel, int aantalDagen)
	{
		baseTestTimelineService.rekenObjectTerug(uitstel, aantalDagen);
		hibernateService.saveOrUpdate(uitstel);
	}

	private void rekenUitnodigingTerug(MammaUitnodiging uitnodiging, int aantalDagen)
	{
		baseTestTimelineService.rekenObjectTerug(uitnodiging, aantalDagen);
		hibernateService.saveOrUpdate(uitnodiging);

		for (MammaAfspraak afspraak : uitnodiging.getAfspraken())
		{
			baseTestTimelineService.rekenObjectTerug(afspraak, aantalDagen);

			MammaOnderzoek onderzoek = afspraak.getOnderzoek();
			if (onderzoek != null)
			{
				baseTestTimelineService.rekenObjectTerug(onderzoek, aantalDagen);
				baseTestTimelineService.rekenObjectTerug(onderzoek.getMammografie(), aantalDagen);
				for (MammaBeoordeling beoordeling : onderzoek.getBeoordelingen())
				{
					baseTestTimelineService.rekenObjectTerug(beoordeling, aantalDagen);
					baseTestTimelineService.rekenObjectTerug(beoordeling.getDiscrepantieLezing(), aantalDagen);
					baseTestTimelineService.rekenObjectTerug(beoordeling.getArbitrageLezing(), aantalDagen);
					baseTestTimelineService.rekenObjectTerug(beoordeling.getTweedeLezing(), aantalDagen);
					baseTestTimelineService.rekenObjectTerug(beoordeling.getEersteLezing(), aantalDagen);
					hibernateService.saveOrUpdate(onderzoek);
				}
			}

		}

	}

	private void rekenBriefTerug(MammaBrief brief, int aantalDagen)
	{
		baseTestTimelineService.rekenObjectTerug(brief, aantalDagen);
		hibernateService.saveOrUpdate(brief);

		MammaMergedBrieven mergedBrieven = brief.getMergedBrieven();
		if (mergedBrieven != null)
		{
			rekenMergedBrievenTerug(mergedBrieven, aantalDagen);
		}

	}

	private void rekenMergedBrievenTerug(MammaMergedBrieven mergedBrieven, int aantalDagen)
	{
		baseTestTimelineService.rekenObjectTerug(mergedBrieven, aantalDagen);
		hibernateService.saveOrUpdate(mergedBrieven);
	}

	private void rekenAfmeldingTerug(MammaAfmelding afmelding, int aantalDagen)
	{
		baseTestTimelineService.rekenObjectTerug(afmelding, aantalDagen);
		hibernateService.saveOrUpdate(afmelding);
	}

	private int aantalDagenCalculator(MammaDossier dossier, MammaTestTimeLineDossierTijdstip tijdstip)
	{
		switch (tijdstip)
		{
		case VERVOLGONDERZOEK_BRIEF:
			return 180;
		case NIEUWE_RONDE:
			return 730;
		case DATUM_TIJD_AFSPRAAK:
			return 0;
		case ONDERZOEK_ONTVANGEN:
			return Math.abs(Ints.checkedCast(ChronoUnit.DAYS
				.between(DateUtil.toLocalDate(dossier.getLaatsteScreeningRonde().getLaatsteUitnodiging().getLaatsteAfspraak().getVanaf()), dateSupplier.getLocalDate())));
		default:
			return 1;
		}
	}

}
