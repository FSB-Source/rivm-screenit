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

import java.time.temporal.ChronoUnit;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.model.mamma.MammaAfmelding;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaBrief;
import nl.rivm.screenit.model.mamma.MammaCapaciteitBlok;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaMergedBrieven;
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
		var dagen = aantalDagenCalculator(dossier, tijdstip);
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

		for (var bezwaarPoging : dossier.getIlmBezwaarPogingen())
		{
			baseTestTimelineService.rekenObjectTerug(bezwaarPoging, aantalDagen);
		}
		for (var ronde : dossier.getScreeningRondes())
		{
			rekenRondeTerug(ronde, aantalDagen);
		}
		for (var afmelding : dossier.getAfmeldingen())
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

		for (var uitstel : ronde.getUitstellen())
		{
			rekenUitstelTerug(uitstel, aantalDagen);
		}

		for (var uitnodiging : ronde.getUitnodigingen())
		{
			rekenUitnodigingTerug(uitnodiging, aantalDagen);
		}
		for (var brief : ronde.getBrieven())
		{
			rekenBriefTerug(brief, aantalDagen);
		}

		ronde.getFollowUpVerslagen().forEach(verslag -> baseTestTimelineService.rekenObjectTerug(verslag, aantalDagen));

		for (var afmelding : ronde.getAfmeldingen())
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

		for (var afspraak : uitnodiging.getAfspraken())
		{
			baseTestTimelineService.rekenObjectTerug(afspraak, aantalDagen);
			ontkoppelAfspraakBuitenCapaciteitblok(afspraak);

			var onderzoek = afspraak.getOnderzoek();
			if (onderzoek != null)
			{
				baseTestTimelineService.rekenObjectTerug(onderzoek, aantalDagen);
				baseTestTimelineService.rekenObjectTerug(onderzoek.getMammografie(), aantalDagen);
				for (var beoordeling : onderzoek.getBeoordelingen())
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

	private void ontkoppelAfspraakBuitenCapaciteitblok(MammaAfspraak afspraak)
	{
		var capaciteitBlok = afspraak.getCapaciteitBlok();
		if (capaciteitBlok != null && afspraakValtBuitenCapaciteitBlok(afspraak, capaciteitBlok))
		{
			afspraak.setCapaciteitBlok(null);
			capaciteitBlok.getAfspraken().remove(afspraak);
			hibernateService.saveOrUpdateAll(afspraak, capaciteitBlok);
		}
	}

	private boolean afspraakValtBuitenCapaciteitBlok(MammaAfspraak afspraak, MammaCapaciteitBlok capaciteitBlok)
	{
		var afspraakMoment = afspraak.getVanaf();
		return capaciteitBlok.getVanaf().compareTo(afspraakMoment) > 0 || capaciteitBlok.getTot().compareTo(afspraakMoment) <= 0;
	}

	private void rekenBriefTerug(MammaBrief brief, int aantalDagen)
	{
		baseTestTimelineService.rekenObjectTerug(brief, aantalDagen);
		hibernateService.saveOrUpdate(brief);

		var mergedBrieven = brief.getMergedBrieven();
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
