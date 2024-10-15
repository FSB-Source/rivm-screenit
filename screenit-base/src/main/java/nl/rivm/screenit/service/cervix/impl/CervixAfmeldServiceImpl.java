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

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.AfmeldingType;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.cervix.CervixAfmelding;
import nl.rivm.screenit.model.cervix.CervixBrief;
import nl.rivm.screenit.model.cervix.CervixDossier;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde;
import nl.rivm.screenit.model.cervix.CervixUitnodiging;
import nl.rivm.screenit.model.cervix.cis.CervixCISHistorie;
import nl.rivm.screenit.model.cervix.enums.CervixHpvBeoordelingWaarde;
import nl.rivm.screenit.model.cervix.enums.CervixLeeftijdcategorie;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.cervix.Cervix2023StartBepalingService;
import nl.rivm.screenit.service.cervix.CervixAfmeldService;
import nl.rivm.screenit.service.cervix.CervixBaseScreeningrondeService;
import nl.rivm.screenit.service.cervix.CervixFactory;
import nl.rivm.screenit.util.BriefUtil;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Component
@Transactional(propagation = Propagation.REQUIRED)
public class CervixAfmeldServiceImpl implements CervixAfmeldService
{
	@Autowired
	private ClientService clientService;

	@Autowired
	private SimplePreferenceService simplePreferenceService;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private BaseBriefService briefService;

	@Autowired
	private CervixFactory cervixFactory;

	@Autowired
	private CervixBaseScreeningrondeService screeningrondeService;

	@Autowired
	private Cervix2023StartBepalingService bmhk2023StartBepalingService;

	@Override
	@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
	public CervixAfmelding maakAfmelding()
	{
		return new CervixAfmelding();
	}

	@Override
	public void definitieveAfmeldingAanvragen(CervixAfmelding afmelding, boolean rappelBrief)
	{
		BriefType briefType = BriefType.CERVIX_AFMELDING_AANVRAAG;
		if (rappelBrief)
		{
			briefType = BriefType.CERVIX_AFMELDING_HANDTEKENING;
		}
		afmelding.setAfmeldingAanvraag(briefService.maakBvoBrief(afmelding, briefType, currentDateSupplier.getDate()));
		hibernateService.saveOrUpdate(afmelding);
	}

	@Override
	public void eenmaligAfmelden(CervixAfmelding afmelding, Account account)
	{
		CervixScreeningRonde ronde = afmelding.getScreeningRonde();
		screeningrondeService.annuleerHerinnering(ronde);
		screeningrondeService.annuleerNietVerstuurdeZAS(ronde);
		screeningrondeService.annuleerUitstel(ronde);

		for (CervixUitnodiging uitnodiging : ronde.getUitnodigingen())
		{
			CervixBrief brief = uitnodiging.getBrief();
			if (BriefUtil.isNietGegenereerdEnNietVervangen(brief) || !uitnodiging.isVerstuurd())
			{
				hibernateService.saveOrUpdate(BriefUtil.setTegenhouden(brief, true));
			}
		}
	}

	@Override
	public void vervolgAfmelden(CervixAfmelding afmelding)
	{
		if (afmelding.getType() == AfmeldingType.DEFINITIEF)
		{
			afmelding.setAfmeldingBevestiging(
				briefService.maakBvoBrief(afmelding, BriefType.CERVIX_BEVESTIGING_DEFINITIEVE_AFMELDING, currentDateSupplier.getDate()));
			hibernateService.saveOrUpdate(afmelding);
		}
	}

	@Override
	public void vervolgHeraanmelden(CervixAfmelding herAanTeMeldenAfmelding, Account account)
	{
		LocalDateTime creatieDatum = currentDateSupplier.getLocalDateTime().plus(200, ChronoUnit.MILLIS);

		if (herAanTeMeldenAfmelding.getType() == AfmeldingType.DEFINITIEF)
		{
			CervixBrief brief = briefService.maakBvoBrief(herAanTeMeldenAfmelding, BriefType.CERVIX_HERAANMELDING_BEVESTIGING, DateUtil.toUtilDate(creatieDatum));
			herAanTeMeldenAfmelding.setHeraanmeldBevestiging(brief);
			hibernateService.saveOrUpdate(herAanTeMeldenAfmelding);
		}

		CervixScreeningRonde ronde = getGeldigeRondeVoorHeraanmelding(herAanTeMeldenAfmelding);
		if (ronde != null)
		{

			if (ronde.getMonsterHpvUitslag() == null
				|| ronde.getMonsterHpvUitslag().getLaatsteHpvBeoordeling().getHpvUitslag() == CervixHpvBeoordelingWaarde.POSITIEF
				&& ronde.getUitstrijkjeCytologieUitslag() == null
				|| ronde.getUitnodigingVervolgonderzoek() != null && ronde.getUitstrijkjeVervolgonderzoekUitslag() == null)
			{
				if (screeningrondeService.heeftUitnodigingMetMonsterInLabproces(ronde))
				{
					return;
				}
				var laatsteUitnodiging = clientService.getLaatstVerstuurdeUitnodiging(ronde, true);
				LocalDate geboorteDatum = DateUtil.toLocalDate(ronde.getDossier().getClient().getPersoon().getGeboortedatum());
				if (DateUtil.getLeeftijd(geboorteDatum, creatieDatum.toLocalDate()) < CervixLeeftijdcategorie.minimumLeeftijd())
				{
					return;
				}

				CervixBrief brief;
				boolean herinneren = true;
				if (laatsteUitnodiging != null && !screeningrondeService.nieuweUitnodigingVoorClientMoetPUZijn(ronde))
				{
					CervixBrief laatsteUitnodigingBrief = laatsteUitnodiging.getBrief();
					brief = briefService.maakBvoBrief(ronde, laatsteUitnodigingBrief.getBriefType(), DateUtil.toUtilDate(creatieDatum));
					brief.setHerdruk(laatsteUitnodigingBrief);
					hibernateService.saveOrUpdate(brief);

					herinneren = laatsteUitnodiging.getHerinneren();
				}
				else
				{
					CervixLeeftijdcategorie leeftijdcategorie = CervixLeeftijdcategorie.getLeeftijdcategorie(geboorteDatum, creatieDatum);
					brief = briefService.maakBvoBrief(ronde, leeftijdcategorie.getUitnodigingsBrief(), DateUtil.toUtilDate(creatieDatum));
				}

				cervixFactory.maakUitnodigingMetVoorEnNaBmhk2023HerinnerenCheck(ronde, brief, herinneren);
			}
		}
	}

	@Override
	public CervixScreeningRonde getGeldigeRondeVoorHeraanmelding(CervixAfmelding herAanTeMeldenAfmelding)
	{
		switch (herAanTeMeldenAfmelding.getType())
		{
		case EENMALIG:
			return herAanTeMeldenAfmelding.getScreeningRonde();
		case DEFINITIEF:
			CervixDossier dossier = herAanTeMeldenAfmelding.getDossier();
			Client client = dossier.getClient();
			CervixScreeningRonde ronde = dossier.getLaatsteScreeningRonde();
			if (ronde != null && (dossier.getVolgendeRondeVanaf() == null || DateUtil.compareBefore(herAanTeMeldenAfmelding.getHeraanmeldDatum(), dossier.getVolgendeRondeVanaf())))
			{
				return ronde;
			}
			else
			{
				CervixCISHistorie cisHistorie = dossier.getCisHistorie();
				if (cisHistorie != null && herAanTeMeldenAfmelding.equals(cisHistorie.getAfmelding()) && !cisHistorie.isHeeftUitslagInRonde0())
				{
					LocalDate vandaag = currentDateSupplier.getLocalDate();
					LocalDate minimaleGeboortedatum = vandaag.minusYears(CervixLeeftijdcategorie._65.getLeeftijd());
					LocalDate maximaleGeboortedatum = vandaag.minusYears(CervixLeeftijdcategorie.minimumLeeftijd());
					LocalDate geboortedatum = DateUtil.toLocalDate(client.getPersoon().getGeboortedatum());
					if (geboortedatum.isAfter(minimaleGeboortedatum) && geboortedatum.isBefore(maximaleGeboortedatum)
						&& isHuidigeDatumBinnenRonde0(client))
					{
						cisHistorie.setScreeningRonde(cervixFactory.maakRonde(dossier));
					}
				}
			}
			return null;
		default:
			throw new IllegalStateException();
		}
	}

	@Override
	public String getAanvullendeHeraanmeldLogMelding(CervixAfmelding afmelding)
	{
		return "";
	}

	private boolean isHuidigeDatumBinnenRonde0(Client client)
	{
		String startdatumBMHKString = simplePreferenceService.getString(PreferenceKey.STARTDATUM_BMHK.name());
		LocalDate startdatumBMHK = LocalDate.parse(startdatumBMHKString, DateTimeFormatter.ofPattern("yyyyMMdd"));
		LocalDate geboorteDatum = DateUtil.toLocalDate(client.getPersoon().getGeboortedatum());

		CervixLeeftijdcategorie leeftijdsCategorie = CervixLeeftijdcategorie.getLeeftijdcategorie(geboorteDatum,
			currentDateSupplier.getLocalDateTime());
		LocalDate leeftijdsCategorieRondeDatum = geboorteDatum.plusYears(leeftijdsCategorie.getLeeftijd());

		return leeftijdsCategorieRondeDatum.isBefore(startdatumBMHK);
	}
}
