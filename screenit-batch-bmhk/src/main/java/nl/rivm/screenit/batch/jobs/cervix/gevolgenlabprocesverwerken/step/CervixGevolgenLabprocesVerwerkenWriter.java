package nl.rivm.screenit.batch.jobs.cervix.gevolgenlabprocesverwerken.step;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
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

import javax.annotation.Nullable;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.batch.jobs.cervix.gevolgenlabprocesverwerken.CervixGevolgenLabprocesVerwerkenConstants;
import nl.rivm.screenit.batch.jobs.helpers.BaseWriter;
import nl.rivm.screenit.model.ScreeningRondeStatus;
import nl.rivm.screenit.model.cervix.CervixAfmelding;
import nl.rivm.screenit.model.cervix.CervixBrief;
import nl.rivm.screenit.model.cervix.CervixDossier;
import nl.rivm.screenit.model.cervix.CervixMonster;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde;
import nl.rivm.screenit.model.cervix.CervixUitstrijkje;
import nl.rivm.screenit.model.cervix.enums.CervixHpvBeoordelingWaarde;
import nl.rivm.screenit.model.cervix.enums.CervixLabformulierStatus;
import nl.rivm.screenit.model.cervix.enums.CervixMonsterType;
import nl.rivm.screenit.model.cervix.enums.CervixOmissieType;
import nl.rivm.screenit.model.cervix.enums.CervixUitstrijkjeStatus;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BezwaarType;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.HuisartsBerichtType;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.service.BaseAfmeldService;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.BezwaarService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.cervix.CervixBaseScreeningrondeService;
import nl.rivm.screenit.service.cervix.CervixFactory;
import nl.rivm.screenit.service.cervix.CervixHuisartsBerichtService;
import nl.rivm.screenit.service.cervix.CervixMailService;
import nl.rivm.screenit.service.cervix.CervixVervolgService;
import nl.rivm.screenit.service.cervix.impl.CervixOmissiesLabproces;
import nl.rivm.screenit.service.cervix.impl.CervixVervolg;
import nl.rivm.screenit.util.AfmeldingUtil;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.cervix.CervixMonsterUtil;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.context.ApplicationContext;
import org.springframework.stereotype.Component;

@Component
@Slf4j
@AllArgsConstructor
public class CervixGevolgenLabprocesVerwerkenWriter extends BaseWriter<CervixMonster>
{

	private final ApplicationContext appContext;

	private final CervixVervolgService vervolgService;

	private final ICurrentDateSupplier dateSupplier;

	private final BaseAfmeldService baseAfmeldService;

	private final BaseBriefService briefService;

	private final CervixHuisartsBerichtService huisartsBerichtService;

	private final CervixFactory factory;

	private final HibernateService hibernateService;

	private final LogService logService;

	private final CervixBaseScreeningrondeService screeningrondeService;

	private final CervixMailService mailService;

	private final BezwaarService bezwaarService;

	@Override
	protected void write(CervixMonster monster) throws Exception
	{
		monster = (CervixMonster) HibernateHelper.deproxy(monster);
		CervixVervolg vervolg;
		try
		{
			var startdatumAanleveringGenotyperingString = (String) getJobExecution().getExecutionContext()
				.get(PreferenceKey.CERVIX_START_AANLEVERING_GENOTYPERING_EN_INVOERING_TRIAGE.name());
			vervolg = vervolgService.bepaalVervolg(monster, DateUtil.parseLocalDateForPattern(startdatumAanleveringGenotyperingString, "yyyyMMdd"));
		}
		catch (IllegalStateException | NullPointerException e)
		{
			logService.logGebeurtenis(LogGebeurtenis.CERVIX_GEVOLGEN_LABPROCES_VERWERKEN_VERVOLG_BEPALEN_MISLUKT,
				monster.getOntvangstScreeningRonde().getDossier().getClient(),
				"Er kon geen vervolg worden bepaald voor monster id " + monster.getMonsterId(),
				Bevolkingsonderzoek.CERVIX);
			LOG.error("Er kon geen vervolg worden bepaald voor client (id: '{}'), monster (id: '{}')",
				monster.getOntvangstScreeningRonde().getDossier().getClient().getId(), monster.getId(), e);
			return;
		}

		if (monster.getOntvangstScreeningRonde() != null
			&& monster.getOntvangstScreeningRonde().getControleUitstrijkjeDatum() == null
			&& monster.getOntvangstdatum() != null
			&& vervolg.getIntervalControleUitstrijkje() != null
			&& monster instanceof CervixUitstrijkje
			&& ((CervixUitstrijkje) monster).getCytologieVerslag() != null)
		{
			monster.getOntvangstScreeningRonde()
				.setControleUitstrijkjeDatum(DateUtil.toLocalDate(((CervixUitstrijkje) monster).getCytologieVerslag().getDatumVerwerkt())
					.plusMonths(vervolg.getIntervalControleUitstrijkje()));
		}

		try
		{

			CervixOmissiesLabproces.Omissie omissie = null;
			if (vervolg.getBriefType() == null)
			{
				omissie = appContext.getBean(CervixOmissiesLabproces.class, monster).bepaalOmissieEnVoerActieUit();
			}

			annuleringen(monster);
			huisartsOnbekendBrief(monster);

			if (vervolg.getBriefType() == null)
			{
				if (omissie == null)
				{
					aantalContextOphogen(CervixGevolgenLabprocesVerwerkenConstants.AANTAL_IN_LABPROCES_KEY);
				}
				else if (omissie.getBriefType() == null)
				{
					aantalContextOphogen(CervixGevolgenLabprocesVerwerkenConstants.AANTAL_IN_LABPROCES_KEY);

					var uitstrijkjeOntbreektHuisartsBericht = omissie.getUitstrijkjeOntbreektHuisartsBericht();
					if (uitstrijkjeOntbreektHuisartsBericht != null)
					{
						aantallenContextOphogen(CervixGevolgenLabprocesVerwerkenConstants.TOTAAL_AANTAL_HUISARTSBERICHTEN_KEY,
							CervixGevolgenLabprocesVerwerkenConstants.HUISARTSBERICHT_TYPE_KEY, uitstrijkjeOntbreektHuisartsBericht.getBerichtType());
					}
				}
				else
				{
					labprocesAfronden(monster, omissie);
				}
			}
			else
			{
				labprocesAfronden(monster, vervolg);
			}

		}
		catch (Exception e)
		{
			LOG.error("Exceptie voor client (id: '{}') en monster (id: '{}')", monster.getOntvangstScreeningRonde().getDossier().getClient().getId(), monster.getId());
			throw e;
		}
	}

	private void labprocesAfronden(CervixMonster monster, CervixVervolg vervolg)
	{
		volgendeRondeVanaf(monster, vervolg);
		var heraanmeldTekstKey = bepaalHeraanmeldKeyEnMeldOpnieuwAan(monster);
		var brief = brief(monster, vervolg, heraanmeldTekstKey);
		huisartsbericht(monster, vervolg);
		uitnodiging(monster, brief);
		vervolgonderzoek(monster, vervolg);
		rondeSluiten(monster, vervolg);
		verwerkBezwaar(monster);
		vervolgService.digitaalLabformulierKlaarVoorCytologie(monster, vervolg.getVervolgTekst());
	}

	private void labprocesAfronden(CervixMonster monster, CervixOmissiesLabproces.Omissie omissie)
	{
		var heraanmeldTekstKey = bepaalHeraanmeldKeyEnMeldOpnieuwAan(monster);
		var brief = brief(monster, omissie, heraanmeldTekstKey);
		huisartsbericht(monster, omissie);
		uitnodiging(monster, brief);
		verwerkBezwaar(monster);
	}

	private void verwerkBezwaar(CervixMonster monster)
	{
		var client = monster.getUitnodiging().getBrief().getClient();
		if (client.getLaatstVoltooideBezwaarMoment() != null
			&& monster.getOntvangstdatum() != null
			&& bezwaarService.checkBezwaarInLaatsteBezwaarMomentAanwezigIs(client, BezwaarType.GEEN_GEBRUIK_LICHAAMSMATERIAAL_WETENSCHAPPELIJK_ONDERZOEK)
		)
		{
			mailService.sendBMHKBezwaarLichaamsmateriaalMailAsync(monster);
		}
		if (client.getLaatstVoltooideBezwaarMoment() != null
			&& monster.getOntvangstdatum() != null
			&& bezwaarService.checkBezwaarInLaatsteBezwaarMomentAanwezigIs(client, BezwaarType.GEEN_SIGNALERING_VERWIJSADVIES)
		)
		{
			mailService.sendBMHKBezwaarControlleVerwijsAdviesMail(monster);
		}
	}

	private void annuleringen(CervixMonster monster)
	{
		var ontvangstRonde = monster.getOntvangstScreeningRonde();
		screeningrondeService.annuleerHerinnering(ontvangstRonde);
		screeningrondeService.annuleerNietVerstuurdeZAS(ontvangstRonde);
		screeningrondeService.annuleerUitstel(ontvangstRonde);
	}

	private void huisartsOnbekendBrief(CervixMonster monster)
	{
		if (CervixMonsterUtil.isUitstrijkje(monster))
		{

			var uitstrijkje = CervixMonsterUtil.getUitstrijkje(monster);
			if (uitstrijkje.getUitstrijkjeStatus() != CervixUitstrijkjeStatus.NIET_ONTVANGEN)
			{
				var labformulier = uitstrijkje.getLabformulier();
				if (labformulier != null && labformulier.getStatus() == CervixLabformulierStatus.HUISARTS_ONBEKEND && labformulier.getHuisartsOnbekendBrief() == null)
				{
					var ontvangstRonde = monster.getOntvangstScreeningRonde();

					if (ontvangstRonde.getUitstrijkjeCytologieUitslag() == null && ontvangstRonde.getMonsterHpvUitslag() != null
						&& ontvangstRonde.getMonsterHpvUitslag().getLaatsteHpvBeoordeling().getHpvUitslag() == CervixHpvBeoordelingWaarde.POSITIEF
						|| ontvangstRonde.getUitstrijkjeVervolgonderzoekUitslag() == null && ontvangstRonde.getInVervolgonderzoekDatum() != null
						&& uitstrijkje.getOntvangstdatum().after(ontvangstRonde.getInVervolgonderzoekDatum()))
					{
						var heraanmeldTekstKey = bepaalHeraanmeldKeyEnMeldOpnieuwAan(monster);
						var huisartsOnbekendBrief = briefService.maakBvoBrief(ontvangstRonde, BriefType.CERVIX_HUISARTS_ONBEKEND);
						huisartsOnbekendBrief.setHeraanmeldenTekstKey(heraanmeldTekstKey);
						huisartsOnbekendBrief.setLabformulier(labformulier);
						labformulier.setHuisartsOnbekendBrief(huisartsOnbekendBrief);

						hibernateService.saveOrUpdateAll(huisartsOnbekendBrief, labformulier);
						aantallenContextOphogen(CervixGevolgenLabprocesVerwerkenConstants.TOTAAL_AANTAL_BRIEVEN_KEY, CervixGevolgenLabprocesVerwerkenConstants.BRIEF_TYPE_KEY,
							BriefType.CERVIX_HUISARTS_ONBEKEND);
					}
				}
			}
		}
	}

	private void volgendeRondeVanaf(CervixMonster monster, CervixVervolg vervolg)
	{
		var leeftijdcategorieVolgendeRonde = vervolg.getLeeftijdcategorieVolgendeRonde();
		if (leeftijdcategorieVolgendeRonde != null)
		{
			var dossier = monster.getOntvangstScreeningRonde().getDossier();
			var geboortedatum = DateUtil.toLocalDate(dossier.getClient().getPersoon().getGeboortedatum());

			var volgendeRondeVanaf = geboortedatum.plusYears(leeftijdcategorieVolgendeRonde.getLeeftijd());
			if (geboortedatum.getDayOfMonth() != volgendeRondeVanaf.getDayOfMonth())
			{

				volgendeRondeVanaf = volgendeRondeVanaf.plusDays(1);
			}
			dossier.setVolgendeRondeVanaf(DateUtil.toUtilDate(volgendeRondeVanaf));

			hibernateService.saveOrUpdate(dossier);
		}
	}

	private PreferenceKey bepaalHeraanmeldKeyEnMeldOpnieuwAan(CervixMonster monster)
	{
		PreferenceKey heraanmeldenTekstKey = null;

		var ontvangstRonde = monster.getOntvangstScreeningRonde();
		var dossier = ontvangstRonde.getDossier();

		if (ontvangstRonde.equals(dossier.getLaatsteScreeningRonde()) && AfmeldingUtil.isEenmaligOfDefinitefAfgemeld(dossier))
		{
			heraanmeldenTekstKey = bepaalTekstKeyActieveAfmelding(ontvangstRonde, dossier);
			heraanmeldenVoordatUitslagGemaaktWordt(monster);
		}
		return heraanmeldenTekstKey;
	}

	private static PreferenceKey bepaalTekstKeyActieveAfmelding(CervixScreeningRonde ontvangstRonde, CervixDossier dossier)
	{
		PreferenceKey heraanmeldenTekstKey = null;
		if (AfmeldingUtil.isAfgerondeDefinitieveAfmelding(dossier.getLaatsteAfmelding()))
		{
			heraanmeldenTekstKey = PreferenceKey.CERVIX_DEFINITIEF_HERAANMELDEN_TEKST;
		}
		else if (AfmeldingUtil.isAfgerondeEenmaligeAfmelding(ontvangstRonde.getLaatsteAfmelding()))
		{
			heraanmeldenTekstKey = PreferenceKey.CERVIX_EENMALIG_HERAANMELDEN_TEKST;
		}
		return heraanmeldenTekstKey;
	}

	private void heraanmeldenVoordatUitslagGemaaktWordt(CervixMonster monster)
	{
		CervixAfmelding afmelding = null;

		var ontvangstRonde = monster.getOntvangstScreeningRonde();
		var dossier = ontvangstRonde.getDossier();
		if (AfmeldingUtil.isAfgerondeDefinitieveAfmelding(dossier.getLaatsteAfmelding()))
		{
			afmelding = dossier.getLaatsteAfmelding();
			aantalContextOphogen(CervixGevolgenLabprocesVerwerkenConstants.AANTAL_DEFINITIEF_HERAANGEMELD_KEY);
		}
		else if (AfmeldingUtil.isAfgerondeEenmaligeAfmelding(ontvangstRonde.getLaatsteAfmelding()))
		{
			afmelding = ontvangstRonde.getLaatsteAfmelding();
			aantalContextOphogen(CervixGevolgenLabprocesVerwerkenConstants.AANTAL_EENMALIG_HERAANGEMELD_KEY);
		}
		if (afmelding != null)
		{
			baseAfmeldService.heraanmeldenZonderVervolg(afmelding);
		}
	}

	private CervixBrief brief(CervixMonster monster, CervixVervolg vervolg, PreferenceKey heraanmeldenTekstKey)
	{
		return brief(monster, vervolg.getBriefType(), heraanmeldenTekstKey, null);
	}

	private CervixBrief brief(CervixMonster monster, CervixOmissiesLabproces.Omissie omissie, PreferenceKey heraanmeldenTekstKey)
	{
		return brief(monster, omissie.getBriefType(), heraanmeldenTekstKey, omissie.getOmissieType());
	}

	private CervixBrief brief(CervixMonster monster, BriefType briefType, PreferenceKey heraanmeldenTekstKey, @Nullable CervixOmissieType omissieType)
	{
		var brief = briefService.maakBvoBrief(monster.getOntvangstScreeningRonde(), briefType);
		monster.setBrief(brief);
		brief.setMonster(monster);
		brief.setHeraanmeldenTekstKey(heraanmeldenTekstKey);
		brief.setOmissieType(omissieType);

		hibernateService.saveOrUpdate(monster);
		hibernateService.saveOrUpdate(brief);
		aantallenContextOphogen(CervixGevolgenLabprocesVerwerkenConstants.TOTAAL_AANTAL_BRIEVEN_KEY, CervixGevolgenLabprocesVerwerkenConstants.BRIEF_TYPE_KEY, briefType);
		return brief;
	}

	private void huisartsbericht(CervixMonster monster, CervixVervolg vervolg)
	{
		huisartsbericht(monster, vervolg.getHuisartsBerichtType(), null);
	}

	private void huisartsbericht(CervixMonster monster, CervixOmissiesLabproces.Omissie omissie)
	{
		huisartsbericht(monster, omissie.getHuisartsBerichtType(), omissie.getOmissieType());
	}

	private void huisartsbericht(CervixMonster monster, HuisartsBerichtType huisartsBerichtType, CervixOmissieType omissieType)
	{
		var client = monster.getOntvangstScreeningRonde().getDossier().getClient();
		if (monster instanceof CervixUitstrijkje)
		{
			huisartsBerichtService.maakCervixHuisartsBericht(huisartsBerichtType, client, (CervixUitstrijkje) HibernateHelper.deproxy(monster), omissieType);

			aantallenContextOphogen(CervixGevolgenLabprocesVerwerkenConstants.TOTAAL_AANTAL_HUISARTSBERICHTEN_KEY,
				CervixGevolgenLabprocesVerwerkenConstants.HUISARTSBERICHT_TYPE_KEY, huisartsBerichtType);
		}
	}

	private void uitnodiging(CervixMonster monster, CervixBrief brief)
	{
		var briefType = brief.getBriefType();
		if (CervixMonsterType.getMonsterType(briefType) != null)
		{
			var herinneren = briefType != BriefType.CERVIX_UITSTRIJKJE_TWEEDE_KEER_ONBEOORDEELBAAR && briefType != BriefType.CERVIX_ZAS_TWEEDE_KEER_ONBEOORDEELBAAR;

			factory.maakUitnodiging(monster.getOntvangstScreeningRonde(), brief, herinneren, false);
		}
	}

	private void vervolgonderzoek(CervixMonster monster, CervixVervolg vervolg)
	{
		if (vervolg.getInVervolgonderzoekDatum() != null)
		{
			CervixScreeningRonde ontvangstRonde = monster.getOntvangstScreeningRonde();
			ontvangstRonde.setInVervolgonderzoekDatum(vervolg.getInVervolgonderzoekDatum());

			hibernateService.saveOrUpdate(ontvangstRonde);
			aantalContextOphogen(CervixGevolgenLabprocesVerwerkenConstants.AANTAL_IN_VERVOLGONDERZOEK_KEY);
		}
	}

	private void rondeSluiten(CervixMonster monster, CervixVervolg vervolg)
	{
		var ontvangstRonde = monster.getOntvangstScreeningRonde();
		if (vervolg.sluitRonde() && ontvangstRonde.getStatus() != ScreeningRondeStatus.AFGEROND)
		{
			ontvangstRonde.setStatus(ScreeningRondeStatus.AFGEROND);
			ontvangstRonde.setStatusDatum(dateSupplier.getDate());

			hibernateService.saveOrUpdate(ontvangstRonde);
			aantalContextOphogen(CervixGevolgenLabprocesVerwerkenConstants.AANTAL_RONDEN_GESLOTEN_KEY);
		}
	}
}
