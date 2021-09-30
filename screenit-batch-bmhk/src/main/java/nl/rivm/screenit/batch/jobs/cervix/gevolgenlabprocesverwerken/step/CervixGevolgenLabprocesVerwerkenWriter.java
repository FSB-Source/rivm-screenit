package nl.rivm.screenit.batch.jobs.cervix.gevolgenlabprocesverwerken.step;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import javax.annotation.Nullable;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.batch.jobs.cervix.gevolgenlabprocesverwerken.CervixGevolgenLabprocesVerwerkenConstants;
import nl.rivm.screenit.batch.jobs.helpers.BaseWriter;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ScreeningRondeStatus;
import nl.rivm.screenit.model.cervix.CervixAfmelding;
import nl.rivm.screenit.model.cervix.CervixBrief;
import nl.rivm.screenit.model.cervix.CervixDossier;
import nl.rivm.screenit.model.cervix.CervixHuisartsBericht;
import nl.rivm.screenit.model.cervix.CervixLabformulier;
import nl.rivm.screenit.model.cervix.CervixMonster;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde;
import nl.rivm.screenit.model.cervix.CervixUitstrijkje;
import nl.rivm.screenit.model.cervix.enums.CervixHpvUitslag;
import nl.rivm.screenit.model.cervix.enums.CervixLabformulierStatus;
import nl.rivm.screenit.model.cervix.enums.CervixLeeftijdcategorie;
import nl.rivm.screenit.model.cervix.enums.CervixMonsterType;
import nl.rivm.screenit.model.cervix.enums.CervixOmissieType;
import nl.rivm.screenit.model.cervix.enums.CervixUitstrijkjeStatus;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BezwaarType;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.HuisartsBerichtType;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.logging.LogEvent;
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
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.joda.time.DateTime;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;

public class CervixGevolgenLabprocesVerwerkenWriter extends BaseWriter<CervixMonster>
{

	private static final Logger LOG = LoggerFactory.getLogger(CervixGevolgenLabprocesVerwerkenWriter.class);

	@Autowired
	private ApplicationContext appContext;

	@Autowired
	private CervixVervolgService vervolgService;

	@Autowired
	private ICurrentDateSupplier dateSupplier;

	@Autowired
	private BaseAfmeldService baseAfmeldService;

	@Autowired
	private BaseBriefService briefService;

	@Autowired
	private CervixHuisartsBerichtService huisartsBerichtService;

	@Autowired
	private CervixFactory factory;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private LogService logService;

	@Autowired
	private CervixBaseScreeningrondeService screeningrondeService;

	@Autowired
	private CervixMailService mailService;

	@Autowired
	private BezwaarService bezwaarService;

	@Override
	protected void write(CervixMonster monster) throws Exception
	{
		monster = (CervixMonster) HibernateHelper.deproxy(monster);
		CervixVervolg vervolg = null;
		try
		{
			vervolg = vervolgService.bepaalVervolg(monster);
		}
		catch (IllegalStateException | NullPointerException e)
		{
			logService.logGebeurtenis(LogGebeurtenis.CERVIX_GEVOLGEN_LABPROCES_VERWERKEN_VERVOLG_BEPALEN_MISLUKT,
				new LogEvent("Er kon geen vervolg worden bepaald voor monster-id " + monster.getMonsterId()), null,
				monster.getOntvangstScreeningRonde().getDossier().getClient(), Bevolkingsonderzoek.CERVIX);
			LOG.error(
				"Er kon geen vervolg worden bepaald voor client met id: " + monster.getOntvangstScreeningRonde().getDossier().getClient().getId() + " monster-id: "
					+ monster.getMonsterId(),
				e);
			return;
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
				else
				{
					if (omissie.getBriefType() == null)
					{
						aantalContextOphogen(CervixGevolgenLabprocesVerwerkenConstants.AANTAL_IN_LABPROCES_KEY);

						CervixHuisartsBericht uitstrijkjeOntbreektHuisartsBericht = omissie.getUitstrijkjeOntbreektHuisartsBericht();
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
			}
			else
			{
				labprocesAfronden(monster, vervolg);
			}

		}
		catch (Exception e)
		{
			LOG.error("Exceptie voor client(id:" + monster.getOntvangstScreeningRonde().getDossier().getClient().getId() + ") monster-id: " + monster.getMonsterId());
			throw e;
		}
	}

	private void labprocesAfronden(CervixMonster monster, CervixVervolg vervolg)
	{
		volgendeRondeVanaf(monster, vervolg);
		PreferenceKey heraanmeldenTekstKey = heraanmelden(monster);
		CervixBrief brief = brief(monster, vervolg, heraanmeldenTekstKey);
		huisartsbericht(monster, vervolg);
		uitnodiging(monster, brief);
		vervolgonderzoek(monster, vervolg);
		rondeSluiten(monster, vervolg);
		verwerkBezwaar(monster);
	}

	private void labprocesAfronden(CervixMonster monster, CervixOmissiesLabproces.Omissie omissie)
	{
		PreferenceKey heraanmeldenTekstKey = heraanmelden(monster);
		CervixBrief brief = brief(monster, omissie, heraanmeldenTekstKey);
		huisartsbericht(monster, omissie);
		uitnodiging(monster, brief);
		verwerkBezwaar(monster);
	}

	private void verwerkBezwaar(CervixMonster monster)
	{
		Client client = monster.getUitnodiging().getBrief().getClient();
		if (client.getLaatstVoltooideBezwaarMoment() != null
			&& bezwaarService.checkBezwaarInLaatsteBezwaarMomentAanwezigIs(client, BezwaarType.GEEN_GEBRUIK_LICHAAMSMATERIAAL_WETENSCHAPPELIJK_ONDERZOEK))
		{
			mailService.sendBMHKBezwaarLichaamsmateriaalMailAsync(monster);
		}
		if (client.getLaatstVoltooideBezwaarMoment() != null
			&& bezwaarService.checkBezwaarInLaatsteBezwaarMomentAanwezigIs(client, BezwaarType.GEEN_SIGNALERING_VERWIJSADVIES))
		{
			mailService.sendBMHKBezwaarControlleVerwijsAdviesMail(monster);
		}
	}

	private void annuleringen(CervixMonster monster)
	{
		CervixScreeningRonde ontvangstRonde = monster.getOntvangstScreeningRonde();
		screeningrondeService.annuleerHerinnering(ontvangstRonde);
		screeningrondeService.annuleerNietVerstuurdeZAS(ontvangstRonde);
		screeningrondeService.annuleerUitstel(ontvangstRonde);
	}

	private void huisartsOnbekendBrief(CervixMonster monster)
	{
		if (monster instanceof CervixUitstrijkje)
		{

			CervixUitstrijkje uitstrijkje = (CervixUitstrijkje) monster;
			if (uitstrijkje.getUitstrijkjeStatus() != CervixUitstrijkjeStatus.NIET_ONTVANGEN)
			{
				CervixLabformulier labformulier = uitstrijkje.getLabformulier();
				if (labformulier != null && labformulier.getStatus() == CervixLabformulierStatus.HUISARTS_ONBEKEND && labformulier.getHuisartsOnbekendBrief() == null)
				{
					CervixScreeningRonde ontvangstRonde = monster.getOntvangstScreeningRonde();

					if (ontvangstRonde.getUitstrijkjeCytologieUitslag() == null && ontvangstRonde.getMonsterHpvUitslag() != null
						&& ontvangstRonde.getMonsterHpvUitslag().getLaatsteHpvBeoordeling().getHpvUitslag() == CervixHpvUitslag.POSITIEF
						|| ontvangstRonde.getUitstrijkjeVervolgonderzoekUitslag() == null && ontvangstRonde.getInVervolgonderzoekDatum() != null
							&& uitstrijkje.getOntvangstdatum().after(ontvangstRonde.getInVervolgonderzoekDatum()))
					{
						PreferenceKey heraanmeldenTekstKey = heraanmelden(monster);
						CervixBrief huisartsOnbekendBrief = briefService.maakCervixBrief(ontvangstRonde, BriefType.CERVIX_HUISARTS_ONBEKEND);
						huisartsOnbekendBrief.setHeraanmeldenTekstKey(heraanmeldenTekstKey);
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
		CervixLeeftijdcategorie leeftijdcategorieVolgendeRonde = vervolg.getLeeftijdcategorieVolgendeRonde();
		if (leeftijdcategorieVolgendeRonde != null)
		{
			CervixDossier dossier = monster.getOntvangstScreeningRonde().getDossier();
			DateTime geboortedatum = new DateTime(dossier.getClient().getPersoon().getGeboortedatum());

			DateTime volgendeRondeVanaf = geboortedatum.plusYears(leeftijdcategorieVolgendeRonde.getLeeftijd());
			if (geboortedatum.getDayOfMonth() != volgendeRondeVanaf.getDayOfMonth())
			{

				volgendeRondeVanaf = volgendeRondeVanaf.plusDays(1);
			}
			dossier.setVolgendeRondeVanaf(volgendeRondeVanaf.toDate());

			hibernateService.saveOrUpdate(dossier);
		}
	}

	private PreferenceKey heraanmelden(CervixMonster monster)
	{
		PreferenceKey heraanmeldenTekstKey = null;

		if (!afgemeldNaOntvangstMonster(monster))
		{
			CervixAfmelding afmelding = null;

			CervixScreeningRonde ontvangstRonde = monster.getOntvangstScreeningRonde();
			CervixDossier dossier = ontvangstRonde.getDossier();
			if (!dossier.getAangemeld())
			{
				afmelding = dossier.getLaatsteAfmelding();
				aantalContextOphogen(CervixGevolgenLabprocesVerwerkenConstants.AANTAL_DEFINITIEF_HERAANGEMELD_KEY);
				heraanmeldenTekstKey = PreferenceKey.CERVIX_DEFINITIEF_HERAANMELDEN_TEKST;
			}
			else
			{
				if (!ontvangstRonde.getAangemeld())
				{
					afmelding = ontvangstRonde.getLaatsteAfmelding();
					aantalContextOphogen(CervixGevolgenLabprocesVerwerkenConstants.AANTAL_EENMALIG_HERAANGEMELD_KEY);
					heraanmeldenTekstKey = PreferenceKey.CERVIX_EENMALIG_HERAANMELDEN_TEKST;
				}
			}
			if (afmelding != null)
			{
				baseAfmeldService.heraanmeldenZonderVervolg(afmelding);
			}
		}
		return heraanmeldenTekstKey;
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
		CervixBrief brief = briefService.maakCervixBrief(monster.getOntvangstScreeningRonde(), briefType);
		monster.setBrief(brief);
		brief.setMonster(monster);
		brief.setHeraanmeldenTekstKey(heraanmeldenTekstKey);
		brief.setOmissieType(omissieType);

		if (afgemeldNaOntvangstMonster(monster))
		{
			brief.setTegenhouden(true);
		}

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
		Client client = monster.getOntvangstScreeningRonde().getDossier().getClient();
		if (monster instanceof CervixUitstrijkje)
		{
			huisartsBerichtService.maakCervixHuisartsBericht(huisartsBerichtType, client, (CervixUitstrijkje) HibernateHelper.deproxy(monster), omissieType);

			aantallenContextOphogen(CervixGevolgenLabprocesVerwerkenConstants.TOTAAL_AANTAL_HUISARTSBERICHTEN_KEY,
				CervixGevolgenLabprocesVerwerkenConstants.HUISARTSBERICHT_TYPE_KEY, huisartsBerichtType);
		}
	}

	private void uitnodiging(CervixMonster monster, CervixBrief brief)
	{
		BriefType briefType = brief.getBriefType();
		if (CervixMonsterType.getMonsterType(briefType) != null)
		{
			boolean herinneren = briefType != BriefType.CERVIX_UITSTRIJKJE_TWEEDE_KEER_ONBEOORDEELBAAR && briefType != BriefType.CERVIX_ZAS_TWEEDE_KEER_ONBEOORDEELBAAR;

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
		CervixScreeningRonde ontvangstRonde = monster.getOntvangstScreeningRonde();
		if (vervolg.sluitRonde() && ontvangstRonde.getStatus() != ScreeningRondeStatus.AFGEROND)
		{
			ontvangstRonde.setStatus(ScreeningRondeStatus.AFGEROND);
			ontvangstRonde.setStatusDatum(dateSupplier.getDate());

			hibernateService.saveOrUpdate(ontvangstRonde);
			aantalContextOphogen(CervixGevolgenLabprocesVerwerkenConstants.AANTAL_RONDEN_GESLOTEN_KEY);
		}
	}

	private boolean afgemeldNaOntvangstMonster(CervixMonster monster)
	{
		CervixScreeningRonde ontvangstRonde = monster.getOntvangstScreeningRonde();
		if (!ontvangstRonde.getAangemeld())
		{
			Date peildatum = monster.getOntvangstdatum();
			if (monster instanceof CervixUitstrijkje)
			{
				CervixLabformulier labformulier = ((CervixUitstrijkje) monster).getLabformulier();
				if (labformulier != null
					&& (peildatum == null || labformulier.getScanDatum().before(peildatum))
					&& (labformulier.getStatus() == CervixLabformulierStatus.GECONTROLEERD
						|| labformulier.getStatus() == CervixLabformulierStatus.GECONTROLEERD_CYTOLOGIE
						|| labformulier.getStatus() == CervixLabformulierStatus.HUISARTS_ONBEKEND))
				{
					peildatum = labformulier.getScanDatum();
				}
			}

			return DateUtil.compareAfter(ontvangstRonde.getLaatsteAfmelding().getAfmeldDatum(), peildatum);
		}
		return false;
	}
}
