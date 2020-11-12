
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

import java.util.ArrayList;
import java.util.List;

import nl.rivm.screenit.dao.cervix.CervixScreeningrondeDao;
import nl.rivm.screenit.model.cervix.CervixAfmelding;
import nl.rivm.screenit.model.cervix.CervixBrief;
import nl.rivm.screenit.model.cervix.CervixDossier;
import nl.rivm.screenit.model.cervix.CervixHpvBeoordeling;
import nl.rivm.screenit.model.cervix.CervixHuisartsBericht;
import nl.rivm.screenit.model.cervix.CervixLabformulier;
import nl.rivm.screenit.model.cervix.CervixMonster;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde;
import nl.rivm.screenit.model.cervix.CervixUitnodiging;
import nl.rivm.screenit.model.cervix.CervixUitstel;
import nl.rivm.screenit.model.cervix.CervixUitstrijkje;
import nl.rivm.screenit.model.cervix.CervixZas;
import nl.rivm.screenit.model.cervix.cis.CervixCISHistorie;
import nl.rivm.screenit.model.cervix.enums.CervixLabformulierStatus;
import nl.rivm.screenit.model.cervix.enums.CervixMonsterType;
import nl.rivm.screenit.model.cervix.enums.CervixUitstrijkjeStatus;
import nl.rivm.screenit.model.cervix.enums.CervixZasStatus;
import nl.rivm.screenit.model.cervix.facturatie.CervixBetaalopdrachtRegelSpecificatie;
import nl.rivm.screenit.model.cervix.facturatie.CervixBoekRegel;
import nl.rivm.screenit.model.cervix.facturatie.CervixVerrichting;
import nl.rivm.screenit.model.cervix.verslag.CervixVerslag;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.cervix.CervixBaseScreeningrondeService;
import nl.rivm.screenit.service.cervix.CervixVervolgService;
import nl.rivm.screenit.service.cervix.enums.CervixVervolgTekst;
import nl.rivm.screenit.util.cervix.CervixMonsterUtil;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;
import nl.topicuszorg.util.collections.CollectionUtils;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS)
public class CervixBaseScreeningrondeServiceImpl implements CervixBaseScreeningrondeService
{
	@Autowired
	private SimplePreferenceService simplePreferenceService;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private ICurrentDateSupplier dateSupplier;

	@Autowired
	private CervixVervolgService vervolgService;

	@Autowired
	private CervixScreeningrondeDao screeningrondeDao;

	@Override
	public void annuleerNietVerstuurdeZAS(CervixScreeningRonde ronde)
	{
		for (CervixUitnodiging anderUitnodiging : ronde.getUitnodigingen())
		{
			if (anderUitnodiging.getMonsterType() == CervixMonsterType.ZAS)
			{
				if (anderUitnodiging.getVerstuurdDatum() == null)
				{
					if (anderUitnodiging.getGeannuleerdDatum() == null)
					{
						anderUitnodiging.setGeannuleerdDatum(dateSupplier.getDateTime().minusMillis(50).toDate());
						hibernateService.saveOrUpdate(anderUitnodiging);
						break;
					}
				}
			}
		}
	}

	@Override
	public void annuleerUitstel(CervixScreeningRonde ronde)
	{
		CervixUitstel uitstel = ronde.getUitstel();
		if (uitstel != null)
		{
			if (uitstel.getGeannuleerdDatum() == null)
			{
				uitstel.setGeannuleerdDatum(dateSupplier.getDateTime().minusMillis(50).toDate());
				hibernateService.saveOrUpdate(uitstel);
			}
		}
	}

	@Override
	public void annuleerHerinnering(CervixScreeningRonde ronde)
	{
		List<CervixUitnodiging> uitnodigingen = screeningrondeDao.getTeHerinnerenUitnodigingen(ronde);
		for (CervixUitnodiging uitnodiging : uitnodigingen)
		{
			if (uitnodiging.getHerinnerenGeannuleerdDatum() == null)
			{
				uitnodiging.setHerinnerenGeannuleerdDatum(dateSupplier.getDateTime().minusMillis(50).toDate());
				hibernateService.saveOrUpdate(uitnodiging);
			}
		}
	}

	@Override
	public boolean heeftUitslagOfHeeftGehad(CervixUitnodiging cervixUitnodiging)
	{
		CervixZas zas = CervixMonsterUtil.getZAS(cervixUitnodiging.getMonster());
		return zas != null && zas.getZasStatus() != CervixZasStatus.VERSTUURD
			|| cervixUitnodiging.getScreeningRonde().getDossier().getLaatsteScreeningRonde().getMonsterHpvUitslag() != null;
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void verwijderCervixScreeningRondes(CervixDossier dossier)
	{
		List<CervixScreeningRonde> rondes = dossier.getScreeningRondes();
		dossier.setLaatsteScreeningRonde(null);
		dossier.setScreeningRondes(new ArrayList<>());
		if (CollectionUtils.isNotEmpty(rondes))
		{
			for (CervixScreeningRonde ronde : rondes)
			{
				verwijderCervixScreeningRonde(ronde);
			}
		}
	}

	@Override
	public void verwijderCervixScreeningRonde(CervixScreeningRonde ronde)
	{
		List<CervixUitnodiging> uitnodigingen = ronde.getUitnodigingen();
		for (CervixUitnodiging uitnodiging : uitnodigingen)
		{
			if (uitnodiging.getMonster() != null)
			{
				CervixMonster monster = uitnodiging.getMonster();
				for (CervixVerrichting verrichting : monster.getVerrichtingen())
				{
					for (CervixBoekRegel boekRegel : verrichting.getBoekRegels())
					{
						boekRegel.setVerrichting(null);
						hibernateService.delete(boekRegel);
						if (boekRegel.getSpecificatie() != null)
						{
							CervixBetaalopdrachtRegelSpecificatie specificatie = boekRegel.getSpecificatie();
							specificatie.getBoekRegels().remove(boekRegel);
							hibernateService.saveOrUpdate(specificatie);
						}
					}
					hibernateService.delete(verrichting);
				}
				for (CervixHpvBeoordeling hpvBeoordeling : monster.getHpvBeoordelingen())
				{
					hibernateService.delete(hpvBeoordeling);
				}
				switch (uitnodiging.getMonsterType())
				{
				case UITSTRIJKJE:
					CervixUitstrijkje uitstrijkje = (CervixUitstrijkje) HibernateHelper.deproxy(monster);
					if (uitstrijkje != null)
					{
						if (uitstrijkje.getLabformulier() != null)
						{
							hibernateService.delete(uitstrijkje.getLabformulier());
						}
						if (uitstrijkje.getCytologieVerslag() != null)
						{
							hibernateService.delete(uitstrijkje.getCytologieVerslag());
						}
						if (uitstrijkje.getCytologieOrder() != null)
						{
							hibernateService.delete(uitstrijkje.getCytologieOrder());
						}
						hibernateService.delete(uitstrijkje);
					}
					break;
				case ZAS:
					CervixZas zas = (CervixZas) HibernateHelper.deproxy(monster);
					hibernateService.delete(zas);
					break;
				}
			}
			CervixBrief uitnodigingBrief = uitnodiging.getBrief();
			hibernateService.delete(uitnodiging);
			hibernateService.delete(uitnodigingBrief);
		}

		List<CervixBrief> brieven = ronde.getBrieven();
		for (CervixBrief brief : brieven)
		{
			hibernateService.delete(brief);
		}

		List<CervixHuisartsBericht> berichten = ronde.getHuisartsBerichten();
		for (CervixHuisartsBericht bericht : berichten)
		{
			hibernateService.delete(bericht);
		}

		List<CervixVerslag> verslagen = ronde.getVerslagen();
		if (CollectionUtils.isNotEmpty(verslagen))
		{
			hibernateService.deleteAll(verslagen);
		}

		if (CollectionUtils.isNotEmpty(ronde.getAfmeldingen()))
		{
			for (CervixAfmelding afmelding : ronde.getAfmeldingen())
			{
				hibernateService.delete(afmelding);
			}
		}
		hibernateService.delete(ronde);
	}

	@Override
	public boolean heeftUitnodigingMetMonsterInLabproces(CervixScreeningRonde ronde)
	{

		for (CervixUitnodiging uitnodiging : ronde.getUitnodigingen())
		{
			CervixMonster monster = uitnodiging.getMonster();
			if (monster != null && monster.getBrief() == null)
			{
				switch (uitnodiging.getMonsterType())
				{
				case UITSTRIJKJE:
					CervixUitstrijkje uitstrijkje = (CervixUitstrijkje) HibernateHelper.deproxy(monster);
					CervixLabformulier labformulier = uitstrijkje.getLabformulier();
					if (uitstrijkje.getUitstrijkjeStatus() != CervixUitstrijkjeStatus.NIET_ONTVANGEN
						|| labformulier != null && (labformulier.getStatus() == CervixLabformulierStatus.GECONTROLEERD
							|| labformulier.getStatus() == CervixLabformulierStatus.GECONTROLEERD_CYTOLOGIE))
					{
						return true; 
					}
					break;
				case ZAS:
					if (((CervixZas) HibernateHelper.deproxy(monster)).getZasStatus() != CervixZasStatus.VERSTUURD)
					{
						return true; 
					}
					break;
				}
			}
		}
		return false;
	}

	@Override
	public boolean isFrisseStart(CervixScreeningRonde ronde)
	{
		CervixCISHistorie cisHistorie = ronde.getDossier().getCisHistorie();
		return cisHistorie != null
			&& ronde.equals(cisHistorie.getScreeningRonde())
			&& !cisHistorie.isHeeftPap0()
			&& cisHistorie.getAfmelding() == null
			&& ronde.getUitstel() == null;
	}

	@Override
	public boolean kanFrisseStartMaken(CervixScreeningRonde ronde)
	{
		return ronde.getLaatsteUitnodiging() == null && isFrisseStart(ronde);
	}

	@Override
	public boolean hasValidScreeningRonde(CervixMonster monster)
	{
		CervixVervolgTekst cervixVervolg = vervolgService.bepaalVervolg(monster).getVervolgTekst();
		return cervixVervolg == CervixVervolgTekst.UITSTRIJKJE_REGISTREER_ONTVANGST
			|| cervixVervolg == CervixVervolgTekst.UITSTRIJKJE_ONTVANGEN_NAAR_HPV
			|| cervixVervolg == CervixVervolgTekst.UITSTRIJKJE_GEANALYSEERD_OP_HPV_POGING_1_ONGELDIG_NAAR_HPV
			|| cervixVervolg == CervixVervolgTekst.UITSTRIJKJE_GEANALYSEERD_OP_HPV_POGING_2_ONGELDIG_VERNIETIG
			|| cervixVervolg == CervixVervolgTekst.UITSTRIJKJE_HPV_POSITIEF_NAAR_CYTOLOGIE
			|| cervixVervolg == CervixVervolgTekst.UITSTRIJKJE_REEDS_HPV_UITSLAG_NAAR_CYTOLOGIE
			|| cervixVervolg == CervixVervolgTekst.UITSTRIJKJE_VERVOLGONDERZOEK_NAAR_CYTOLOGIE
			|| cervixVervolg == CervixVervolgTekst.UITSTRIJKJE_NIET_ANALYSEERBAAR
			|| cervixVervolg == CervixVervolgTekst.UITSTRIJKJE_HPV_NEGATIEF_BEWAAR
			|| cervixVervolg == CervixVervolgTekst.UITSTRIJKJE_CYTOLOGIE_UITSLAG_BEWAAR
			|| cervixVervolg == CervixVervolgTekst.UITSTRIJKJE_HPV_POSITIEF_CYTOLOGIE_ONBEOORDEELBAAR_BEWAAR
			|| cervixVervolg == CervixVervolgTekst.UITSTRIJKJE_CLIENT_REEDS_GEINFORMEERD_BEWAAR
			|| cervixVervolg == CervixVervolgTekst.UITSTRIJKJE_CLIENT_REEDS_GEINFORMEERD_VERNIETIG;
	}

	@Override
	public CervixScreeningRonde getLaatsteScreeningRonde(String bsn)
	{
		return screeningrondeDao.getLaatsteScreeningRonde(bsn);
	}

}
