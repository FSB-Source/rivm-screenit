package nl.rivm.screenit.service.cervix.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import nl.rivm.screenit.model.GbaPersoon;
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
import nl.rivm.screenit.model.colon.ColonDossier;
import nl.rivm.screenit.service.cervix.CervixTestTimelineTimeService;
import nl.rivm.screenit.service.cervix.enums.CervixTestTimeLineDossierTijdstip;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;
import nl.topicuszorg.hibernate.object.model.HibernateObject;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.commons.beanutils.BeanUtils;
import org.apache.commons.beanutils.ConvertUtils;
import org.apache.commons.beanutils.PropertyUtils;
import org.apache.commons.beanutils.converters.DateConverter;
import org.joda.time.DateTime;
import org.joda.time.Days;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.REQUIRED)
public class CervixTestTimelineTimeServiceImpl implements CervixTestTimelineTimeService
{

	private static final Logger LOG = LoggerFactory.getLogger(CervixTestTimelineTimeService.class);

	@Autowired
	private HibernateService hibernateService;

	@Override
	public boolean rekenDossierTerug(CervixDossier dossier, CervixTestTimeLineDossierTijdstip tijdstip)
	{
		int dagen = aantalDagenCalculator(dossier, tijdstip);
		rekenDossierTerug(dossier, dagen);
		return true;
	}

	@Override
	public boolean rekenDossierTerug(CervixDossier dossier, int aantalDagen)
	{
		LOG.debug("Dossier aantal dagen terug gezet: " + aantalDagen);
		rekenObjectTerug(dossier, aantalDagen);
		hibernateService.saveOrUpdate(dossier);

		for (CervixScreeningRonde ronde : dossier.getScreeningRondes())
		{
			rekenRondeTerug(ronde, aantalDagen);
		}
		for (CervixAfmelding afmelding : dossier.getAfmeldingen())
		{
			rekenAfmeldingTerug(afmelding, aantalDagen);
		}

		rekenAllePersoonsDatumTerug(dossier.getClient().getPersoon(), aantalDagen);
		return true;
	}

	private void rekenAllePersoonsDatumTerug(GbaPersoon persoon, int aantalDagen)
	{
		if (persoon.getOverlijdensdatum() != null)
		{
			persoon.setOverlijdensdatum(new DateTime(persoon.getOverlijdensdatum()).minusDays(aantalDagen).toDate());
		}
		if (persoon.getDatumVertrokkenUitNederland() != null)
		{
			persoon.setDatumVertrokkenUitNederland(new DateTime(persoon.getDatumVertrokkenUitNederland()).minusDays(aantalDagen).toDate());
		}
		if (persoon.getDatumVestigingNederland() != null)
		{
			persoon.setDatumVestigingNederland(new DateTime(persoon.getDatumVestigingNederland()).minusDays(aantalDagen).toDate());
		}
		hibernateService.saveOrUpdate(persoon);
	}

	private void rekenRondeTerug(CervixScreeningRonde ronde, int aantalDagen)
	{
		rekenObjectTerug(ronde, aantalDagen);
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
		rekenObjectTerug(uitnodiging, aantalDagen);
		hibernateService.saveOrUpdate(uitnodiging);

		rekenMonsterTerug(uitnodiging.getMonster(), aantalDagen);
	}

	private void rekenBriefTerug(CervixBrief brief, int aantalDagen)
	{
		rekenObjectTerug(brief, aantalDagen);
		hibernateService.saveOrUpdate(brief);

		CervixMergedBrieven mergedBrieven = brief.getMergedBrieven();
		if (mergedBrieven != null)
		{
			rekenMergedBrievenTerug(mergedBrieven, aantalDagen);
		}

	}

	private void rekenMergedBrievenTerug(CervixMergedBrieven mergedBrieven, int aantalDagen)
	{
		rekenObjectTerug(mergedBrieven, aantalDagen);
		hibernateService.saveOrUpdate(mergedBrieven);
	}

	private void rekenHuisartsberichtTerug(CervixHuisartsBericht huisartsBericht, int aantalDagen)
	{
		rekenObjectTerug(huisartsBericht, aantalDagen);
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
		rekenObjectTerug(hpvBeoordeling, aantalDagen);
		hibernateService.saveOrUpdate(hpvBeoordeling);
		rekenHpvBerichtTerug(hpvBeoordeling.getHpvBericht(), aantalDagen);
	}

	private void rekenHpvBerichtTerug(CervixHpvBericht hpvBericht, int aantalDagen)
	{
		rekenObjectTerug(hpvBericht, aantalDagen);
		hibernateService.saveOrUpdate(hpvBericht);

	}

	private void rekenZasTerug(CervixZas zas, int aantalDagen)
	{
		rekenObjectTerug(zas, aantalDagen);
		hibernateService.saveOrUpdate(zas);
	}

	private void rekenUitstrijkjeTerug(CervixUitstrijkje uitstrijkje, int aantalDagen)
	{
		rekenObjectTerug(uitstrijkje, aantalDagen);
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
		rekenObjectTerug(labformulier, aantalDagen);
		hibernateService.saveOrUpdate(labformulier);
	}

	private void rekenCytologieOrderTerug(CervixCytologieOrder cytologieOrder, int aantalDagen)
	{
		rekenObjectTerug(cytologieOrder, aantalDagen);
		hibernateService.saveOrUpdate(cytologieOrder);
	}

	private void rekenVerslagTerug(CervixVerslag verslag, int aantalDagen)
	{
		rekenCytologieVerslagTerug((CervixCytologieVerslag) verslag, aantalDagen);
		rekenCdaBerichtTerug(verslag.getOntvangenCdaBericht(), aantalDagen);
	}

	private void rekenCdaBerichtTerug(OntvangenCdaBericht cdaBericht, int aantalDagen)
	{
		rekenObjectTerug(cdaBericht, aantalDagen);
		hibernateService.saveOrUpdate(cdaBericht);
	}

	private void rekenCytologieVerslagTerug(CervixCytologieVerslag cytologieVerslag, int aantalDagen)
	{
		rekenObjectTerug(cytologieVerslag, aantalDagen);
		hibernateService.saveOrUpdate(cytologieVerslag);
	}

	private void rekenAfmeldingTerug(CervixAfmelding afmelding, int aantalDagen)
	{
		rekenObjectTerug(afmelding, aantalDagen);
		hibernateService.saveOrUpdate(afmelding);
	}

	private void rekenUitstelTerug(CervixUitstel uitstel, int aantalDagen)
	{
		rekenObjectTerug(uitstel, aantalDagen);
		hibernateService.saveOrUpdate(uitstel);
	}

	private int aantalDagenCalculator(CervixDossier dossier, CervixTestTimeLineDossierTijdstip tijdstip)
	{
		switch (tijdstip)
		{
		case ONTVANGEN:
			return 14;
		case VERVOLGONDERZOEK_BRIEF:
			return 180;
		case NIEUWE_RONDE:
			return 1825;
		default:
			return 1;
		}
	}

	public int overgeblevenDagen(Date date, int aantalDagen)
	{
		DateTime aantdagenReverse = new DateTime().minusDays(aantalDagen);
		Days dagen = Days.daysBetween(aantdagenReverse.toLocalDate(), new DateTime(date).toLocalDate());
		return dagen.getDays() > 0 ? dagen.getDays() : 0;
	}

	private boolean rekenObjectTerug(HibernateObject object, int aantalDagen)
	{
		try
		{
			if (object != null)
			{
				for (Field dateField : getAllDateFieldsFrom(object))
				{
					SimpleDateFormat format = new SimpleDateFormat("dd-MM-yyyy");
					DateConverter dateConverter = new DateConverter();
					dateConverter.setPattern("dd-MM-yyyy");
					ConvertUtils.register(dateConverter, Date.class);

					Date oudeDatum = (Date) PropertyUtils.getProperty(object, dateField.getName());
					if (oudeDatum != null)
					{
						Date nieuweDatum = new DateTime(oudeDatum).minusDays(aantalDagen).toDate();
						BeanUtils.setProperty(object, dateField.getName(), nieuweDatum);
						hibernateService.saveOrUpdate(object);
						LOG.debug("--- " + object.getClass().getName() + "." + dateField.getName() + " van datum " + format.format(oudeDatum) + ", naar datum "
							+ format.format(nieuweDatum) + " ---");
					}
				}
			}
		}
		catch (IllegalAccessException | InvocationTargetException | NoSuchMethodException e)
		{
			LOG.error("Er is een fout opgetreden in de reflection voor het terug zetten van het dossier", e);
			return false;
		}

		return true;
	}

	private List<Field> getAllDateFieldsFrom(Object object)
	{
		Class<?> clazz = HibernateHelper.deproxy(object).getClass();
		List<Field> dateFields = new ArrayList<Field>();
		for (Class<?> c = clazz; c != null; c = c.getSuperclass())
		{
			for (Field field : c.getDeclaredFields())
			{
				if (Date.class == field.getType())
				{
					dateFields.add(field);
					LOG.debug("--- DateField geregistreerd van inherited class: " + c.getName() + ", field: " + field.getName() + " ---");
				}
			}
		}
		return dateFields;
	}
}
