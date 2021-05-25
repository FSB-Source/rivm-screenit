package nl.rivm.screenit.service.mamma.impl;

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
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.mamma.MammaAfmelding;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaBrief;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaMergedBrieven;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.MammaUitnodiging;
import nl.rivm.screenit.model.mamma.MammaUitstel;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.mamma.MammaBaseTestTimelineTimeService;
import nl.rivm.screenit.service.mamma.enums.MammaTestTimeLineDossierTijdstip;
import nl.rivm.screenit.util.DateUtil;
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

import com.google.common.primitives.Ints;

@Service
@Transactional(propagation = Propagation.REQUIRED)
public class MammaBaseTestTimelineTimeServiceImpl implements MammaBaseTestTimelineTimeService
{
	private static final Logger LOG = LoggerFactory.getLogger(MammaBaseTestTimelineTimeServiceImpl.class);

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private ICurrentDateSupplier dateSupplier;

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
		LOG.debug("Dossier aantal dagen terug gezet: " + aantalDagen);
		rekenObjectTerug(dossier, aantalDagen);
		hibernateService.saveOrUpdate(dossier);

		for (MammaScreeningRonde ronde : dossier.getScreeningRondes())
		{
			rekenRondeTerug(ronde, aantalDagen);
		}
		for (MammaAfmelding afmelding : dossier.getAfmeldingen())
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

	private void rekenRondeTerug(MammaScreeningRonde ronde, int aantalDagen)
	{
		rekenObjectTerug(ronde, aantalDagen);
		hibernateService.saveOrUpdate(ronde);

		ronde.getFollowUpRadiologieVerslagen().forEach(mammaFollowUpRadiologieVerslag -> rekenObjectTerug(mammaFollowUpRadiologieVerslag, aantalDagen));

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

		for (MammaAfmelding afmelding : ronde.getAfmeldingen())
		{
			rekenAfmeldingTerug(afmelding, aantalDagen);
		}
	}

	private void rekenUitstelTerug(MammaUitstel uitstel, int aantalDagen)
	{
		rekenObjectTerug(uitstel, aantalDagen);
		hibernateService.saveOrUpdate(uitstel);
	}

	private void rekenUitnodigingTerug(MammaUitnodiging uitnodiging, int aantalDagen)
	{
		rekenObjectTerug(uitnodiging, aantalDagen);
		hibernateService.saveOrUpdate(uitnodiging);

		for (MammaAfspraak afspraak : uitnodiging.getAfspraken())
		{
			rekenObjectTerug(afspraak, aantalDagen);

			MammaOnderzoek onderzoek = afspraak.getOnderzoek();
			if (onderzoek != null)
			{
				rekenObjectTerug(onderzoek, aantalDagen);
				rekenObjectTerug(onderzoek.getMammografie(), aantalDagen);
				for (MammaBeoordeling beoordeling : onderzoek.getBeoordelingen())
				{
					rekenObjectTerug(beoordeling, aantalDagen);
					rekenObjectTerug(beoordeling.getDiscrepantieLezing(), aantalDagen);
					rekenObjectTerug(beoordeling.getArbitrageLezing(), aantalDagen);
					rekenObjectTerug(beoordeling.getTweedeLezing(), aantalDagen);
					rekenObjectTerug(beoordeling.getEersteLezing(), aantalDagen);
					hibernateService.saveOrUpdate(onderzoek);
				}
			}

		}

	}

	private void rekenBriefTerug(MammaBrief brief, int aantalDagen)
	{
		rekenObjectTerug(brief, aantalDagen);
		hibernateService.saveOrUpdate(brief);

		MammaMergedBrieven mergedBrieven = brief.getMergedBrieven();
		if (mergedBrieven != null)
		{
			rekenMergedBrievenTerug(mergedBrieven, aantalDagen);
		}

	}

	private void rekenMergedBrievenTerug(MammaMergedBrieven mergedBrieven, int aantalDagen)
	{
		rekenObjectTerug(mergedBrieven, aantalDagen);
		hibernateService.saveOrUpdate(mergedBrieven);
	}

	private void rekenAfmeldingTerug(MammaAfmelding afmelding, int aantalDagen)
	{
		rekenObjectTerug(afmelding, aantalDagen);
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
