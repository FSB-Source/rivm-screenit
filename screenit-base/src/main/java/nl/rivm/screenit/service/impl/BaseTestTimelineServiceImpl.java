package nl.rivm.screenit.service.impl;

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

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.text.SimpleDateFormat;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import lombok.Setter;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.service.BaseTestTimelineService;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;
import nl.topicuszorg.hibernate.object.model.HibernateObject;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.commons.beanutils.BeanUtils;
import org.apache.commons.beanutils.ConvertUtils;
import org.apache.commons.beanutils.PropertyUtils;
import org.apache.commons.beanutils.converters.DateConverter;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.REQUIRED)
@Slf4j
public class BaseTestTimelineServiceImpl implements BaseTestTimelineService
{

	@Autowired
	private HibernateService hibernateService;

	@Setter
	private boolean terugrekenenEnabled = true;

	@Override
	public void rekenAllePersoonsDatumTerug(GbaPersoon persoon, int aantalDagen)
	{
		if (!terugrekenenEnabled)
		{
			return;
		}
		if (persoon.getOverlijdensdatum() != null)
		{
			persoon.setOverlijdensdatum(DateUtil.minDagen(persoon.getOverlijdensdatum(), aantalDagen));
		}
		if (persoon.getDatumVertrokkenUitNederland() != null)
		{
			persoon.setDatumVertrokkenUitNederland(DateUtil.minDagen(persoon.getDatumVertrokkenUitNederland(), aantalDagen));
		}
		if (persoon.getDatumVestigingNederland() != null)
		{
			persoon.setDatumVestigingNederland(DateUtil.minDagen(persoon.getDatumVestigingNederland(), aantalDagen));
		}
		hibernateService.saveOrUpdate(persoon);
	}

	@Override
	public boolean rekenObjectTerug(HibernateObject object, int aantalDagen)
	{
		if (!terugrekenenEnabled)
		{
			return true;
		}
		try
		{
			if (object != null)
			{
				for (Field dateField : getAllDateFieldsFrom(object))
				{
					SimpleDateFormat format = new SimpleDateFormat(Constants.DEFAULT_DATE_FORMAT);
					Object oudeDatum = PropertyUtils.getProperty(object, dateField.getName());
					if (dateField.getType() == Date.class)
					{
						DateConverter dateConverter = new DateConverter();
						dateConverter.setPattern(Constants.DEFAULT_DATE_FORMAT);
						ConvertUtils.register(dateConverter, Date.class);

						if (oudeDatum != null)
						{
							Date nieuweDatum = DateUtil.minDagen((Date) oudeDatum, aantalDagen);
							BeanUtils.setProperty(object, dateField.getName(), nieuweDatum);
							hibernateService.saveOrUpdate(object);
							LOG.debug("--- " + object.getClass().getName() + "." + dateField.getName() + " van datum " + format.format((Date) oudeDatum) + ", naar datum "
								+ format.format(nieuweDatum) + " ---");
						}
					}
					else if (dateField.getType() == LocalDate.class)
					{
						if (oudeDatum != null)
						{
							LocalDate nieuweDatum = ((LocalDate) oudeDatum).minusDays(aantalDagen);
							BeanUtils.setProperty(object, dateField.getName(), nieuweDatum);
							hibernateService.saveOrUpdate(object);
							LOG.debug(
								"--- " + object.getClass().getName() + "." + dateField.getName() + " van datum " + format.format(DateUtil.toUtilDate((LocalDate) oudeDatum))
									+ ", naar datum " + format.format(DateUtil.toUtilDate(nieuweDatum)) + " ---");
						}
					}
					else if (dateField.getType() == LocalDateTime.class)
					{
						if (oudeDatum != null)
						{
							LocalDateTime nieuweDatum = ((LocalDateTime) oudeDatum).minusDays(aantalDagen);
							BeanUtils.setProperty(object, dateField.getName(), nieuweDatum);
							hibernateService.saveOrUpdate(object);
							LOG.debug(
								"--- " + object.getClass().getName() + "." + dateField.getName() + " van datum " + format.format(DateUtil.toUtilDate((LocalDateTime) oudeDatum))
									+ ", naar datum " + format.format(DateUtil.toUtilDate(nieuweDatum)) + " ---");
						}
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
				if (Date.class == field.getType() || LocalDate.class == field.getType() || LocalDateTime.class == field.getType())
				{
					dateFields.add(field);
					LOG.debug("--- DateField geregistreerd van inherited class: " + c.getName() + ", field: " + field.getName() + " ---");
				}
			}
		}
		return dateFields;
	}

}
