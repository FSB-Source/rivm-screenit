package nl.rivm.screenit.main.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.persistence.Column;

import nl.rivm.screenit.main.service.IntervalcarcinoomProcessdataVerwerkingService;
import nl.rivm.screenit.main.service.impl.KoppelresultatenKankerregistratieVerwerkingContext.ColonKoppelresultatenKankerregistratieHeaderMapping;
import nl.rivm.screenit.model.colon.ColonKoppelresultaatKankerregistratie;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.colon.ColonUitnodiging;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.commons.beanutils.PropertyUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang.reflect.FieldUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS)
public class KoppelresultatenKankerregistratieVerwerkingServiceImpl implements IntervalcarcinoomProcessdataVerwerkingService
{

	private static final Logger LOG = LoggerFactory.getLogger(KoppelresultatenKankerregistratieVerwerkingServiceImpl.class);

	@Autowired
	private HibernateService hibernateService;

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void verwerkRegel(KoppelresultatenKankerregistratieVerwerkingContext context)
	{

		try
		{
			String bvo = context.getColumnValue(ColonKoppelresultatenKankerregistratieHeaderMapping.BVO);
			if ("DK".equals(StringUtils.trimToEmpty(bvo)) && context.getBevolkingsonderzoek().equals(Bevolkingsonderzoek.COLON))
			{
				String uitnodigingsNummer = context.getColumnValue(ColonKoppelresultatenKankerregistratieHeaderMapping.UITNODIGINGSNUMMER);
				Map<String, Long> parameters = new HashMap<>();
				parameters.put("uitnodigingsId", Long.valueOf(uitnodigingsNummer));
				List<ColonUitnodiging> uitnodigingen = hibernateService.getByParameters(ColonUitnodiging.class, parameters);

				if (uitnodigingen != null && uitnodigingen.size() == 1)
				{
					ColonUitnodiging uitnodiging = uitnodigingen.get(0);
					ColonScreeningRonde screeningRonde = uitnodiging.getScreeningRonde();
					ColonKoppelresultaatKankerregistratie koppelresultaatKankerregistratie = new ColonKoppelresultaatKankerregistratie();
					koppelresultaatKankerregistratie.setScreeningsRonde(screeningRonde);
					koppelresultaatKankerregistratie.setUploadedFile(context.getFile());
					for (ColonKoppelresultatenKankerregistratieHeaderMapping m : ColonKoppelresultatenKankerregistratieHeaderMapping.values())
					{
						String fieldName = m.getFieldName();
						if (StringUtils.isNotBlank(fieldName))
						{
							Field field = FieldUtils.getDeclaredField(ColonKoppelresultaatKankerregistratie.class, fieldName, true);
							PropertyUtils.setProperty(koppelresultaatKankerregistratie, fieldName, convert(context.getColumnValue(m), field));
						}
					}
					screeningRonde.getKoppelresultatenKankerregistratie().add(koppelresultaatKankerregistratie);
					hibernateService.saveOrUpdateAll(koppelresultaatKankerregistratie, screeningRonde);
				}
				else
				{
					throw new IllegalStateException("Geen uitnodiging gevonden voor id " + uitnodigingsNummer);
				}
			}
			else
			{
				throw new IllegalStateException("Upload alleen mogelijk voor DK");
			}
		}
		catch (IllegalStateException | IllegalArgumentException | IllegalAccessException | InvocationTargetException | NoSuchMethodException e)
		{
			context.addMelding("Fout in regel #" + context.getRegelnummer() + ":  " + e.getMessage());
			LOG.warn("Probleem opgetreden met verwerken van project bestand op regelnummer " + context.getRegelnummer(),
				e);
		}
	}

	private Object convert(String columnValue, Field field)
	{
		if (StringUtils.isNotBlank(columnValue))
		{
			if (field.getType().equals(Integer.class))
			{
				return Integer.valueOf(columnValue);
			}
			else if (field.getType().equals(Date.class))
			{
				SimpleDateFormat format = new SimpleDateFormat("dd-MM-yyyy");
				format.setLenient(false);
				if (columnValue.length() != 10)
				{
					throw new IllegalArgumentException("Opgegeven datum heeft niet het juiste formaat '" + format.toPattern() + "': " + columnValue);
				}
				try
				{
					return format.parse(columnValue);
				}
				catch (ParseException e)
				{
					LOG.error("Parser error " + columnValue, e);
					throw new IllegalArgumentException("Opgegeven datum heeft niet het juiste formaat '" + format.toPattern() + "': " + columnValue);
				}
			}
			else if (field.getType().equals(String.class))
			{
				if (columnValue.length() > 255)
				{
					LOG.error("Value is langer dan 255 characters '" + columnValue + "'");
					throw new IllegalArgumentException("Value is langer dan 255 characters '" + columnValue + "'");
				}
			}
		}
		else
		{
			Column annotation = field.getAnnotation(Column.class);
			if (annotation != null)
			{
				if (!annotation.nullable())
				{
					LOG.error("Veld '" + field.getName() + "' is verplicht, maar er is geen waarde opgegeven.");
					throw new IllegalArgumentException("Veld '" + field.getName() + "' is verplicht, maar er is geen waarde opgegeven.");
				}
			}
		}
		return columnValue;
	}

}
