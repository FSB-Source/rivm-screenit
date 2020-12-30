package nl.rivm.screenit.util.cervix.CsvToHpvBericht;

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

import java.io.File;
import java.io.FileReader;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.List;

import nl.rivm.screenit.util.cervix.HpvBerichtGenerator.CervixHpvBerichtGenerator;
import nl.rivm.screenit.util.cervix.HpvBerichtGenerator.CervixHpvBerichtGeneratorMonsterWrapper;
import nl.rivm.screenit.util.cervix.HpvBerichtGenerator.CervixHpvBerichtGeneratorWrapper;
import nl.rivm.screenit.util.cervix.HpvBerichtGenerator.CervixHpvBerichtWaarde;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import au.com.bytecode.opencsv.CSVReader;
import ca.uhn.hl7v2.model.Message;

public class CsvToHpvBericht
{

	private static final Logger LOG = LoggerFactory.getLogger(CsvToHpvBericht.class);

	private static SimpleDateFormat formatter = new SimpleDateFormat("dd-MM-yyyy");

	private CsvToHpvBericht()
	{

	}

	public static List<Message> csvToHpvBerichten(File file)
	{
		return csvToHpvBerichten(file, new Character(','));
	}

	private static List<Message> csvToHpvBerichten(File file, Character seperator)
	{
		String huidigeMessageId = null;
		CervixHpvBerichtGeneratorWrapper wrapper = null;
		List<CervixHpvBerichtGeneratorWrapper> wrappers = new ArrayList<>();
		List<Message> hpvBerichten = new ArrayList<>();
		try (CSVReader csvReader = new CSVReader(new FileReader(file), seperator.charValue()))
		{
			for (String[] line : csvReader.readAll())
			{
				if (CervixHpvBerichtWaarde.fromValue(line[4]) == null)
				{
					continue;
				}
				String messageId = line[0];
				if (wrapper == null || !messageId.equals(huidigeMessageId))
				{
					wrapper = new CervixHpvBerichtGeneratorWrapper();
					wrapper.setMessageId(messageId);
					wrapper.setLabNaam(line[1]);
					wrapper.setInstrumentId(line[2]);
					huidigeMessageId = messageId;
					wrappers.add(wrapper);
				}

				CervixHpvBerichtGeneratorMonsterWrapper monsterWrapper = new CervixHpvBerichtGeneratorMonsterWrapper();
				monsterWrapper.setBarcode(line[3]);
				monsterWrapper.setUitslag(CervixHpvBerichtWaarde.fromValue(line[4]));
				monsterWrapper.setAnalyseDatum(formatter.parse(line[5]));
				monsterWrapper.setAutorisatieDatum(formatter.parse(line[6]));
				monsterWrapper.setWrapper(wrapper);
				wrapper.getMonsterWrappers().add(monsterWrapper);
			}
			for (CervixHpvBerichtGeneratorWrapper hpvWrapper : wrappers)
			{
				hpvBerichten.add(CervixHpvBerichtGenerator.geefHl7Bericht(hpvWrapper));
			}
		}
		catch (ArrayIndexOutOfBoundsException e)
		{
			if (';' != seperator.charValue())
			{

				hpvBerichten = csvToHpvBerichten(file, new Character(';'));
			}
			else
			{
				LOG.error("Array out of bound exception afgevangen", e);
			}
		}
		catch (Exception e)
		{
			LOG.error("Er is iets misgegaan met het genereren van HPV berichten door middel van een Bestand", e);
		}
		return hpvBerichten;
	}
}
