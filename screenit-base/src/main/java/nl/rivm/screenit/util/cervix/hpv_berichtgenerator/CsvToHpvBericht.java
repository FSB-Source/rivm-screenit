package nl.rivm.screenit.util.cervix.hpv_berichtgenerator;

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

import java.io.File;
import java.io.FileReader;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.List;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.model.cervix.berichten.CervixHpvOrderCode;
import nl.rivm.screenit.model.cervix.berichten.CervixHpvResultCode;
import nl.rivm.screenit.model.cervix.berichten.CervixHpvResultValue;
import nl.rivm.screenit.model.cervix.enums.CervixHpvResultaatBerichtBron;

import au.com.bytecode.opencsv.CSVReader;
import ca.uhn.hl7v2.model.Message;

@Slf4j
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class CsvToHpvBericht
{

	public static List<Message> csvToHpvBerichten(File file)
	{
		return csvToHpvBerichten(file, ',');
	}

	private static List<Message> csvToHpvBerichten(File file, Character seperator)
	{

		String huidigeMessageId = null;
		CervixHpvBerichtGeneratorWrapper wrapper = null;
		List<CervixHpvBerichtGeneratorWrapper> wrappers = new ArrayList<>();
		List<Message> hpvBerichten = new ArrayList<>();
		try (CSVReader csvReader = new CSVReader(new FileReader(file), seperator))
		{
			for (String[] line : csvReader.readAll())
			{
				if ("messageId".equals(line[0]))
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
					wrapper.setResultaatBerichtBron(CervixHpvResultaatBerichtBron.valueOf(line[3]));
					huidigeMessageId = messageId;
					wrappers.add(wrapper);
				}

				CervixHpvBerichtGeneratorMonsterWrapper monsterWrapper = maakMonsterWrapper(line);
				wrapper.getMonsterWrappers().add(monsterWrapper);
			}
			for (CervixHpvBerichtGeneratorWrapper hpvWrapper : wrappers)
			{
				hpvBerichten.add(CervixHpvBerichtGenerator.geefHL7Bericht(hpvWrapper));
			}
		}
		catch (ArrayIndexOutOfBoundsException e)
		{
			if (';' != seperator)
			{

				hpvBerichten = csvToHpvBerichten(file, ';');
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

	private static CervixHpvBerichtGeneratorMonsterWrapper maakMonsterWrapper(String[] line) throws ParseException
	{
		SimpleDateFormat formatter = new SimpleDateFormat("dd-MM-yyyy");

		CervixHpvBerichtGeneratorMonsterWrapper monsterWrapper = new CervixHpvBerichtGeneratorMonsterWrapper();
		monsterWrapper.setBarcode(line[4]);
		monsterWrapper.setOrdercode(CervixHpvOrderCode.fromBerichtWaarde(line[5]));
		monsterWrapper.setAnalysecode1(CervixHpvResultCode.fromBerichtWaarde(line[6]));
		monsterWrapper.setAnalyseresultaat1(CervixHpvResultValue.fromValue(line[7]));
		monsterWrapper.setAnalysecode2(CervixHpvResultCode.fromBerichtWaarde(line[8]));
		monsterWrapper.setAnalyseresultaat2(CervixHpvResultValue.fromValue(line[9]));
		monsterWrapper.setAnalysecode3(CervixHpvResultCode.fromBerichtWaarde(line[10]));
		monsterWrapper.setAnalyseresultaat3(CervixHpvResultValue.fromValue(line[11]));
		monsterWrapper.setAnalyseDatum(formatter.parse(line[12]));
		monsterWrapper.setAutorisatieDatum(formatter.parse(line[13]));
		return monsterWrapper;
	}
}
