package nl.rivm.screenit.util.cervix.HpvBerichtGenerator;

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

import java.io.IOException;
import java.io.InputStream;

import org.apache.commons.io.IOUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ca.uhn.hl7v2.DefaultHapiContext;
import ca.uhn.hl7v2.HapiContext;
import ca.uhn.hl7v2.model.Message;
import ca.uhn.hl7v2.parser.Parser;

public class CervixHpvBerichtGenerator
{

	private static final Logger LOG = LoggerFactory.getLogger(CervixHpvBerichtGenerator.class);

	private static String FOLDER = "/hpvBerichtGenerator/";

	private static String HEADER_BERICHT = "HL7_hpv_bericht_header.txt";

	private static String UITSLAG_BERICHT = "HL7_hpv_bericht_uitslag.txt";

	private static String UITSLAG = "{UITSLAG}";

	private static String BARCODE = "{BARCODE}";

	private static String ANALYSEDATUM = "{ANALYSEDATUM}";

	private static String AUTORISATIEDATUM = "{AUTORISATIEDATUM}";

	private static String LABNAAM = "{LABNAAM}";

	private static String INSTRUMENTID = "{INSTRUMENTID}";

	private static String MESSAGEID = "{MESSAGEID}";

	private CervixHpvBerichtGenerator()
	{

	}

	public static Message geefHl7Bericht(CervixHpvBerichtGeneratorWrapper wrapper)
	{
		String bericht = geefHL7Bericht(wrapper);

		try
		{
			HapiContext context = new DefaultHapiContext(); 
			Parser p = context.getPipeParser();
			Message hapiMsg = p.parse(bericht);
			return hapiMsg;
		}
		catch (Exception e)
		{
			LOG.error("Er ging iets mis met parsen van het bericht", e);
		}
		return null;
	}

	public static String geefHL7Bericht(CervixHpvBerichtGeneratorWrapper wrapper)
	{
		String uitslagString = getUitslagString();
		String headerString = getHeaderString();

		String uitslagBericht = "";
		for (CervixHpvBerichtGeneratorMonsterWrapper monsterWrapper : wrapper.getMonsterWrappers())
		{
			uitslagBericht += vulUitslag(uitslagString, monsterWrapper);
		}

		String volledigBericht = headerString + uitslagBericht;

		return vulHeaderWaardes(volledigBericht, wrapper);
	}

	private static String vulHeaderWaardes(String volledigBericht, CervixHpvBerichtGeneratorWrapper wrapper)
	{
		volledigBericht = volledigBericht.replace(INSTRUMENTID, wrapper.getInstrumentId());
		volledigBericht = volledigBericht.replace(LABNAAM, wrapper.getLabNaam());
		volledigBericht = volledigBericht.replace(MESSAGEID, wrapper.getMessageId());
		return volledigBericht;
	}

	private static String vulUitslag(String uitslagString, CervixHpvBerichtGeneratorMonsterWrapper monsterWrapper)
	{
		uitslagString = uitslagString.replace(ANALYSEDATUM, monsterWrapper.getStringAnalyseDatum());
		uitslagString = uitslagString.replace(AUTORISATIEDATUM, monsterWrapper.getStringAutorisatieDatum());
		uitslagString = uitslagString.replace(UITSLAG, monsterWrapper.getUitslag().getBerichtWaarde());
		uitslagString = uitslagString.replace(BARCODE, monsterWrapper.getBarcode());
		return uitslagString;
	}

	private static String getHeaderString()
	{
		InputStream header = null;
		try
		{
			header = CervixHpvBerichtGenerator.class.getResourceAsStream(FOLDER + HEADER_BERICHT);
			String headerString = IOUtils.toString(header);
			return headerString;
		}
		catch (Exception e)
		{
			LOG.error("Hl7 bericht kon niet worden opgehaald");
		}
		finally
		{
			try
			{
				if (header != null)
				{
					header.close();
				}
			}
			catch (IOException e)
			{
				LOG.error("De uitslag stream kon niet worden gesloten");
			}
		}
		return null;
	}

	private static String getUitslagString()
	{
		InputStream uitslag = null;
		try
		{
			uitslag = CervixHpvBerichtGenerator.class.getResourceAsStream(FOLDER + UITSLAG_BERICHT);
			String uitslagString = IOUtils.toString(uitslag);
			return uitslagString;
		}
		catch (Exception e)
		{
			LOG.error("Hl7 bericht kon niet worden opgehaald");
		}
		finally
		{
			try
			{
				if (uitslag != null)
				{
					uitslag.close();
				}
			}
			catch (IOException e)
			{
				LOG.error("De uitslag stream kon niet worden gesloten");
			}
		}
		return null;
	}

}
