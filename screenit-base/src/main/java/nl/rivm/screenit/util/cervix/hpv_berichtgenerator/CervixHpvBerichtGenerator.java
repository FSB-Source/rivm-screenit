package nl.rivm.screenit.util.cervix.hpv_berichtgenerator;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.nio.charset.Charset;
import java.util.concurrent.Executors;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.model.cervix.berichten.CervixHpvResultCode;
import nl.rivm.screenit.model.cervix.berichten.CervixHpvResultValue;

import org.apache.commons.io.IOUtils;

import ca.uhn.hl7v2.DefaultHapiContext;
import ca.uhn.hl7v2.HL7Exception;
import ca.uhn.hl7v2.HapiContext;
import ca.uhn.hl7v2.model.Message;
import ca.uhn.hl7v2.parser.Parser;

@Slf4j
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class CervixHpvBerichtGenerator
{
	private static final String INSTRUMENTID = "{INSTRUMENTID}";

	private static final String LABNAAM = "{LABNAAM}";

	private static final String MESSAGEID = "{MESSAGEID}";

	private static final String BARCODE = "{BARCODE}";

	private static final String ANALYSETYPE = "{ANALYSETYPE}";

	private static final String ANALYSEDATUM = "{ANALYSEDATUM}";

	private static final String AUTORISATIEDATUM = "{AUTORISATIEDATUM}";

	private static final String ANALYSE_ID = "{ANALYSE_ID}";

	private static final String RESULT_CODE = "{RESULT_CODE}";

	private static final String RESULT_VALUE = "{RESULT_VALUE}";

	public static Message geefHL7Bericht(CervixHpvBerichtGeneratorWrapper wrapper)
	{
		String bericht = geefHL7BerichtTekst(wrapper);

		return createMessage(bericht);
	}

	public static Message geefHL7Bericht(String file)
	{
		String bericht = getFileContents(file);
		if (!bericht.contains("\r\n"))
		{
			if (bericht.contains("\n"))
			{
				bericht = bericht.replaceAll("\\n", "\r\n");
			}
			else
			{
				bericht = bericht.replaceAll("\\r", "\r\n");
			}
		}
		return createMessage(bericht);
	}

	public static String getFileContents(String relatiefResourcePad)
	{
		InputStream inputStream = CervixHpvBerichtGenerator.class.getResourceAsStream(relatiefResourcePad);

		String outputText;
		try
		{
			outputText = IOUtils.toString(inputStream, Charset.defaultCharset());
		}
		catch (IOException e)
		{
			throw new IllegalStateException("Fout bij inlezen bestand: " + e.getMessage(), null);
		}

		return outputText;
	}

	private static Message createMessage(String bericht)
	{
		try (HapiContext context = new DefaultHapiContext())
		{
			context.setExecutorService(Executors.newSingleThreadExecutor()); 
			Parser parser = context.getPipeParser();
			return parser.parse(bericht);
		}
		catch (HL7Exception | IOException e)
		{
			throw new IllegalStateException("Er ging iets mis met parsen van het bericht", e);
		}
	}

	public static String geefHL7BerichtTekst(CervixHpvBerichtGeneratorWrapper wrapper)
	{
		String monsterTemplate = getMonsterTemplate();
		String analyseTemplate = getAnalyseTemplate();

		StringBuilder berichtTekst = new StringBuilder(getHeaderTemplate());
		for (CervixHpvBerichtGeneratorMonsterWrapper monsterWrapper : wrapper.getMonsterWrappers())
		{
			berichtTekst.append(monsterSnippet(monsterWrapper, monsterTemplate, analyseTemplate));
		}

		var result = vulAlgemeneWaardes(berichtTekst.toString(), wrapper);
		LOG.debug("geefHL7BerichtTekst resultaat: '{}'", result.replace("\n", "\\n").replace("\r", "\\r"));
		return result;
	}

	private static String monsterSnippet(CervixHpvBerichtGeneratorMonsterWrapper monsterWrapper, String monsterTemplate, String analyseTemplate)
	{
		var monsterSnippet = monsterTemplate
			+ analyseSnippet(1, monsterWrapper.getAnalysecode1(), monsterWrapper.getAnalyseresultaat1(), analyseTemplate)
			+ analyseSnippet(2, monsterWrapper.getAnalysecode2(), monsterWrapper.getAnalyseresultaat2(), analyseTemplate)
			+ analyseSnippet(3, monsterWrapper.getAnalysecode3(), monsterWrapper.getAnalyseresultaat3(), analyseTemplate);

		return monsterSnippet
			.replace(BARCODE, monsterWrapper.getBarcode())
			.replace(ANALYSEDATUM, monsterWrapper.getStringAnalyseDatum())
			.replace(AUTORISATIEDATUM, monsterWrapper.getStringAutorisatieDatum())
			.replace(ANALYSETYPE, monsterWrapper.getOrdercode() != null ? monsterWrapper.getOrdercode().getBerichtWaarde() : "");
	}

	private static String analyseSnippet(int analyseId, CervixHpvResultCode analysecode, CervixHpvResultValue analyseresultaat, String analyseTemplate)
	{
		if (analysecode != null || analyseresultaat != null)
		{
			return analyseTemplate
				.replace(ANALYSE_ID, String.valueOf(analyseId + 1))
				.replace(RESULT_CODE, analysecode != null ? analysecode.getBerichtWaarde() : "")
				.replace(RESULT_VALUE, analyseresultaat != null ? analyseresultaat.getBerichtWaarde() : "");
		}
		return "";
	}

	private static String vulAlgemeneWaardes(String volledigBericht, CervixHpvBerichtGeneratorWrapper wrapper)
	{
		return volledigBericht.replace(INSTRUMENTID, wrapper.getInstrumentId())
			.replace(LABNAAM, wrapper.getLabNaam())
			.replace(MESSAGEID, wrapper.getMessageId())
			.replace("\n", "\r\n"); 
	}

	private static String getHeaderTemplate()
	{
		return "MSH|^~\\&|cobas 4800 software 2.2.0.1509^{INSTRUMENTID}^M|{LABNAAM}|LIS|LIS Facility|20151217151241+0100||OUL^R22^OUL_R22|{MESSAGEID}|P|2.5.1|||ER|AL||"
			+ "UNICODE UTF-8|||LAB-29^IHE\n";
	}

	private static String getMonsterTemplate()
	{
		return "SPM|1|{BARCODE}&ROCHE||PCYT^^99ROC|||||||P^^HL70369\n"
			+ "SAC|||{BARCODE}\n"
			+ "OBR||\"\"||{ANALYSETYPE}^{ANALYSETYPE}^99ROC||20151217122946\n"
			+ "ORC|SC||||CM\n"
			+ "OBX|1|DR|RunTimeRange^Run Execution Time Range^99ROC^S_OTHER^Other_Supplemental^IHELAW|1.0|{ANALYSEDATUM}^{AUTORISATIEDATUM}|||\"\"|||F|||||FSE||C4800^Roche~{INSTRUMENTID}^Roche|{AUTORISATIEDATUM}\n";
	}

	private static String getAnalyseTemplate()
	{
		return "OBX|{ANALYSE_ID}|ST|{RESULT_CODE}^{RESULT_CODE}^99ROC|1.1|{RESULT_VALUE}|||Full^^99ROC|||F|||||FSE||C4800^Roche~{INSTRUMENTID}^Roche|{AUTORISATIEDATUM}\n"
			+ "INV|\"\"|OK^^HL70383|OT^^HL70384|MwpId^^99ROC|AD1005007^^99ROC|C01^^99ROC\n"
			+ "INV|\"\"|OK^^HL70383|OT^^HL70384|DwpId^^99ROC|AA1005007^^99ROC\n"
			+ "NTE|1||F;M7\n"
			+ "NTE|2||\n"
			+ "NTE|3||\n";
	}
}
