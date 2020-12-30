package nl.rivm.screenit.main.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.math.BigDecimal;
import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.main.service.KwaliteitscontroleLabService;
import nl.rivm.screenit.main.service.QbaseService;
import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.colon.IFOBTBestand;
import nl.rivm.screenit.model.colon.IFOBTUitslag;
import nl.rivm.screenit.model.colon.IFobtLaboratorium;
import nl.rivm.screenit.model.colon.SKMLExterneControleBarcode;
import nl.rivm.screenit.model.colon.SKMLInterneControleBarcode;
import nl.rivm.screenit.model.colon.SKMLSentineelControleBarcode;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.apache.commons.collections.CollectionUtils;
import org.joda.time.DateTime;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class QbaseServiceImpl implements QbaseService
{

	private static final String PERCENTAGE_TUSSEN = "3285";

	private static final String PERCENTAGE_LAAG = "3286";

	private static final String PERCENTAGE_HOOG = "3287";

	private static final String NEWLINE = "\r\n";

	private static final String CLUSTER = "1";

	private static final String DATE_TIME_PATTERN = "dd-MM-yyyy HH:mm";

	private static final String DATE_PATTERN = "dd-MM-yyyy";

	private static final String DECIMAL_PATTERN = "0.00";

	@Autowired
	private ICurrentDateSupplier dateSupplier;

	@Autowired
	private KwaliteitscontroleLabService controleService;

	@Autowired
	private SimplePreferenceService preferenceService;

	@Autowired
	private LogService logService;

	@Override
	public String maakQbaseBestand(List<IFOBTBestand> bestanden, Account ingelogdeAccount)
	{
		StringBuilder qsb = new StringBuilder();
		maakQbaseHeader(qsb, getLabcode(bestanden, ingelogdeAccount));
		maakM1(qsb);
		verwerkBestanden(bestanden, qsb);
		return qsb.toString();
	}

	private String getLabcode(List<IFOBTBestand> bestanden, Account ingelogdeAccount)
	{
		if (CollectionUtils.isNotEmpty(bestanden))
		{
			IFOBTBestand bestand = bestanden.get(0);
			IFobtLaboratorium laboratorium = bestand.getLaboratorium();
			String qbasenummer = laboratorium.getQbasenummer();
			logService.logGebeurtenis(LogGebeurtenis.QBASE_BESTAND_GEMAAKT, ingelogdeAccount, "Labid voor QBASE " + qbasenummer, Bevolkingsonderzoek.COLON);
			return qbasenummer;
		}
		else
		{
			return "GEEN";
		}
	}

	private void verwerkBestanden(List<IFOBTBestand> bestanden, StringBuilder qsb)
	{
		List<IFOBTUitslag> externeUitslagen = new ArrayList<>();
		List<IFOBTUitslag> clientUitslagen = new ArrayList<>();
		Map<String, List<IFOBTUitslag>> interneUitslagenMapping = new HashMap<>();
		Map<String, List<IFOBTUitslag>> sentineelUitslagenMapping = new HashMap<>();

		if (CollectionUtils.isNotEmpty(bestanden))
		{

			for (IFOBTBestand bestand : bestanden)
			{
				for (IFOBTUitslag uitslag : bestand.getUitslagen())
				{
					if (uitslag.getType() != null)
					{
						switch (uitslag.getType())
						{
						case EXTERN:
							externeUitslagen.add(uitslag);
							break;
						case CLIENT:
							clientUitslagen.add(uitslag);
							break;
						case INTERN:
						{
							SKMLInterneControleBarcode barcode = controleService.getInterneControleBarcode(uitslag.getBarcode());
							String header = barcode.getQbaseId() + " SKML " + barcode.getControleTekst();
							mapUitslagOpHeader(interneUitslagenMapping, uitslag, header);
							break;
						}
						case SENTINEEL:
						{
							SKMLSentineelControleBarcode barcode = controleService.getSentineelControleBarcode(uitslag.getBarcode());
							String header = barcode.getQbaseId() + " Sentinel " + barcode.getSentineelType().getOmschrijving();
							mapUitslagOpHeader(sentineelUitslagenMapping, uitslag, header);
							break;
						}
						case CLIENT_EIKEN:

						default:
							break;
						}
					}
				}
			}

			int index = 2;
			List<String> sentineelHeaders = new ArrayList<>(sentineelUitslagenMapping.keySet());
			index = maakMx(qsb, index, sentineelHeaders);

			List<String> interneHeaders = new ArrayList<>(interneUitslagenMapping.keySet());
			index = maakMx(qsb, index, interneHeaders);

			for (IFOBTUitslag uitslag : externeUitslagen)
			{
				SKMLExterneControleBarcode barcode = controleService.getExterneControleBarcode(uitslag.getBarcode());
				maakSE(qsb, uitslag, barcode.getSchema().getLetter());
			}
			Map<Date, List<IFOBTUitslag>> clientUitslagenPerDag = new HashMap<>();
			for (IFOBTUitslag uitslag : clientUitslagen)
			{
				Date analyseDatum = new DateTime(uitslag.getAnalyseDatum()).toLocalDate().toDate();
				if (!clientUitslagenPerDag.containsKey(analyseDatum))
				{
					clientUitslagenPerDag.put(analyseDatum, new ArrayList<IFOBTUitslag>());
				}
				List<IFOBTUitslag> lijst = clientUitslagenPerDag.get(analyseDatum);
				lijst.add(uitslag);
			}
			for (Map.Entry<Date, List<IFOBTUitslag>> clientUitslagDag : clientUitslagenPerDag.entrySet())
			{
				maakSI1(qsb, clientUitslagDag.getKey(), berekenClientAantallen(clientUitslagDag.getValue()));
			}

			index = 2;
			index = maakSIx(qsb, sentineelUitslagenMapping, index, sentineelHeaders);
			index = maakSIx(qsb, interneUitslagenMapping, index, interneHeaders);
		}
	}

	private static void mapUitslagOpHeader(Map<String, List<IFOBTUitslag>> uitslagenMapping, IFOBTUitslag uitslag, String header)
	{
		List<IFOBTUitslag> uitslagen = uitslagenMapping.get(header);
		if (uitslagen == null)
		{
			uitslagen = new ArrayList<>();
			uitslagenMapping.put(header, uitslagen);
		}
		uitslagen.add(uitslag);
	}

	private Map<String, BigDecimal> berekenClientAantallen(List<IFOBTUitslag> uitslagen)
	{
		Map<String, BigDecimal> result = new HashMap<String, BigDecimal>();

		int aantalHoogPerc = 0;
		int aantalLaagPerc = 0;
		int aantalTussenPerc = 0;
		if (CollectionUtils.isNotEmpty(uitslagen))
		{
			int totaal = uitslagen.size();
			int aantalHoog = 0;
			int aantalLaag = 0;

			Integer detectiegrensI = preferenceService.getInteger(PreferenceKey.IFOBT_DETECTIEGRENS.name());
			if (detectiegrensI == null)
			{
				detectiegrensI = Integer.valueOf(22);
			}
			BigDecimal detectiegrens = BigDecimal.valueOf(detectiegrensI / 100);
			BigDecimal normwaarde = BigDecimal.valueOf(preferenceService.getInteger(PreferenceKey.IFOBT_NORM_WAARDE.name()) / 100);

			for (IFOBTUitslag uitslag : uitslagen)
			{
				BigDecimal uitslagWaarde = uitslag.getUitslag();
				if (uitslagWaarde.compareTo(normwaarde) > 0)
				{
					aantalHoog++;
				}
				else if (uitslagWaarde.compareTo(detectiegrens) < 0)
				{
					aantalLaag++;
				}
			}

			if (aantalHoog > 0)
			{
				aantalHoogPerc = aantalHoog * 10000 / totaal;
			}
			if (aantalLaag > 0)
			{
				aantalLaagPerc = aantalLaag * 10000 / totaal;
			}

			aantalTussenPerc = 10000 - aantalHoogPerc - aantalLaagPerc;
		}

		result.put(PERCENTAGE_HOOG, BigDecimal.valueOf(aantalHoogPerc / 100.0));
		result.put(PERCENTAGE_LAAG, BigDecimal.valueOf(aantalLaagPerc / 100.0));
		result.put(PERCENTAGE_TUSSEN, BigDecimal.valueOf(aantalTussenPerc / 100.0));

		return result;
	}

	private void maakQbaseHeader(StringBuilder qsb, String labcode)
	{
		SimpleDateFormat formatter = new SimpleDateFormat("dd-MM-yyyy");
		qsb.append("SKML CCSCR ");
		qsb.append(labcode);
		qsb.append(NEWLINE);
		qsb.append("K Gegenereerd door ScreenIT op ");
		qsb.append(formatter.format(dateSupplier.getDate()));
		qsb.append(NEWLINE);
	}

	private static void maakM1(StringBuilder qsb)
	{
		qsb.append("M 1 14510 Clientgegevens");
		qsb.append(NEWLINE);
	}

	private static int maakMx(StringBuilder qsb, int index, List<String> headers)
	{
		Collections.sort(headers);
		for (String header : headers)
		{
			qsb.append("M ").append(index++).append(" ").append(header);
			qsb.append(NEWLINE);
		}
		return index;
	}

	private static void maakSE(StringBuilder qsb, IFOBTUitslag uitslag, String letter)
	{
		qsb.append("SE ");
		qsb.append(letter);
		qsb.append(" ");
		qsb.append(CLUSTER);
		qsb.append(" ");
		qsb.append(new SimpleDateFormat(DATE_TIME_PATTERN).format(uitslag.getAnalyseDatum()));
		qsb.append(NEWLINE);
		qsb.append("R 1455 ");
		qsb.append(new DecimalFormat(DECIMAL_PATTERN).format(uitslag.getUitslag()));
		qsb.append(NEWLINE);
	}

	private static void maakSI1(StringBuilder qsb, Date datum, Map<String, BigDecimal> percentages)
	{
		qsb.append("SI 1 ");
		qsb.append(CLUSTER);
		qsb.append(" ");
		qsb.append(new SimpleDateFormat(DATE_PATTERN).format(datum));
		qsb.append(NEWLINE);
		qsb.append("R ").append(PERCENTAGE_HOOG).append(" ");
		qsb.append(new DecimalFormat(DECIMAL_PATTERN).format(percentages.get(PERCENTAGE_HOOG)));
		qsb.append(NEWLINE);
		qsb.append("R ").append(PERCENTAGE_LAAG).append(" ");
		qsb.append(new DecimalFormat(DECIMAL_PATTERN).format(percentages.get(PERCENTAGE_LAAG)));
		qsb.append(NEWLINE);
		qsb.append("R ").append(PERCENTAGE_TUSSEN).append(" ");
		qsb.append(new DecimalFormat(DECIMAL_PATTERN).format(percentages.get(PERCENTAGE_TUSSEN)));
		qsb.append(NEWLINE);
	}

	private static int maakSIx(StringBuilder qsb, Map<String, List<IFOBTUitslag>> uitslagenMapping, int index, List<String> headers)
	{
		for (String header : headers)
		{
			List<IFOBTUitslag> uitslagen = uitslagenMapping.get(header);
			String nummer = "" + index++;
			for (IFOBTUitslag uitslag : uitslagen)
			{
				qsb.append("SI ");
				qsb.append(nummer);
				qsb.append(" ");
				qsb.append(CLUSTER);
				qsb.append(" ");
				qsb.append(new SimpleDateFormat(DATE_TIME_PATTERN).format(uitslag.getAnalyseDatum()));
				qsb.append(NEWLINE);
				qsb.append("R 1455 ");
				qsb.append(new DecimalFormat(DECIMAL_PATTERN).format(uitslag.getUitslag()));
				qsb.append(NEWLINE);
			}
		}
		return index;
	}
}
