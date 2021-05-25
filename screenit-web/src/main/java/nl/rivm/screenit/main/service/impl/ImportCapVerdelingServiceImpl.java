
package nl.rivm.screenit.main.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.io.File;
import java.io.FileInputStream;
import java.io.PushbackInputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import nl.rivm.screenit.main.service.ImportCapVerdelingService;
import nl.rivm.screenit.model.Gemeente;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.UitnodigingsGebied;
import nl.rivm.screenit.model.colon.ColoscopieCentrum;
import nl.rivm.screenit.model.colon.ColoscopieCentrumColonCapaciteitVerdeling;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Level;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.rivm.screenit.service.LogService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.commons.lang.exception.ExceptionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.poi.hssf.usermodel.HSSFRow;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.ss.usermodel.WorkbookFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional
public class ImportCapVerdelingServiceImpl implements ImportCapVerdelingService
{

	private static final Logger LOG = LoggerFactory.getLogger(ImportCapVerdelingServiceImpl.class);

	private static final int START_IL_ID_ROW = 1;

	private static final int START_IL_ID_COL = 8;

	private static final int GEMEENTECODE_COL = 2;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private LogService logService;

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public Level verwerkBestand(InstellingGebruiker loggedInInstellingGebruiker, File file)
	{
		Workbook workbook;
		String melding = "";
		Level level = Level.INFO;
		try (PushbackInputStream pushbackInputStream = new PushbackInputStream(new FileInputStream(file));)
		{
			workbook = WorkbookFactory.create(pushbackInputStream);
			workbook.setMissingCellPolicy(HSSFRow.CREATE_NULL_AS_BLANK);

			Sheet sheet = workbook.getSheetAt(0);
			if (sheet == null)
			{
				throw new IllegalStateException("Geen sheet in het excel gevonden");
			}

			LOG.info("CapVerdeling stap 1: Nieuwe capaciteit uit excel halen");
			int rowIdx = START_IL_ID_ROW;
			Map<Long, List<String>> capVerdeling = new HashMap<>();
			Map<Integer, Long> colToId = new HashMap<>();
			while (sheet.getLastRowNum() >= rowIdx)
			{
				Row row = sheet.getRow(rowIdx);
				if (rowIdx == START_IL_ID_ROW)
				{
					int colIdx = START_IL_ID_COL;
					while (row.getLastCellNum() >= colIdx)
					{
						long ilId = getNummericCellValue(row, colIdx).longValue();
						capVerdeling.put(ilId, new ArrayList<String>());
						colToId.put(colIdx, ilId);
						colIdx += 4;
					}
					rowIdx += 2;
				}
				else
				{
					String gemeenteCode = getStringCellValue(row, GEMEENTECODE_COL);
					if (StringUtils.isNotBlank(gemeenteCode))
					{
						int colIdx = START_IL_ID_COL;
						while (row.getLastCellNum() >= colIdx && colToId.containsKey(colIdx))
						{
							String adherentie = "";
							double numericCellValue = getNummericCellValue(row, colIdx - 1);
							if (numericCellValue > 0.0)
							{
								numericCellValue *= 10000.0;
								adherentie = String.valueOf(Double.valueOf(numericCellValue).intValue());
							}
							String capaciteit = "";
							numericCellValue = getNummericCellValue(row, colIdx);
							if (numericCellValue > 0.0)
							{
								numericCellValue *= 10000.0;
								capaciteit = String.valueOf(Double.valueOf(numericCellValue).intValue());
							}

							if (StringUtils.isNotBlank(adherentie) && StringUtils.isNotBlank(capaciteit))
							{
								capVerdeling.get(colToId.get(colIdx)).add(gemeenteCode + "," + adherentie + "," + capaciteit);
							}
							colIdx += 4;
						}
					}
					rowIdx++;
				}
			}
			for (Entry<Long, List<String>> ilEntry : capVerdeling.entrySet())
			{
				Long ilId = ilEntry.getKey();
				List<String> capaciteitsVerdeling = ilEntry.getValue();
				ColoscopieCentrum intakelocatie = hibernateService.get(ColoscopieCentrum.class, ilId);
				if (intakelocatie != null)
				{
					LOG.info("CapVerdeling stap 2: Nieuwe capaciteit toepassen bij " + intakelocatie.getNaam());
					LOG.info("CapVerdeling stap 2.1: Hele capaciteitsverdeling van IL op 0% zetten");
					for (ColoscopieCentrumColonCapaciteitVerdeling verdeling : intakelocatie.getCapaciteitVerdeling())
					{
						verdeling.setPercentageCapaciteit(0);
					}
					Integer totCapPerIntakelocatie = 0;
					LOG.info("CapVerdeling stap 2.2: Per gemeente/uitnodigingsgebied (nieuwe) verdeling maken");
					String subMelding = "";
					for (String capaciteitPerGemeente : capaciteitsVerdeling)
					{
						String[] splitted = capaciteitPerGemeente.split(",");
						String gemeenteCode = StringUtils.leftPad(splitted[0], 4, '0');
						Integer adherentie = Integer.valueOf(splitted[1]);
						Integer capaciteit = Integer.valueOf(splitted[2]);
						if (subMelding.length() > 0)
						{
							subMelding += " + ";
						}
						subMelding += gemeenteCode + " -> " + capaciteit / 100.0 + "%";
						totCapPerIntakelocatie += capaciteit;
						boolean foundGemeente = false;
						LOG.info("CapVerdeling stap 2.2.1: bestaande verdeling updaten bij uitnodigingsgebied/gemeente " + gemeenteCode + " naar a" + adherentie / 100.0 + "% c"
							+ capaciteit / 100.0 + "%");
						for (ColoscopieCentrumColonCapaciteitVerdeling verdeling : intakelocatie.getCapaciteitVerdeling())
						{
							UitnodigingsGebied uitnodigingsGebied = verdeling.getUitnodigingsGebied();
							if (uitnodigingsGebied.getGemeente().getCode().equals(gemeenteCode))
							{
								if (StringUtils.isBlank(uitnodigingsGebied.getGemeenteDeel()) && uitnodigingsGebied.getPostcodeGebied() == null
									&& StringUtils.isBlank(uitnodigingsGebied.getWoonplaats()))
								{
									verdeling.setPercentageCapaciteit(capaciteit);
									verdeling.setPercentageAdherentie(adherentie);
									hibernateService.saveOrUpdate(uitnodigingsGebied);
									foundGemeente = true;
								}
							}
							else if (uitnodigingsGebied.getNaam().equals(gemeenteCode))
							{
								verdeling.setPercentageCapaciteit(capaciteit);
								verdeling.setPercentageAdherentie(adherentie);
								hibernateService.saveOrUpdate(uitnodigingsGebied);
								foundGemeente = true;
							}
						}
						if (!foundGemeente)
						{
							LOG.info("CapVerdeling stap 2.2.2: nieuwe koppeling naar uitnodigingsgebied/gemeente " + gemeenteCode);
							ColoscopieCentrumColonCapaciteitVerdeling nieuweVerdeling = new ColoscopieCentrumColonCapaciteitVerdeling();
							nieuweVerdeling.setPercentageCapaciteit(capaciteit);
							nieuweVerdeling.setPercentageAdherentie(adherentie);

							Map<String, String> param = new HashMap<>();
							param.put("code", gemeenteCode);
							Gemeente gemeente = hibernateService.getUniqueByParameters(Gemeente.class, param);
							if (gemeente != null)
							{
								for (UitnodigingsGebied uitnodigingsGebied : gemeente.getUitnodigingsGebieden())
								{
									if (StringUtils.isBlank(uitnodigingsGebied.getGemeenteDeel()) && uitnodigingsGebied.getPostcodeGebied() == null
										&& StringUtils.isBlank(uitnodigingsGebied.getWoonplaats()))
									{
										nieuweVerdeling.setColoscopieCentrum(intakelocatie);
										nieuweVerdeling.setUitnodigingsGebied(uitnodigingsGebied);
										uitnodigingsGebied.getVerdeling().add(nieuweVerdeling);
										hibernateService.saveOrUpdate(uitnodigingsGebied);
										intakelocatie.getCapaciteitVerdeling().add(nieuweVerdeling);
										LOG.info("CapVerdeling stap 2.2.2: nieuwe koppeling naar gemeente " + gemeenteCode + " aangemaakt");
										hibernateService.saveOrUpdate(nieuweVerdeling);
										break;
									}
								}
							}
							else
							{
								param = new HashMap<>();
								param.put("naam", gemeenteCode);
								UitnodigingsGebied uitnodigingsgebied = hibernateService.getUniqueByParameters(UitnodigingsGebied.class, param);
								nieuweVerdeling.setColoscopieCentrum(intakelocatie);
								nieuweVerdeling.setUitnodigingsGebied(uitnodigingsgebied);
								uitnodigingsgebied.getVerdeling().add(nieuweVerdeling);
								hibernateService.saveOrUpdate(uitnodigingsgebied);
								intakelocatie.getCapaciteitVerdeling().add(nieuweVerdeling);
								LOG.info("CapVerdeling stap 2.2.2: nieuwe koppeling naar uitnodigingsgebied " + gemeenteCode + " aangemaakt");
								hibernateService.saveOrUpdate(nieuweVerdeling);
							}
						}
					}

					LOG.info("CapVerdeling stap 2.3: gemeentes/uitnodigingsgebieden die geen percentage hebben verwijderen (indien van toepassing)");
					List<ColoscopieCentrumColonCapaciteitVerdeling> verdelingToDelete = new ArrayList<>();
					for (ColoscopieCentrumColonCapaciteitVerdeling verdeling : intakelocatie.getCapaciteitVerdeling())
					{
						if (Integer.valueOf(0).equals(verdeling.getPercentageCapaciteit()))
						{
							verdelingToDelete.add(verdeling);
							LOG.info("CapVerdeling stap 2.3.1: Koppeling tussen gemeentes/uitnodigingsgebieden " + verdeling.getUitnodigingsGebied().getGemeente().getCode()
								+ " en IL " + intakelocatie.getNaam() + " wordt verwijderd.");
						}
					}
					for (ColoscopieCentrumColonCapaciteitVerdeling verdeling : verdelingToDelete)
					{
						UitnodigingsGebied uitnodigingsGebied = verdeling.getUitnodigingsGebied();
						uitnodigingsGebied.getVerdeling().remove(uitnodigingsGebied);
						hibernateService.saveOrUpdate(uitnodigingsGebied);
						intakelocatie.getCapaciteitVerdeling().remove(verdeling);
						hibernateService.delete(verdeling);
					}
					hibernateService.saveOrUpdate(intakelocatie);
					LOG.info("CapVerdeling stap 4: totaal capaciteit percentage voor " + intakelocatie.getNaam() + " is " + totCapPerIntakelocatie / 100.0 + "%");

					if (totCapPerIntakelocatie < 10000)
					{
						if (melding.length() == 0)
						{
							melding += "Intakelocaties met minder dan 100% capaciteitsverdeling:";
						}
						if (subMelding.length() == 0)
						{
							subMelding = "geen gemeentes met capaciteit";
						}
						else
						{
							subMelding += " = " + totCapPerIntakelocatie / 100.0 + "%";
						}
						melding += "<br>" + intakelocatie.getNaam() + ": " + subMelding;
						level = Level.WARNING;
					}
				}
				else
				{
					LOG.error("CapVerdeling stap 4: intakelocatie niet aanwezig " + ilEntry);
				}
			}
			melding = "Aantal verwerkte intakelocaties: " + capVerdeling.size() + "<br>" + melding;
		}
		catch (Exception e)
		{
			LOG.error("Fout bij importeren ", e);
			melding = ExceptionUtils.getFullStackTrace(e);
			level = Level.ERROR;
		}
		finally
		{
			if (melding.endsWith("\n"))
			{
				melding = melding.substring(0, melding.length() - 2);
			}
			melding = melding.replaceAll("\n", "<br>");
			hibernateService.clearSecondLevelCache();
			LogEvent logEvent = new LogEvent(melding);
			logEvent.setLevel(level);
			logService.logGebeurtenis(LogGebeurtenis.IMPORT_CAP_VERDELING_VERWERKT, logEvent, loggedInInstellingGebruiker, Bevolkingsonderzoek.COLON);
		}

		return level;
	}

	private String getStringCellValue(Row row, int index)
	{
		return getStringCellValue(row, index, 1.0);
	}

	private String getStringCellValue(Row row, int index, double factor)
	{
		String cellValue = null;
		if (row.getLastCellNum() > index && row.getFirstCellNum() <= index)
		{
			Cell cell = row.getCell(index);
			if (cell != null)
			{
				if (cell.getCellType() == Cell.CELL_TYPE_NUMERIC)
				{
					cellValue = "" + ((int) (cell.getNumericCellValue() * factor));
				}
				else
				{
					cellValue = cell.getStringCellValue();
				}
			}
		}
		return cellValue;
	}

	private Double getNummericCellValue(Row row, int index)
	{
		Double cellValue = Double.valueOf(0.0);
		if (row.getLastCellNum() > index && row.getFirstCellNum() <= index)
		{
			Cell cell = row.getCell(index);
			if (cell != null)
			{
				if (cell.getCellType() == Cell.CELL_TYPE_NUMERIC)
				{
					cellValue = Double.valueOf(cell.getNumericCellValue());
				}
				else
				{
					String stringCellValue = cell.getStringCellValue();
					if (StringUtils.isNotBlank(stringCellValue))
					{
						cellValue = Double.valueOf(stringCellValue);
					}
				}
			}
		}
		return cellValue;
	}
}
