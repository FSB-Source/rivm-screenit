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

import java.io.File;
import java.io.FileInputStream;
import java.io.PushbackInputStream;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.main.service.colon.ImportCapVerdelingService;
import nl.rivm.screenit.model.Gemeente;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.colon.ColonIntakelocatie;
import nl.rivm.screenit.model.colon.ColoscopieCentrumColonCapaciteitVerdeling;
import nl.rivm.screenit.model.colon.UitnodigingsGebied;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Level;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.colon.ColonUitnodigingsgebiedService;
import nl.rivm.screenit.util.BigDecimalUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.commons.lang.exception.ExceptionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.CellType;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.WorkbookFactory;
import org.jetbrains.annotations.Nullable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Slf4j
@Service
@RequiredArgsConstructor
public class ImportCapVerdelingServiceImpl implements ImportCapVerdelingService
{

	private static final int START_IL_ID_ROW = 1;

	private static final int START_IL_ID_COL = 8;

	private static final int GEMEENTECODE_COL = 2;

	private final HibernateService hibernateService;

	private final LogService logService;

	private final ColonUitnodigingsgebiedService uitnodigingsgebiedService;

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public Level verwerkBestand(InstellingGebruiker loggedInInstellingGebruiker, File file)
	{
		var melding = "";
		var level = Level.INFO;
		try (var workbook = WorkbookFactory.create(new PushbackInputStream(new FileInputStream(file))))
		{
			workbook.setMissingCellPolicy(Row.MissingCellPolicy.CREATE_NULL_AS_BLANK);

			var sheet = workbook.getSheetAt(0);
			if (sheet == null)
			{
				throw new IllegalStateException("Geen sheet in het excel gevonden");
			}

			var totaleCapaciteitVerdeling = new TreeMap<Long, List<String>>();
			var ilIdToNaam = new HashMap<Long, String>();

			leesExcel(sheet, totaleCapaciteitVerdeling, ilIdToNaam);

			var gewijzigdeGebieden = new HashSet<UitnodigingsGebied>();
			for (var ilEntry : totaleCapaciteitVerdeling.entrySet())
			{
				var intakelocatie = zoekIntakelocatie(ilIdToNaam, ilEntry.getKey());
				if (intakelocatie != null)
				{
					LOG.info("CapVerdeling stap 2: Nieuwe capaciteit toepassen bij " + intakelocatie.getNaam());
					resetCapaciteitVerdeling(intakelocatie);
					LOG.info("CapVerdeling stap 2.2: Per gemeente/uitnodigingsgebied (nieuwe) verdeling maken");
					var totCapPerIntakelocatie = 0;
					var subMelding = "";
					var ilCapaciteitsVerdeling = ilEntry.getValue();
					for (var capaciteitPerGebied : ilCapaciteitsVerdeling)
					{
						String[] splitted = capaciteitPerGebied.split(",");
						String gemeenteCode = StringUtils.leftPad(splitted[0], 4, '0');
						Integer adherentie = Integer.valueOf(splitted[1]);
						Integer capaciteit = Integer.valueOf(splitted[2]);
						if (subMelding.length() > 0)
						{
							subMelding += " + ";
						}
						subMelding += gemeenteCode + " -> " + capaciteit / 100.0 + "%";
						totCapPerIntakelocatie += capaciteit;
						werkUitnodigingsgebiedenBij(intakelocatie, gemeenteCode, adherentie, capaciteit, gewijzigdeGebieden);
					}
					verwijderOngebruikteKoppelingenMetGebieden(intakelocatie, gewijzigdeGebieden);

					LOG.info("CapVerdeling stap 4: totaal capaciteit percentage voor " + intakelocatie.getNaam() + " is " + totCapPerIntakelocatie / 100.0 + "%");

					if (totCapPerIntakelocatie < 10000)
					{
						if (melding.length() == 0)
						{
							melding += "Intakelocaties met minder dan 100% capaciteitsverdeling:<br>";
						}
						if (subMelding.length() == 0)
						{
							subMelding = "geen uitnodigingsgebieden met capaciteit";
						}
						else
						{
							subMelding += " = " + totCapPerIntakelocatie / 100.0 + "%";
						}
						melding += intakelocatie.getNaam() + ": " + subMelding + "<br>";
						level = Level.WARNING;
					}
				}
				else
				{
					LOG.error("CapVerdeling stap 2: intakelocatie niet aanwezig " + ilEntry);
				}
			}
			String adherentieMelding = uitnodigingsgebiedService.valideerAdherentieVanGewijzigdeGebieden(gewijzigdeGebieden);
			melding = "Aantal aangepaste intakelocaties: " + totaleCapaciteitVerdeling.size() + "<br>" + melding +
				"Aantal aangepaste uitnodigingsgebieden: " + gewijzigdeGebieden.size() + adherentieMelding;
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

	private void leesExcel(Sheet sheet, Map<Long, List<String>> totaleCapVerdeling, Map<Long, String> ilidToNaam)
	{
		LOG.info("CapVerdeling stap 1: Nieuwe capaciteit uit excel halen");
		var rowIdx = START_IL_ID_ROW;
		var colToId = new HashMap<Integer, Long>();
		while (sheet.getLastRowNum() >= rowIdx)
		{
			var row = sheet.getRow(rowIdx);
			if (rowIdx == START_IL_ID_ROW)
			{
				rowIdx = leesHeader(row, rowIdx, colToId, totaleCapVerdeling, ilidToNaam);
			}
			else
			{
				rowIdx = leesCapaciteitEnAdherentie(row, rowIdx, colToId, totaleCapVerdeling);
			}
		}
	}

	private int leesHeader(Row row, int rowIdx, Map<Integer, Long> colToId, Map<Long, List<String>> totaleCapVerdeling, Map<Long, String> ilidToNaam)
	{
		var colIdx = START_IL_ID_COL;
		Sheet sheet = row.getSheet();
		while (row.getLastCellNum() >= colIdx)
		{
			var ilId = getNummericCellValue(row, colIdx).longValue();
			totaleCapVerdeling.put(ilId, new ArrayList<>());
			colToId.put(colIdx, ilId);
			ilidToNaam.put(ilId, getStringCellValue(sheet.getRow(rowIdx - 1), colIdx - 1));
			colIdx += 4;
		}
		return rowIdx + 2;
	}

	private int leesCapaciteitEnAdherentie(Row row, int rowIdx, Map<Integer, Long> colToId, Map<Long, List<String>> totaleCapVerdeling)
	{
		var gemeenteCode = getStringCellValue(row, GEMEENTECODE_COL);
		if (StringUtils.isNotBlank(gemeenteCode))
		{
			var colIdx = START_IL_ID_COL;
			while (row.getLastCellNum() >= colIdx && colToId.containsKey(colIdx))
			{
				var adherentie = "";
				var numericCellValue = getNummericCellValue(row, colIdx - 1);
				if (numericCellValue.compareTo(BigDecimal.ZERO) > 0)
				{
					adherentie = BigDecimalUtil.decimalToString(numericCellValue.multiply(BigDecimal.valueOf(10000)));
				}
				var capaciteit = "";
				numericCellValue = getNummericCellValue(row, colIdx);
				if (numericCellValue.compareTo(BigDecimal.ZERO) > 0)
				{
					capaciteit = BigDecimalUtil.decimalToString(numericCellValue.multiply(BigDecimal.valueOf(10000)));
				}

				if (StringUtils.isNotBlank(adherentie) && StringUtils.isNotBlank(capaciteit))
				{
					totaleCapVerdeling.get(colToId.get(colIdx)).add(gemeenteCode + "," + adherentie + "," + capaciteit);
				}
				colIdx += 4;
			}
		}
		return ++rowIdx;
	}

	@Nullable
	private ColonIntakelocatie zoekIntakelocatie(Map<Long, String> ilIdToNaam, Long ilId)
	{
		var intakelocatie = hibernateService.get(ColonIntakelocatie.class, ilId);
		var ilNaam = ilIdToNaam.get(ilId);
		while ((intakelocatie == null || ilNaam.indexOf(intakelocatie.getNaam()) < 0) && StringUtils.isNotBlank(ilNaam))
		{
			intakelocatie = hibernateService.getUniqueByParameters(ColonIntakelocatie.class, Map.of("naam", ilNaam.trim()));
			ilNaam = ilNaam.substring(1);
		}
		return intakelocatie;
	}

	private static void resetCapaciteitVerdeling(ColonIntakelocatie intakelocatie)
	{
		LOG.info("CapVerdeling stap 2.1: Hele capaciteitsverdeling van IL op 0% zetten");
		for (ColoscopieCentrumColonCapaciteitVerdeling verdeling : intakelocatie.getCapaciteitVerdeling())
		{
			verdeling.setPercentageCapaciteit(0);
		}
	}

	private void werkUitnodigingsgebiedenBij(ColonIntakelocatie intakelocatie, String gemeenteCode, Integer adherentie, Integer capaciteit,
		Set<UitnodigingsGebied> gewijzigdeGebieden)
	{
		boolean foundGemeente = werkBestaandeUitnodigingsgebiedenBij(intakelocatie, gemeenteCode, adherentie, capaciteit, gewijzigdeGebieden);
		if (!foundGemeente)
		{
			LOG.info("CapVerdeling stap 2.2.2: nieuwe koppeling naar uitnodigingsgebied/gemeente " + gemeenteCode);
			var nieuweVerdeling = new ColoscopieCentrumColonCapaciteitVerdeling();
			nieuweVerdeling.setPercentageCapaciteit(capaciteit);
			nieuweVerdeling.setPercentageAdherentie(adherentie);

			var gemeente = hibernateService.getUniqueByParameters(Gemeente.class, Map.of("code", gemeenteCode));
			if (gemeente != null)
			{
				maakNieuweKoppelingMetUitnodigingsgebiedMetGemeenteCode(intakelocatie, gemeenteCode, nieuweVerdeling, gemeente, gewijzigdeGebieden);
			}
			else
			{
				maakNieuweKoppelingMetUitnodigingsgebiedMetGebiedsnaam(intakelocatie, gemeenteCode, nieuweVerdeling, gewijzigdeGebieden);
			}
		}
	}

	private boolean werkBestaandeUitnodigingsgebiedenBij(ColonIntakelocatie intakelocatie, String gemeenteCode, Integer adherentie, Integer capaciteit,
		Set<UitnodigingsGebied> gewijzigdeGebieden)
	{
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
					gewijzigdeGebieden.add(uitnodigingsGebied);
					foundGemeente = true;
				}
			}
			else if (uitnodigingsGebied.getNaam().equals(gemeenteCode))
			{
				verdeling.setPercentageCapaciteit(capaciteit);
				verdeling.setPercentageAdherentie(adherentie);
				hibernateService.saveOrUpdate(uitnodigingsGebied);
				gewijzigdeGebieden.add(uitnodigingsGebied);
				foundGemeente = true;
			}
		}
		return foundGemeente;
	}

	private void maakNieuweKoppelingMetUitnodigingsgebiedMetGemeenteCode(ColonIntakelocatie intakelocatie, String gemeenteCode,
		ColoscopieCentrumColonCapaciteitVerdeling nieuweVerdeling, Gemeente gemeente, Set<UitnodigingsGebied> gewijzigdeGebieden)
	{
		for (var uitnodigingsGebied : gemeente.getUitnodigingsGebieden())
		{
			if (StringUtils.isBlank(uitnodigingsGebied.getGemeenteDeel()) && uitnodigingsGebied.getPostcodeGebied() == null
				&& StringUtils.isBlank(uitnodigingsGebied.getWoonplaats()))
			{
				gewijzigdeGebieden.add(uitnodigingsGebied);
				nieuweVerdeling.setIntakelocatie(intakelocatie);
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

	private void maakNieuweKoppelingMetUitnodigingsgebiedMetGebiedsnaam(ColonIntakelocatie intakelocatie, String gemeenteCode,
		ColoscopieCentrumColonCapaciteitVerdeling nieuweVerdeling, Set<UitnodigingsGebied> gewijzigdeGebieden)
	{
		var uitnodigingsgebied = hibernateService.getUniqueByParameters(UitnodigingsGebied.class, Map.of("naam", gemeenteCode));
		gewijzigdeGebieden.add(uitnodigingsgebied);
		nieuweVerdeling.setIntakelocatie(intakelocatie);
		nieuweVerdeling.setUitnodigingsGebied(uitnodigingsgebied);
		uitnodigingsgebied.getVerdeling().add(nieuweVerdeling);
		hibernateService.saveOrUpdate(uitnodigingsgebied);
		intakelocatie.getCapaciteitVerdeling().add(nieuweVerdeling);
		LOG.info("CapVerdeling stap 2.2.2: nieuwe koppeling naar uitnodigingsgebied " + gemeenteCode + " aangemaakt");
		hibernateService.saveOrUpdate(nieuweVerdeling);
	}

	private void verwijderOngebruikteKoppelingenMetGebieden(ColonIntakelocatie intakelocatie, Set<UitnodigingsGebied> gewijzigdeGebieden)
	{
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
			uitnodigingsGebied.getVerdeling().remove(verdeling);
			hibernateService.saveOrUpdate(uitnodigingsGebied);
			intakelocatie.getCapaciteitVerdeling().remove(verdeling);
			hibernateService.delete(verdeling);
			gewijzigdeGebieden.add(uitnodigingsGebied);
		}
		hibernateService.saveOrUpdate(intakelocatie);
	}

	private static String getStringCellValue(Row row, int index)
	{
		return getStringCellValue(row, index, BigDecimal.ONE);
	}

	private static String getStringCellValue(Row row, int index, BigDecimal factor)
	{
		String cellValue = null;
		if (row.getLastCellNum() > index && row.getFirstCellNum() <= index)
		{
			Cell cell = row.getCell(index);
			if (cell != null)
			{
				if (cell.getCellType() == CellType.NUMERIC)
				{
					cellValue = BigDecimalUtil.decimalToString(BigDecimal.valueOf(cell.getNumericCellValue()).multiply(factor));
				}
				else
				{
					cellValue = cell.getStringCellValue();
				}
			}
		}
		return cellValue;
	}

	private static BigDecimal getNummericCellValue(Row row, int index)
	{
		var cellValue = BigDecimal.ZERO;
		var stringCelValue = getStringCellValue(row, index);
		if (StringUtils.isNotBlank(stringCelValue))
		{
			cellValue = new BigDecimal(stringCelValue);
		}
		return cellValue;
	}
}
