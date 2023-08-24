package nl.rivm.screenit.main.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.main.comparator.SKMLExterneSchemaComparator;
import nl.rivm.screenit.main.dao.SKMLExternSchemaDao;
import nl.rivm.screenit.main.model.SKMLImportVoortgang;
import nl.rivm.screenit.main.model.SKMLSchemaMapping;
import nl.rivm.screenit.main.service.SKMLExterneSchemaXlsService;
import nl.rivm.screenit.main.util.SKMLXlsUtil;
import nl.rivm.screenit.model.colon.SKMLExternSchema;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.ss.usermodel.WorkbookFactory;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.REQUIRED)
@Slf4j
@AllArgsConstructor
public class SKMLExterneSchemaXlsServiceImpl implements SKMLExterneSchemaXlsService
{

	private final SKMLExternSchemaDao skmlDao;

	private final HibernateService hibernateService;

	private final ICurrentDateSupplier currentDateSupplier;

	@Override
	public SKMLImportVoortgang importSchemaXls(InputStream inputStream, boolean allesOverschrijven)
	{
		LOG.info("Start met import xls SKML Extern Schema");
		SKMLImportVoortgang voortgang = new SKMLImportVoortgang();
		voortgang.setStart(currentDateSupplier.getDate());
		List<SKMLExternSchema> schemaLijst = new ArrayList<>();
		Workbook workbook = maakWorkBook(inputStream);
		voortgang = verwerkWorkbook(workbook, schemaLijst, voortgang);
		Collections.sort(schemaLijst, new SKMLExterneSchemaComparator());
		voortgang = verwerkSchemas(schemaLijst, allesOverschrijven, voortgang);
		voortgang.setEind(currentDateSupplier.getDate());
		LOG.info("Klaar met import xls SKML Extern Schema");
		return voortgang;
	}

	private SKMLImportVoortgang verwerkSchemas(List<SKMLExternSchema> schemas, boolean allesOverschrijven, SKMLImportVoortgang voortgang)
	{
		if (CollectionUtils.isNotEmpty(schemas))
		{
			if (allesOverschrijven)
			{
				List<SKMLExternSchema> oudeSchemas = new ArrayList<>();
				oudeSchemas.addAll(skmlDao.haalSchemasVanafDeadlineDatum(schemas.get(0).getDeadline()));
				if (CollectionUtils.isNotEmpty(oudeSchemas))
				{
					for (SKMLExternSchema schema : oudeSchemas)
					{
						LOG.debug("Schema met ID: " + schema.getId() + " wordt verwijderd.");
						schema.setActief(false);
						voortgang.setVerwijderd(voortgang.getVerwijderd() + 1);
					}
					hibernateService.saveOrUpdateAll(oudeSchemas);
				}
			}
			for (SKMLExternSchema schema : schemas)
			{
				List<SKMLExternSchema> lijst = skmlDao.zoekSchema(schema, true);
				if (CollectionUtils.isNotEmpty(lijst))
				{
					SKMLExternSchema dbSchema = lijst.get(0);
					dbSchema.setJaar(schema.getJaar());
					dbSchema.setRonde(schema.getRonde());
					dbSchema.setLetter(schema.getLetter());
					LOG.debug("Bestaand schema met ID: " + dbSchema.getId() + " wordt geupdate.");
					voortgang.setGeupdate(voortgang.getGeupdate() + 1);
					hibernateService.saveOrUpdate(dbSchema);
				}
				else
				{
					LOG.debug("Nieuw schema wordt opgeslagen.");
					voortgang.setNieuw(voortgang.getNieuw() + 1);
					hibernateService.saveOrUpdate(schema);
				}
			}
		}
		else
		{
			String foutmelding = "Er waren geen SKML externe controle schemas om te verwerken";
			LOG.warn(foutmelding);
			voortgang.getFoutmeldingen().add(foutmelding);
		}
		return voortgang;
	}

	private Workbook maakWorkBook(InputStream xlsStream)
	{
		Workbook workbook = null;
		try
		{
			workbook = WorkbookFactory.create(xlsStream);
			workbook.setMissingCellPolicy(Row.MissingCellPolicy.CREATE_NULL_AS_BLANK);
		}
		catch (IOException e)
		{
			LOG.error(e.getMessage(), e);
		}
		return workbook;
	}

	private SKMLImportVoortgang verwerkWorkbook(Workbook workbook, List<SKMLExternSchema> schemaLijst, SKMLImportVoortgang voortgang)
	{
		if (workbook != null)
		{
			int aantalSheets = workbook.getNumberOfSheets();
			int sheetCounter = 0;
			while (sheetCounter < aantalSheets)
			{
				verwerkSheet(workbook.getSheetAt(sheetCounter), schemaLijst, voortgang);
				sheetCounter++;
			}
		}
		else
		{
			String foutmelding = "Workbook is null. Er was een probleem bij het omzetten van de inputtream naar Workbook. SKML extern schema import is gestaakt";
			LOG.error(foutmelding);
			voortgang.getFoutmeldingen().add(foutmelding);
		}
		return voortgang;
	}

	private SKMLImportVoortgang verwerkSheet(Sheet sheet, List<SKMLExternSchema> schemaLijst, SKMLImportVoortgang voortgang)
	{
		LOG.info("Verwerk sheet met naam: " + sheet.getSheetName());
		int rowCounter = 0;
		boolean klaarMetSheet = false;
		SKMLSchemaMapping mapping = null;
		while (!klaarMetSheet)
		{
			Row row = sheet.getRow(rowCounter);
			if (row != null && rowCounter == 0)
			{
				mapping = maakMapping(row);
				rowCounter++;
			}
			else if (row != null && mapping != null)
			{
				voortgang = verwerkRow(row, mapping, schemaLijst, voortgang);
				rowCounter++;
			}
			else
			{
				LOG.info("Klaar met Sheet: " + sheet.getSheetName() + ", einde op rij: " + rowCounter);
				klaarMetSheet = true;
			}
		}
		return voortgang;
	}

	private SKMLImportVoortgang verwerkRow(Row row, SKMLSchemaMapping mapping, List<SKMLExternSchema> schemaLijst, SKMLImportVoortgang voortgang)
	{
		SKMLExternSchema schema = new SKMLExternSchema();
		schema.setActief(Boolean.TRUE);
		schema.setJaar(SKMLXlsUtil.getIntFromCell(row.getCell(mapping.getSkmljaar())));
		schema.setRonde(SKMLXlsUtil.getIntFromCell(row.getCell(mapping.getSkmlronde())));
		schema.setLetter(SKMLXlsUtil.formatEnValideerMonsterLetter(SKMLXlsUtil.getStringFromCell(row.getCell(mapping.getMonsterletter()))));
		schema.setDeadline(SKMLXlsUtil.getDateFromCell(row.getCell(mapping.getDeadline())));
		if (SKMLXlsUtil.isSKMLExternSchemaValide(schema))
		{
			schemaLijst.add(schema);
		}
		else
		{
			SimpleDateFormat format = new SimpleDateFormat("dd-MM-yyyy");
			String deadline = "";
			if (schema.getDeadline() != null)
			{
				deadline = format.format(schema.getDeadline());
			}
			String foutmelding = "Fout bij regel " + row.getRowNum() + " [J: " + schema.getJaar() + ", R: " + schema.getRonde() + ", L: " + schema.getLetter() + ", D: " + deadline
				+ "]";
			LOG.warn(foutmelding);
			voortgang.getFoutmeldingen().add(foutmelding);
		}
		return voortgang;
	}

	private SKMLSchemaMapping maakMapping(Row row)
	{
		LOG.debug("Begonnen met aanmaken mappings object.");
		SKMLSchemaMapping mapping = new SKMLSchemaMapping();
		int rijenTeller = 0;
		while (rijenTeller < 100)
		{
			String mappingString = row.getCell(rijenTeller).getStringCellValue();
			if (StringUtils.isNotBlank(mappingString))
			{
				if (StringUtils.equalsIgnoreCase("skml jaar", mappingString))
				{
					mapping.setSkmljaar(rijenTeller);
				}
				else if (StringUtils.equalsIgnoreCase("skml ronde", mappingString))
				{
					mapping.setSkmlronde(rijenTeller);
				}
				else if (StringUtils.equalsIgnoreCase("monster letter", mappingString))
				{
					mapping.setMonsterletter(rijenTeller);
				}
				else if (StringUtils.equalsIgnoreCase("deadline", mappingString))
				{
					mapping.setDeadline(rijenTeller);
				}

				if (SKMLXlsUtil.isMappingCompleet(mapping))
				{
					LOG.debug("Mapping is compleet");
					break;
				}
			}
			rijenTeller++;
		}

		if (!SKMLXlsUtil.isMappingCompleet(mapping))
		{
			LOG.warn("Mapping is incompleet, niet alle velden konden worden toegewezen in het XLS bestand.");
			mapping = null;
		}
		LOG.debug("Klaar met aanmaken mappings object.");
		return mapping;
	}
}
