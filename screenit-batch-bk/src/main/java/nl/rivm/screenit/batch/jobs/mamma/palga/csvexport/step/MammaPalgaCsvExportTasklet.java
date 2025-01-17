package nl.rivm.screenit.batch.jobs.mamma.palga.csvexport.step;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bk
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.io.FileWriter;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.text.SimpleDateFormat;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.batch.popupconfig.MammaPalgaExportConfig;
import nl.rivm.screenit.model.batch.popupconfig.MammaPalgaGrondslag;
import nl.rivm.screenit.model.enums.FileStoreLocation;
import nl.rivm.screenit.model.enums.JobStartParameter;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.UploadDocumentService;
import nl.rivm.screenit.service.mamma.MammaPalgaService;
import nl.rivm.screenit.util.CsvUtil;
import nl.rivm.screenit.util.NaamUtil;
import nl.rivm.screenit.util.StringUtil;
import nl.rivm.screenit.util.ZipUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.commons.lang3.StringUtils;
import org.springframework.batch.core.StepContribution;
import org.springframework.batch.core.scope.context.ChunkContext;
import org.springframework.batch.core.step.tasklet.Tasklet;
import org.springframework.batch.repeat.RepeatStatus;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.lang.Nullable;
import org.springframework.stereotype.Component;

import com.fasterxml.jackson.databind.ObjectMapper;

import au.com.bytecode.opencsv.CSVWriter;

@Slf4j
@Component
public class MammaPalgaCsvExportTasklet implements Tasklet
{
	@Autowired
	private String locatieFilestore;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private MammaPalgaService palgaService;

	@Autowired
	private UploadDocumentService uploadDocumentService;

	@Autowired
	private HibernateService hibernateService;

	private final ObjectMapper objectMapper = new ObjectMapper();

	@Override
	public RepeatStatus execute(@Nullable StepContribution contribution, ChunkContext chunkContext) throws IOException
	{
		var jobParameters = chunkContext.getStepContext().getStepExecution()
			.getJobExecution().getJobParameters();
		MammaPalgaExportConfig exportConfig = objectMapper.readValue(jobParameters.getString(JobStartParameter.MAMMA_PALGA_EXPORT.name()), MammaPalgaExportConfig.class);

		var path = locatieFilestore + FileStoreLocation.MAMMA_PALGA_CSV_EXPORT.getPath();
		var filePrefix = getFilePrefix(exportConfig);
		var csvDocuments = genereerCsvDocuments(path, filePrefix, exportConfig);
		palgaService.deleteExports(null, null);
		zipExport(csvDocuments, filePrefix, path);

		return RepeatStatus.FINISHED;
	}

	private List<UploadDocument> genereerCsvDocuments(String path, String filePrefix, MammaPalgaExportConfig exportConfig) throws IOException
	{
		var clientenIds = palgaService.getClientenVoorPalga(exportConfig);
		LOG.info("#clienten gevonden: " + clientenIds.size());
		List<UploadDocument> export = new ArrayList<>();
		if (!clientenIds.isEmpty())
		{
			var aantalClientenPerFile = exportConfig.getMaxAantalPerFile();
			var aantalFiles = (int) Math.ceil((double) clientenIds.size() / aantalClientenPerFile);
			if (aantalFiles > 99)
			{
				throw new IllegalStateException("Te veel files aangemaakt: " + aantalFiles);
			}
			for (var i = 0; i < aantalFiles; i++)
			{
				var from = i * aantalClientenPerFile;
				var to = Math.min((i + 1) * aantalClientenPerFile, clientenIds.size());
				export.add(genereerCsv(clientenIds.subList(from, to), i + 1, path, filePrefix));
			}
		}
		else
		{
			LOG.warn("Geen clienten gevonden voor export.");
		}
		return export;
	}

	private UploadDocument genereerCsv(List<Long> clientenIds, int fileNummer, String path, String prefix) throws IOException
	{
		var file = new File(path);
		file.mkdirs();
		var document = new UploadDocument();
		var fileName = getFileName(prefix, fileNummer) + ".csv";
		file = new File(path + fileName);
		try (var csvOutput = new CSVWriter(new FileWriter(file, false), ';', CSVWriter.NO_QUOTE_CHARACTER))
		{
			LOG.info("Start vullen van CSV voor download: {}, aantal clienten: {}", fileName, clientenIds.size());
			clientenIds.forEach(clientId -> csvOutput.writeNext(getGegevensVanClient(clientId).toArray(new String[] {})));
		}
		CsvUtil.truncateLastLine(file);
		document.setFile(file);
		document.setNaam(fileName);
		document.setPath(file.getPath().replace(locatieFilestore, ""));
		return document;
	}

	private String getFilePrefix(MammaPalgaExportConfig exportConfig)
	{
		var exportdatum = currentDateSupplier.getLocalDate().format(DateTimeFormatter.ofPattern(Constants.DATE_FORMAT_YYYYMMDD));
		return String.format("CHTRDS%s%s%02d", exportdatum, exportConfig.getGewensteUitslag().getCodeInFilePrefix(), leveringsnummerVoorFilePrefix(exportConfig));
	}

	private int leveringsnummerVoorFilePrefix(MammaPalgaExportConfig exportConfig)
	{
		return exportConfig.getGrondslag() == MammaPalgaGrondslag.KWALITEITSBORGING ? exportConfig.getVolgnummerKwaliteitsborging() : 0;
	}

	private String getFileName(String prefix, int fileNummer)
	{
		return prefix + String.format("%02d", fileNummer);
	}

	private List<String> getGegevensVanClient(Long clientId)
	{
		var client = hibernateService.get(Client.class, clientId);
		var dateFormat = new SimpleDateFormat(Constants.DATE_FORMAT_YYYYMMDD);
		List<String> gegevens = new ArrayList<>();
		var persoon = client.getPersoon();

		gegevens.add(Long.toString(client.getMammaDossier().getId()));
		var voorlettersClient = NaamUtil.getVoorlettersClient(client);
		var voorletterClient = StringUtils.isNoneBlank(voorlettersClient) && voorlettersClient.length() > 0 && StringUtil.isAlfabetKarakter(voorlettersClient.charAt(0))
			? voorlettersClient.substring(0, 1) : "";
		gegevens.add(voorletterClient);
		gegevens.add(persoon.getAchternaam().trim());
		gegevens.add(dateFormat.format(persoon.getGeboortedatum()));
		gegevens.add(persoon.getGeslacht().getMnem());
		gegevens.add(persoon.getBsn());
		hibernateService.getHibernateSession().evict(client);
		return gegevens;
	}

	private void zipExport(List<UploadDocument> export, String prefix, String path) throws IOException
	{
		if (!export.isEmpty())
		{
			String fileName = getFileName(prefix, 0) + ".zip";
			File zipFile = ZipUtil.maakZips(export, path + fileName, 1048576).iterator().next();
			UploadDocument zipDocument = new UploadDocument();
			zipDocument.setFile(zipFile);
			zipDocument.setNaam(fileName);
			zipDocument.setActief(true);
			zipDocument.setContentType("application/zip");
			for (UploadDocument document : export)
			{
				uploadDocumentService.delete(document);
			}
			palgaService.saveOrUpdateExport(zipDocument);
		}
		else
		{
			LOG.warn("Geen export om te zippen.");
		}
	}

	private void truncateLastLine(File file)
	{
		try (RandomAccessFile randomAccessFile = new RandomAccessFile(file, "rw"))
		{
			randomAccessFile.setLength(randomAccessFile.length() - 1);
		}
		catch (IOException e)
		{
			LOG.error("Error bij het truncaten van de laatste regel van het bestand", e);
		}
	}

}
