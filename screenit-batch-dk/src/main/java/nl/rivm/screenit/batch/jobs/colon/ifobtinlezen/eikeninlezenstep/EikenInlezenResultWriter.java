
package nl.rivm.screenit.batch.jobs.colon.ifobtinlezen.eikeninlezenstep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-dk
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
import java.io.IOException;
import java.math.BigDecimal;
import java.util.List;

import nl.rivm.screenit.batch.BaseCsvFileReader;
import nl.rivm.screenit.batch.jobs.colon.ifobtinlezen.IfobtInlezenConstants;
import nl.rivm.screenit.dao.colon.IFOBTResultDao;
import nl.rivm.screenit.dao.colon.IFobtDao;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.colon.IFOBTBestand;
import nl.rivm.screenit.model.colon.IFOBTResult;
import nl.rivm.screenit.model.colon.IFOBTTest;
import nl.rivm.screenit.model.colon.IFOBTUitslag;
import nl.rivm.screenit.model.colon.IFobtLaboratorium;
import nl.rivm.screenit.model.colon.enums.IFOBTBestandStatus;
import nl.rivm.screenit.model.colon.enums.IFOBTUitslagType;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.logging.IfobtVerwerkingBeeindigdLogEvent;
import nl.rivm.screenit.model.verwerkingverslag.IfobtVerwerkingRapportageEntry;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.core.JobExecution;
import org.springframework.batch.core.StepExecution;
import org.springframework.batch.core.annotation.BeforeStep;
import org.springframework.batch.item.ExecutionContext;
import org.springframework.batch.item.ItemWriter;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

public class EikenInlezenResultWriter implements ItemWriter<IFOBTResult>
{

	private static final Logger LOG = LoggerFactory.getLogger(EikenInlezenResultWriter.class);

	@Autowired
	private IFobtDao ifobtDao;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private LogService logService;

	@Autowired
	private IFOBTResultDao ifobtResultDao;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	@Qualifier("eikenFileLocation")
	private String eikenFileLocation;

	private StepExecution stepExecution;

	private JobExecution jobExecution;

	@Override
	public void write(List<? extends IFOBTResult> items) throws Exception
	{
		IfobtVerwerkingBeeindigdLogEvent logEvent = (IfobtVerwerkingBeeindigdLogEvent) stepExecution.getJobExecution().getExecutionContext()
			.get(IfobtInlezenConstants.RAPPORTAGEKEYINLEZEN);

		Long eikenBestandId = (Long) stepExecution.getJobExecution().getExecutionContext().get(IfobtInlezenConstants.RAPPORTAGEKEYCURRENTIFOBTBESTAND);
		try
		{
			List<IfobtVerwerkingRapportageEntry> bestanden = logEvent.getRapportage().getBestanden();

			IFOBTBestand bestand = null;
			IfobtVerwerkingRapportageEntry verslagEntry = null;
			LOG.trace("Start met chunk");
			for (IFOBTResult eikenResult : items)
			{
				try
				{
					boolean onbekendeBarcode = false;
					String barcode = eikenResult.getSid();
					LOG.trace(barcode + " " + eikenResult.getBestandsNaam());
					IFOBTTest eikenBuis = ifobtDao.getIfobtTest(barcode);

					IFOBTUitslag uitslag = new IFOBTUitslag();
					uitslag.setAnalyseDatum(eikenResult.getDateTimeResult());
					uitslag.setBarcode(barcode);

					if (eikenBuis == null)
					{
						String melding = "FIT of controle buis met barcode " + barcode + " in bestand " + eikenResult.getBestandsNaam() + " bestaat niet.";
						logService.logGebeurtenis(LogGebeurtenis.IFOBT_ONBEKENDE_BARCODE, null, melding, Bevolkingsonderzoek.COLON);
						addMelding(melding);
						LOG.warn(melding);

						onbekendeBarcode = true;
					}
					else
					{
						uitslag.setType(IFOBTUitslagType.CLIENT_EIKEN);
						logAction(eikenBuis);
					}
					bestand = createOrGetBestand(eikenResult);

					if (!bestand.getId().equals(eikenBestandId))
					{
						origBestandKanVerijderdWorden(eikenBestandId);
						eikenBestandId = bestand.getId();
						verslagEntry = null;
						LOG.debug("Nieuw bestand " + bestand.getNaamBestand());
					}
					if (verslagEntry == null)
					{
						for (IfobtVerwerkingRapportageEntry entry : bestanden)
						{
							if (entry.getIfobtBestandId().equals(bestand.getId()))
							{
								verslagEntry = entry;
								break;
							}
						}
						if (verslagEntry == null)
						{
							verslagEntry = new IfobtVerwerkingRapportageEntry();
							verslagEntry.setBestandsNaam(bestand.getNaamBestand());
							verslagEntry.setRapportage(logEvent.getRapportage());
							verslagEntry.setIfobtBestandId(bestand.getId());
							bestanden.add(verslagEntry);
						}
						LOG.debug("Nieuw verslagEntry " + verslagEntry.getBestandsNaam());
					}

					if (!onbekendeBarcode)
					{
						uitslag.setBestand(bestand);
						uitslag.setUitslag(new BigDecimal(eikenResult.getResultValue()));
						hibernateService.saveOrUpdate(uitslag);

						if (!IFOBTUitslagType.CLIENT_EIKEN.equals(uitslag.getType()))
						{
							bestand.setAantalControleUitslagen(bestand.getAantalControleUitslagen() + 1);
						}
					}

					verslagEntry.setAantalVerwerkingen(verslagEntry.getAantalVerwerkingen() + 1);
				}
				catch (Exception e)
				{
					String melding = e.getMessage();
					if (melding == null)
					{
						melding = "Onbekende fout in regel met barcode " + eikenResult.getSid() + " in bestand " + eikenResult.getBestandsNaam();
					}

					LOG.warn("Fout bij verwerking van uitslag regel ", e);
					addMelding(melding);
				}
			}

			Boolean hasMoreFiles = (Boolean) getExecutionContext().get(BaseCsvFileReader.RAPPORTAGEKEYHASMOREFILES);
			Boolean hasMoreResultsForNewChunk = (Boolean) getExecutionContext().get(BaseCsvFileReader.RAPPORTAGEKEYHASMORERESULTS);
			if (Boolean.FALSE.equals(hasMoreFiles) && Boolean.FALSE.equals(hasMoreResultsForNewChunk))
			{
				origBestandKanVerijderdWorden(eikenBestandId);
			}
		}
		catch (Exception e)
		{
			LOG.warn("Fout bij verwerking van chunk", e);
			addMelding(e.getMessage());
		}

		getExecutionContext().put(IfobtInlezenConstants.RAPPORTAGEKEYCURRENTIFOBTBESTAND, eikenBestandId);
	}

	private void addMelding(String melding)
	{
		String huidigeMelding = "";
		ExecutionContext context = jobExecution.getExecutionContext();
		if (context.containsKey(BaseCsvFileReader.RAPPORTAGEKEYFOUTINBESTAND))
		{
			huidigeMelding = context.getString(BaseCsvFileReader.RAPPORTAGEKEYFOUTINBESTAND);
		}

		if (melding == null)
		{
			melding = "Er is een onbekende fout opgetreden bij de verwerking van een set van uitslagen. Neem contact op met de helpdesk.";
		}
		if (StringUtils.isBlank(huidigeMelding))
		{
			huidigeMelding = melding;
		}
		else if (!huidigeMelding.contains(melding))
		{
			huidigeMelding += "<br>" + melding;
		}
		context.putString(BaseCsvFileReader.RAPPORTAGEKEYFOUTINBESTAND, huidigeMelding);
	}

	protected ExecutionContext getExecutionContext()
	{
		return jobExecution.getExecutionContext();
	}

	protected ExecutionContext getStepExecutionContext()
	{
		return stepExecution.getExecutionContext();
	}

	private void origBestandKanVerijderdWorden(Long eikenBestandId)
	{
		if (eikenBestandId != null)
		{
			IFOBTBestand prevBestand = hibernateService.load(IFOBTBestand.class, eikenBestandId);
			prevBestand.setStatus(IFOBTBestandStatus.KAN_ORIG_BESTAND_VERWIJDEREN);
			hibernateService.saveOrUpdate(prevBestand);
		}
	}

	private IFOBTBestand createOrGetBestand(IFOBTResult eikenResult) throws IOException
	{
		IFOBTBestand bestand = ifobtDao.getIfobtBestand(eikenResult.getBestandsNaam());
		if (bestand == null)
		{
			bestand = new IFOBTBestand();
			bestand.setStatus(IFOBTBestandStatus.NIEUW);
			bestand.setStatusDatum(currentDateSupplier.getDate());
			bestand.setInstumentId(eikenResult.getInstrumentID());
			String labID = eikenResult.getLabID();
			IFobtLaboratorium eikenLaboratorium = ifobtResultDao.getIFobtLaboratorium(labID);
			if (eikenLaboratorium == null)
			{
				String melding = "Eiken laboratorium met id " + labID + " in bestand " + eikenResult.getBestandsNaam() + " bestaat niet";
				throw new IllegalStateException(melding);
			}
			bestand.setLaboratorium(eikenLaboratorium);

			bestand.setNaamBestand(eikenResult.getBestandsNaam());
			String seperator = System.getProperty("file.separator");
			File file = new File(eikenFileLocation + seperator + eikenResult.getBestandsNaam());
			String archiefLocationNaam = eikenFileLocation + seperator + "archief" + seperator + System.currentTimeMillis() + "-" + file.getName();
			FileUtils.copyFile(file, new File(archiefLocationNaam));
			bestand.setPathBestand(archiefLocationNaam);

			hibernateService.saveOrUpdate(bestand);

		}
		return bestand;
	}

	public void setIfobtDao(IFobtDao ifobtDao)
	{
		this.ifobtDao = ifobtDao;
	}

	public void setHibernateService(HibernateService hibernateService)
	{
		this.hibernateService = hibernateService;
	}

	public void setIfobtResultDao(IFOBTResultDao ifobtResultDao)
	{
		this.ifobtResultDao = ifobtResultDao;
	}

	public void setCurrentDateSupplier(ICurrentDateSupplier currentDateSupplier)
	{
		this.currentDateSupplier = currentDateSupplier;
	}

	public void setLogService(LogService logService)
	{
		this.logService = logService;
	}

	private void logAction(IFOBTTest ifobt)
	{
		Client client = ifobt.getColonUitnodigingExtra().getScreeningRonde().getDossier().getClient();
		logService.logGebeurtenis(LogGebeurtenis.IFOBT_ONTVANGEN, client, "barcode: " + ifobt.getBarcode(), Bevolkingsonderzoek.COLON);
	}

	@BeforeStep
	public void saveStepExecution(StepExecution stepExecution)
	{
		this.stepExecution = stepExecution;
		this.jobExecution = stepExecution.getJobExecution();
	}
}
