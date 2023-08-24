package nl.rivm.screenit.batch.jobs.generalis.gba.upload105step;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-alg
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

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.Calendar;
import java.util.UUID;

import lombok.Setter;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.batch.dao.GbaDao;
import nl.rivm.screenit.batch.jobs.generalis.gba.GbaConstants;
import nl.rivm.screenit.batch.jobs.generalis.gba.GbaFtpConnection;
import nl.rivm.screenit.batch.jobs.generalis.gba.verwerk105step.Vo105ItemWriter;
import nl.rivm.screenit.config.GbaConfig;
import nl.rivm.screenit.model.gba.GbaVerwerkingsLog;
import nl.rivm.screenit.service.FileService;

import org.apache.commons.io.FileUtils;
import org.springframework.batch.core.ExitStatus;
import org.springframework.batch.core.StepContribution;
import org.springframework.batch.core.StepExecution;
import org.springframework.batch.core.StepExecutionListener;
import org.springframework.batch.core.scope.context.ChunkContext;
import org.springframework.batch.core.step.tasklet.Tasklet;
import org.springframework.batch.repeat.RepeatStatus;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.jcraft.jsch.SftpException;

@Slf4j
@Component
public class Vo105UploadTasklet implements Tasklet, StepExecutionListener
{

	@Autowired
	private GbaDao gbaDao;

	@Autowired
	private FileService fileService;

	@Autowired
	private GbaConfig gbaConfig;

	@Setter
	private StepExecution stepExecution;

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public RepeatStatus execute(StepContribution contribution, ChunkContext chunkContext)
	{
		var executionContext = chunkContext.getStepContext().getJobExecutionContext();
		GbaVerwerkingsLog verwerkingsLog = (GbaVerwerkingsLog) executionContext.get(GbaConstants.RAPPORTAGEKEYGBA);
		String vo105Bestand = (String) executionContext.get(Vo105ItemWriter.VO105_BESTAND_KEY);

		var gbaFtpConnection = new GbaFtpConnection(gbaConfig, verwerkingsLog)
		{
			@Override
			protected void ftpActies() throws SftpException
			{
				getChannelSftp().cd(gbaConfig.gbaUploadFolder());

				File file = new File(vo105Bestand);
				saveFile(file);
				try (var inputStream = new FileInputStream(file))
				{
					getChannelSftp().put(inputStream, "VO105_BVO");
				}
				catch (IOException e)
				{
					throw new IllegalStateException("IOExeceptie tijdens upload Vo105", e);
				}

				gbaDao.updateVerstuurdeVragen(stepExecution.getJobExecution().getId().toString());
			}
		};

		if (vo105Bestand != null)
		{
			gbaFtpConnection.run();
			FileUtils.deleteQuietly(new File(vo105Bestand));
		}

		return RepeatStatus.FINISHED;
	}

	private void saveFile(File vo105File)
	{
		StringBuilder directory = new StringBuilder();

		directory.append(gbaConfig.voFileStorePath());

		Calendar cal = Calendar.getInstance();
		directory.append(File.separator);
		directory.append(cal.get(Calendar.YEAR));
		directory.append(File.separator);
		directory.append(cal.get(Calendar.MONTH) + 1);
		directory.append(File.separator);
		directory.append(cal.get(Calendar.DAY_OF_MONTH));
		directory.append(File.separator);

		String uniqueFileName = UUID.randomUUID().toString();
		String fullPath = directory + File.separator + uniqueFileName + "-outgoing.vo105";

		try
		{
			fileService.save(fullPath, vo105File);
			LOG.info("vo105 bestand {} opgeslagen onder {}", vo105File.getPath(), fullPath);

		}
		catch (IOException e)
		{
			LOG.error("Error bij opslaan van vo105 bestand", e);
		}
	}

	@Override
	public void beforeStep(StepExecution stepExecution)
	{
		this.stepExecution = stepExecution;
	}

	@Override
	public ExitStatus afterStep(StepExecution stepExecution)
	{
		return ExitStatus.COMPLETED;
	}
}
