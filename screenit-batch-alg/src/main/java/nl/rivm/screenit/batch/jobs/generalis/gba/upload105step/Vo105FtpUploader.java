package nl.rivm.screenit.batch.jobs.generalis.gba.upload105step;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-alg
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
import java.io.IOException;
import java.util.UUID;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.batch.jobs.generalis.gba.GbaFtpConnection;
import nl.rivm.screenit.config.GbaConfig;
import nl.rivm.screenit.model.enums.FileStoreLocation;
import nl.rivm.screenit.model.gba.GbaVerwerkingsLog;
import nl.rivm.screenit.repository.algemeen.GbaVraagRepository;
import nl.rivm.screenit.service.FileService;
import nl.rivm.screenit.service.ICurrentDateSupplier;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.config.ConfigurableBeanFactory;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Component;

import com.jcraft.jsch.SftpException;

@Component
@Scope(value = ConfigurableBeanFactory.SCOPE_PROTOTYPE)
@Slf4j
public class Vo105FtpUploader extends GbaFtpConnection implements Vo105Uploader
{
	@Autowired
	private GbaConfig gbaConfig;

	@Autowired
	private FileService fileService;

	@Autowired
	private GbaVraagRepository gbaVraagRepository;

	@Autowired
	private ICurrentDateSupplier dateSupplier;

	private String vo105Bestand;

	private GbaVerwerkingsLog gbaVerwerkingsLog;

	private Long jobExecutionId;

	@Override
	public void initialize(String vo105Bestand, Long jobExecutionId, GbaVerwerkingsLog gbaVerwerkingsLog)
	{
		this.vo105Bestand = vo105Bestand;
		this.jobExecutionId = jobExecutionId;
		this.gbaVerwerkingsLog = gbaVerwerkingsLog;
	}

	@Override
	public void run()
	{
		run(gbaConfig, gbaVerwerkingsLog);
	}

	@Override
	protected void ftpActies() throws SftpException
	{
		prepareUpload();
		var file = new File(vo105Bestand);
		saveFileOnFileStore(file);
		upload(file);
		gbaVraagRepository.markeerVragenAlsVerstuurd(jobExecutionId.toString());
	}

	protected void prepareUpload() throws SftpException
	{
		getChannelSftp().cd(gbaConfig.gbaUploadFolder());
	}

	protected void upload(File file) throws SftpException
	{
		try (var inputStream = new FileInputStream(file))
		{
			getChannelSftp().put(inputStream, "VO105_BVO");
		}
		catch (IOException e)
		{
			throw new IllegalStateException("IOExeceptie tijdens upload Vo105", e);
		}
	}

	private void saveFileOnFileStore(File vo105File)
	{
		var directory = gbaConfig.voFileStorePath() + FileStoreLocation.relativePathForDate(dateSupplier.getLocalDate());
		var uniqueFileName = UUID.randomUUID() + "-outgoing.vo105";
		var fullPath = directory + File.separator + uniqueFileName;

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
}
