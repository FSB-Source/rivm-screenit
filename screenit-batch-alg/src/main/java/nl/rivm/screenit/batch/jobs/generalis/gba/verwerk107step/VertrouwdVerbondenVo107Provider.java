package nl.rivm.screenit.batch.jobs.generalis.gba.verwerk107step;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-alg
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

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.batch.jobs.generalis.gba.GbaFtpConnection;
import nl.rivm.screenit.batch.jobs.generalis.gba.GbaFtpSettings;
import nl.rivm.screenit.model.gba.GbaVerwerkingsLog;

import org.apache.commons.io.IOUtils;

import com.google.common.collect.Collections2;
import com.jcraft.jsch.ChannelSftp.LsEntry;
import com.jcraft.jsch.SftpException;

@Getter
@Setter
@Slf4j
public class VertrouwdVerbondenVo107Provider implements IVo107Provider
{
	private String host;

	private int port;

	private String username;

	private String password;

	private String downloadPath;

	private String knownHostPath;

	@Override
	public List<Vo107File> getVo107Files(final GbaVerwerkingsLog gbaVerwerkingsLog)
	{
		final List<Vo107File> vo107Files = new ArrayList<>();

		var gbaFtpConnection = new GbaFtpConnection(maakFtpSettings(), gbaVerwerkingsLog)
		{
			@Override
			protected void ftpActies() throws SftpException
			{
				getChannelSftp().cd(downloadPath);

				List<LsEntry> files = getChannelSftp().ls(".");

				files = new ArrayList<>(Collections2.filter(files, input -> !input.getFilename().startsWith(".")));

				files.sort(Comparator.comparing(LsEntry::getFilename));

				for (LsEntry file : files)
				{
					SFTPVo107File vo107File = new SFTPVo107File(file.getFilename(), gbaVerwerkingsLog);
					vo107Files.add(vo107File);
				}
			}
		};

		gbaFtpConnection.run();

		return vo107Files;
	}

	private GbaFtpSettings maakFtpSettings()
	{
		var settings = new GbaFtpSettings();
		settings.setHost(host);
		settings.setPort(port);
		settings.setUsername(username);
		settings.setPassword(password);
		settings.setKnownHostPath(knownHostPath);
		return settings;
	}

	protected class SFTPVo107File implements Vo107File
	{
		private final String filename;

		private final GbaVerwerkingsLog gbaVerwerkingsLog;

		public SFTPVo107File(String filename, GbaVerwerkingsLog gbaVerwerkingsLog)
		{
			this.filename = filename;
			this.gbaVerwerkingsLog = gbaVerwerkingsLog;
		}

		@Override
		public void saveToFile(final File targetFile)
		{
			var gbaFtpConnection = new GbaFtpConnection(maakFtpSettings(), gbaVerwerkingsLog)
			{
				@Override
				protected void ftpActies() throws SftpException
				{
					getChannelSftp().cd(downloadPath);

					try (var fileInputStream = getChannelSftp().get(filename); FileOutputStream fileOutputStream = new FileOutputStream(targetFile))
					{
						IOUtils.copyLarge(fileInputStream, fileOutputStream);
					}
					catch (IOException e)
					{
						LOG.error("Error bij ophalen van vo107 bestand", e);
						createFoutRegel(gbaVerwerkingsLog, "Fout bij ophalen van vo107 betand");
						throw new IllegalStateException(e);
					}
				}
			};
			gbaFtpConnection.run();
		}

		@Override
		public String getFilename()
		{
			return filename;
		}

		@Override
		public void deleteFile()
		{
			var gbaFtpConnection = new GbaFtpConnection(maakFtpSettings(), gbaVerwerkingsLog)
			{
				@Override
				protected void ftpActies() throws SftpException
				{
					getChannelSftp().cd(downloadPath);
					getChannelSftp().rm(filename);
				}
			};
			gbaFtpConnection.run();
		}
	}
}
