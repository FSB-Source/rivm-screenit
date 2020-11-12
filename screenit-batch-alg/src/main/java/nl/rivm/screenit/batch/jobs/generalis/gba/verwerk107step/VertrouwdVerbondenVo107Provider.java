
package nl.rivm.screenit.batch.jobs.generalis.gba.verwerk107step;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-alg
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

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import javax.annotation.Nullable;

import lombok.Getter;
import lombok.Setter;
import nl.rivm.screenit.model.gba.GbaFoutCategorie;
import nl.rivm.screenit.model.gba.GbaFoutRegel;
import nl.rivm.screenit.model.gba.GbaVerwerkingsLog;

import org.apache.commons.io.IOUtils;
import org.slf4j.LoggerFactory;

import com.google.common.base.Predicate;
import com.google.common.collect.Collections2;
import com.jcraft.jsch.Channel;
import com.jcraft.jsch.ChannelSftp;
import com.jcraft.jsch.ChannelSftp.LsEntry;
import com.jcraft.jsch.JSch;
import com.jcraft.jsch.JSchException;
import com.jcraft.jsch.Session;
import com.jcraft.jsch.SftpException;

@Getter
@Setter
public class VertrouwdVerbondenVo107Provider implements IVo107Provider
{

	private static final org.slf4j.Logger LOG = LoggerFactory.getLogger(VertrouwdVerbondenVo107Provider.class);

	private String host;

	private int port;

	private String username;

	private String password;

	private String downloadPath;

	private String knownHostPath;

	private int sftpConnectionTimeout;

	private long tryInterval;

	private int maxNoTries;

	@Override
	public List<Vo107File> getVo107Files(final GbaVerwerkingsLog gbaVerwerkingsLog)
	{
		final List<Vo107File> vo107Files = new ArrayList<>();

		new FtpConnection(gbaVerwerkingsLog)
		{
			@Override
			protected void ftpActies() throws SftpException
			{
				c.cd(downloadPath);

				List<LsEntry> files = c.ls(".");

				files = new ArrayList<>(Collections2.filter(files, new Predicate<LsEntry>()
				{
					@Override
					public boolean apply(@Nullable LsEntry input)
					{

						return !input.getFilename().startsWith(".");
					}
				}));

				Collections.sort(files, new Comparator<LsEntry>()
				{
					@Override
					public int compare(LsEntry o1, LsEntry o2)
					{
						return o1.getFilename().compareTo(o2.getFilename());
					}
				});

				for (LsEntry file : files)
				{
					SFTPVo107File vo107File = new SFTPVo107File(file.getFilename(), gbaVerwerkingsLog);
					vo107Files.add(vo107File);
				}
			}
		};

		return vo107Files;
	}

	private ChannelSftp openChannel(Session session) throws JSchException
	{
		Channel channel = session.openChannel("sftp");
		channel.connect();
		ChannelSftp c = (ChannelSftp) channel;
		return c;
	}

	private Session connect(JSch jsch) throws JSchException
	{
		jsch.setKnownHosts(knownHostPath);
		Session session = jsch.getSession(username, host, port);

		session.setPassword(password);
		session.connect();
		return session;
	}

	private void createFoutRegel(GbaVerwerkingsLog gbaVerwerkingsLog, String foutregel)
	{
		GbaFoutRegel gbaFoutRegel = new GbaFoutRegel();
		gbaFoutRegel.setFout(foutregel);
		gbaFoutRegel.setFoutCategorie(GbaFoutCategorie.PROCES);
		gbaFoutRegel.setVerwerkingsLog(gbaVerwerkingsLog);
		gbaVerwerkingsLog.getFouten().add(gbaFoutRegel);
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
			new FtpConnection(gbaVerwerkingsLog)
			{
				@Override
				protected void ftpActies() throws SftpException
				{
					c.cd(downloadPath);

					try (InputStream fileInputStream = c.get(filename); FileOutputStream fileOutputStream = new FileOutputStream(targetFile);)
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
		}

		@Override
		public String getFilename()
		{
			return filename;
		}

		@Override
		public void deleteFile()
		{
			new FtpConnection(gbaVerwerkingsLog)
			{
				@Override
				protected void ftpActies() throws SftpException
				{
					c.cd(downloadPath);
					c.rm(filename);
				}
			};
		}
	}

	private abstract class FtpConnection
	{
		protected ChannelSftp c;

		private FtpConnection(GbaVerwerkingsLog gbaVerwerkingsLog)
		{
			for (int tries = 1; tries <= maxNoTries; tries++)
			{
				Session session = null;
				try
				{
					JSch jsch = new JSch();
					session = connect(jsch);
					c = openChannel(session);

					ftpActies();
					break;
				}
				catch (JSchException e)
				{
					LOG.error("Error in connectie met server poging " + tries, e);
					createFoutRegel(gbaVerwerkingsLog, "Fout in de connectie met server poging " + tries + " van " + maxNoTries);
				}
				catch (SftpException e)
				{
					LOG.error("Error bij actie op de server poging " + tries, e);
					createFoutRegel(gbaVerwerkingsLog, "Fout bij actie op de server poging " + tries + " van " + maxNoTries);
				}
				finally
				{
					if (session != null)
					{
						session.disconnect();
					}
					if (c != null)
					{
						c.disconnect();
					}
				}

				try
				{
					Thread.sleep(tryInterval);
				}
				catch (InterruptedException e)
				{
					LOG.error("Exception bij sleep retry", e);
					createFoutRegel(gbaVerwerkingsLog, "Fout bij wachten retry");
				}
			}
		}

		protected abstract void ftpActies() throws SftpException;
	}

}
