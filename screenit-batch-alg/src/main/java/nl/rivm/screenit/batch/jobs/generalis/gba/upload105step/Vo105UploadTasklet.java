
package nl.rivm.screenit.batch.jobs.generalis.gba.upload105step;

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
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Calendar;
import java.util.Map;
import java.util.UUID;

import nl.rivm.screenit.batch.dao.GbaDao;
import nl.rivm.screenit.batch.jobs.generalis.gba.GbaConstants;
import nl.rivm.screenit.batch.jobs.generalis.gba.verwerk105step.Vo105ItemWriter;
import nl.rivm.screenit.batch.service.impl.GbaServiceImpl;
import nl.rivm.screenit.batch.util.SSHProxy;
import nl.rivm.screenit.model.gba.GbaFoutCategorie;
import nl.rivm.screenit.model.gba.GbaFoutRegel;
import nl.rivm.screenit.model.gba.GbaVerwerkingsLog;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang.StringUtils;
import org.slf4j.LoggerFactory;
import org.springframework.batch.core.ExitStatus;
import org.springframework.batch.core.StepContribution;
import org.springframework.batch.core.StepExecution;
import org.springframework.batch.core.StepExecutionListener;
import org.springframework.batch.core.scope.context.ChunkContext;
import org.springframework.batch.core.step.tasklet.Tasklet;
import org.springframework.batch.repeat.RepeatStatus;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.jcraft.jsch.Channel;
import com.jcraft.jsch.ChannelSftp;
import com.jcraft.jsch.JSch;
import com.jcraft.jsch.JSchException;
import com.jcraft.jsch.Session;
import com.jcraft.jsch.SftpException;

public class Vo105UploadTasklet implements Tasklet, StepExecutionListener
{

	private static final org.slf4j.Logger LOG = LoggerFactory.getLogger(GbaServiceImpl.class);

	@Autowired
	private GbaDao gbaDao;

	private String host;

	private int port;

	private String username;

	private String password;

	private String proxyHost;

	private int proxyPort;

	private String proxyUsername;

	private String proxyPassword;

	private String uploadPath;

	private String knownHostPath;

	private String voFileStorePath;

	private int sftpConnectionTimeout = 0;

	private StepExecution stepExecution;

	private long tryInterval;

	private int maxNoTries;

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public RepeatStatus execute(StepContribution contribution, ChunkContext chunkContext) throws FileNotFoundException
	{
		GbaVerwerkingsLog verwerkingsLog = (GbaVerwerkingsLog) chunkContext.getStepContext().getJobExecutionContext().get(GbaConstants.RAPPORTAGEKEYGBA);

		final Map<String, String> vo105Bestanden = (Map<String, String>) chunkContext.getStepContext().getJobExecutionContext().get(Vo105ItemWriter.BESTANDEN_MAP_KEY);

		new FtpConnection(verwerkingsLog)
		{

			@Override
			protected void ftpActies() throws SftpException, FileNotFoundException
			{
				c.cd(uploadPath);

				for (String regioCode : vo105Bestanden.keySet())
				{
					File file = new File(vo105Bestanden.get(regioCode));
					saveFile(file, regioCode);
					c.put(new FileInputStream(file), "VO105_" + regioCode);
				}

				gbaDao.updateVerstuurdeVragen(stepExecution.getJobExecution().getId().toString());
			}
		};

		for (String filePath : vo105Bestanden.values())
		{
			new File(filePath).delete();
		}

		return RepeatStatus.FINISHED;
	}

	private void createFoutRegel(GbaVerwerkingsLog gbaVerwerkingsLog, String foutregel)
	{
		GbaFoutRegel gbaFoutRegel = new GbaFoutRegel();
		gbaFoutRegel.setFout(foutregel);
		gbaFoutRegel.setFoutCategorie(GbaFoutCategorie.PROCES);
		gbaFoutRegel.setVerwerkingsLog(gbaVerwerkingsLog);
		gbaVerwerkingsLog.getFouten().add(gbaFoutRegel);
	}

	private void saveFile(File vo105File, String regioCode)
	{
		StringBuilder directory = new StringBuilder();

		directory.append(voFileStorePath);

		Calendar cal = Calendar.getInstance();
		directory.append(System.getProperty("file.separator"));
		directory.append(cal.get(Calendar.YEAR));
		directory.append(System.getProperty("file.separator"));
		directory.append(cal.get(Calendar.MONTH) + 1);
		directory.append(System.getProperty("file.separator"));
		directory.append(cal.get(Calendar.DAY_OF_MONTH));
		directory.append(System.getProperty("file.separator"));

		String uniqueFileName = UUID.randomUUID().toString();
		File dir = new File(directory.toString());
		dir.mkdirs();
		File file = new File(directory.toString() + System.getProperty("file.separator") + uniqueFileName + "-" + regioCode + "-outgoing.vo105");

		LOG.info("Saved file: " + file.getPath());

		try
		{
			FileUtils.copyFile(vo105File, file);
		}
		catch (IOException e)
		{
			LOG.error("Error bij opslaan van vo105 bestand", e);
		}
	}

	private Session connectGateway(JSch jsch) throws JSchException
	{
		jsch.setKnownHosts(knownHostPath);
		Session session = jsch.getSession(proxyUsername, proxyHost, proxyPort);
		session.setPassword(proxyPassword);
		session.connect(sftpConnectionTimeout);
		return session;
	}

	private Session connect(JSch jsch, Session gateway) throws JSchException
	{
		boolean checkHostKey = Boolean.parseBoolean(System.getProperty("SFTP_CHECK_HOST_KEY"));
		if (checkHostKey)
		{
			jsch.setKnownHosts(knownHostPath);
		}
		Session session = jsch.getSession(username, host, port);
		if (!checkHostKey)
		{
			java.util.Properties config = new java.util.Properties();
			config.put("StrictHostKeyChecking", "no");
			session.setConfig(config);
		}

		if (gateway != null)
		{
			session.setProxy(new SSHProxy(gateway));
		}

		String server_host_key = System.getProperty("SERVER_HOST_KEY");
		String keyfile = System.getProperty("PRIVATE_KEYFILE");
		if (StringUtils.isNotBlank(server_host_key) && StringUtils.isNotBlank(keyfile))
		{
			LOG.info("SERVER_HOST_KEY is set to " + server_host_key);
			session.setConfig("server_host_key", server_host_key);
			jsch.addIdentity(keyfile);
		}
		session.setPassword(password);
		session.connect();
		return session;
	}

	private ChannelSftp openChannel(Session session) throws JSchException
	{
		Channel channel = session.openChannel("sftp");
		channel.connect();
		ChannelSftp c = (ChannelSftp) channel;
		return c;
	}

	public void setHost(String host)
	{
		this.host = host;
	}

	public void setPort(int port)
	{
		this.port = port;
	}

	public void setUsername(String username)
	{
		this.username = username;
	}

	public void setPassword(String password)
	{
		this.password = password;
	}

	public void setUploadPath(String uploadPath)
	{
		this.uploadPath = uploadPath;
	}

	public void setKnownHostPath(String knownHostPath)
	{
		this.knownHostPath = knownHostPath;
	}

	public void setVoFileStorePath(String voFileStorePath)
	{
		this.voFileStorePath = voFileStorePath;
	}

	public void setSftpConnectionTimeout(int sftpConnectionTimeout)
	{
		this.sftpConnectionTimeout = sftpConnectionTimeout;
	}

	public void setProxyHost(String proxyHost)
	{
		this.proxyHost = proxyHost;
	}

	public void setProxyPort(int proxyPort)
	{
		this.proxyPort = proxyPort;
	}

	public void setProxyUsername(String proxyUsername)
	{
		this.proxyUsername = proxyUsername;
	}

	public void setProxyPassword(String proxyPassword)
	{
		this.proxyPassword = proxyPassword;
	}

	public void setMaxNoTries(int maxNoTries)
	{
		this.maxNoTries = maxNoTries;
	}

	public void setTryInterval(long tryInterval)
	{
		this.tryInterval = tryInterval;
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

	private abstract class FtpConnection
	{
		protected ChannelSftp c;

		private FtpConnection(GbaVerwerkingsLog gbaVerwerkingsLog) throws FileNotFoundException
		{
			for (int tries = 1; tries <= maxNoTries; tries++)
			{
				Session session = null;
				Session gateWaySession = null;
				try
				{
					JSch jsch = new JSch();
					if (StringUtils.isNotBlank(proxyHost))
					{
						gateWaySession = connectGateway(jsch);
					}
					session = connect(jsch, gateWaySession);
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
					if (gateWaySession != null)
					{
						gateWaySession.disconnect();
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

		protected abstract void ftpActies() throws SftpException, FileNotFoundException;
	}
}
