package nl.rivm.screenit.batch.jobs.generalis.gba;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-alg
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

import lombok.AccessLevel;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.config.GbaConfig;
import nl.rivm.screenit.model.gba.GbaFoutCategorie;
import nl.rivm.screenit.model.gba.GbaFoutRegel;
import nl.rivm.screenit.model.gba.GbaVerwerkingsLog;

import org.apache.commons.lang.StringUtils;

import com.jcraft.jsch.Channel;
import com.jcraft.jsch.ChannelSftp;
import com.jcraft.jsch.JSch;
import com.jcraft.jsch.JSchException;
import com.jcraft.jsch.Session;
import com.jcraft.jsch.SftpException;

@Slf4j
public abstract class GbaFtpConnection
{
	@Getter(AccessLevel.PROTECTED)
	private ChannelSftp channelSftp;

	public void run(GbaConfig gbaConfig, GbaVerwerkingsLog gbaVerwerkingsLog)
	{
		Session session = null;
		try
		{
			session = connect(gbaConfig, new JSch());
			channelSftp = openChannel(session);
			ftpActies();
		}
		catch (JSchException e)
		{
			LOG.error("Error in connectie met server", e);
			createFoutRegel(gbaVerwerkingsLog, "Fout in de connectie met server");
		}
		catch (SftpException e)
		{
			LOG.error("Error bij actie op de server", e);
			createFoutRegel(gbaVerwerkingsLog, "Fout bij actie op de server");
		}
		finally
		{
			if (session != null)
			{
				session.disconnect();
			}
			if (channelSftp != null)
			{
				channelSftp.disconnect();
			}
		}
	}

	private Session connect(GbaConfig gbaConfig, JSch jsch) throws JSchException
	{
		boolean checkHostKey = Boolean.parseBoolean(System.getProperty("SFTP_CHECK_HOST_KEY"));
		if (checkHostKey)
		{
			jsch.setKnownHosts(gbaConfig.gbaFtpKnownHostFile());
		}
		Session session = jsch.getSession(gbaConfig.gbaFtpUsername(), gbaConfig.gbaFtpHost(), gbaConfig.gbaFtpPort());
		if (!checkHostKey)
		{
			java.util.Properties config = new java.util.Properties();
			config.put("StrictHostKeyChecking", "no");
			session.setConfig(config);
		}

		String serverHostKey = System.getProperty("SERVER_HOST_KEY");
		String keyfile = System.getProperty("PRIVATE_KEYFILE");
		if (StringUtils.isNotBlank(serverHostKey) && StringUtils.isNotBlank(keyfile))
		{
			session.setConfig("server_host_key", serverHostKey);
			jsch.addIdentity(keyfile);
		}
		session.setPassword(gbaConfig.gbaFtpPassword());
		session.connect();
		return session;
	}

	private ChannelSftp openChannel(Session session) throws JSchException
	{
		Channel channel = session.openChannel("sftp");
		channel.connect();
		return (ChannelSftp) channel;
	}

	protected void createFoutRegel(GbaVerwerkingsLog gbaVerwerkingsLog, String foutregel)
	{
		GbaFoutRegel gbaFoutRegel = new GbaFoutRegel();
		gbaFoutRegel.setFout(foutregel);
		gbaFoutRegel.setFoutCategorie(GbaFoutCategorie.PROCES);
		gbaFoutRegel.setVerwerkingsLog(gbaVerwerkingsLog);
		gbaVerwerkingsLog.getFouten().add(gbaFoutRegel);
	}

	protected abstract void ftpActies() throws SftpException;
}
