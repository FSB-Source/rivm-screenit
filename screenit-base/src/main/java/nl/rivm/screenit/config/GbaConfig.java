package nl.rivm.screenit.config;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import lombok.Setter;

import org.apache.commons.lang3.StringUtils;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
@ConfigurationProperties(prefix = "gba")
@Setter
public class GbaConfig
{
	private static final Integer DEFAULT_SFTP_TIMEOUT = 2000;

	private static final Long DEFAULT_TRY_INTERVAL = 60000L;

	private static final Integer DEFAULT_MAX_NO_TRIES = 5;

	private String ftphost;

	private Integer ftpport;

	private String ftpusername;

	private String ftppassword;

	private String ftpknownhostfile;

	private String downloadfolder;

	private String uploadfolder;

	private String voFileStorePath;

	private Integer sftpConnectionTimeout;

	private Long tryInterval;

	private Integer maxNoTries;

	@Bean
	public String gbaFtpHost()
	{
		return StringUtils.defaultIfBlank(ftphost, "");
	}

	@Bean
	public Integer gbaFtpPort()
	{
		return ftpport != null ? ftpport : 0;
	}

	@Bean
	public String gbaFtpUsername()
	{
		return StringUtils.defaultIfBlank(ftpusername, "");
	}

	@Bean
	public String gbaFtpPassword()
	{
		return StringUtils.defaultIfBlank(ftppassword, "");
	}

	@Bean
	public String gbaFtpKnownHostFile()
	{
		return StringUtils.defaultIfBlank(ftpknownhostfile, "");
	}

	@Bean
	public String gbaDownloadFolder()
	{
		return StringUtils.defaultIfBlank(downloadfolder, "");
	}

	@Bean
	public String gbaUploadFolder()
	{
		return StringUtils.defaultIfBlank(uploadfolder, "");
	}

	@Bean
	public String voFileStorePath()
	{
		return StringUtils.defaultIfBlank(voFileStorePath, "");
	}

	@Bean
	public Integer sftpConnectionTimeout()
	{
		return sftpConnectionTimeout != null ? sftpConnectionTimeout : DEFAULT_SFTP_TIMEOUT;
	}

	@Bean
	public Long tryInterval()
	{
		return tryInterval != null ? tryInterval : DEFAULT_TRY_INTERVAL;
	}

	@Bean
	public Integer maxNoTries()
	{
		return maxNoTries != null ? maxNoTries : DEFAULT_MAX_NO_TRIES;
	}

}
