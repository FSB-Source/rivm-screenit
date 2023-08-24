
package nl.rivm.screenit.batch;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-base
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

import lombok.extern.slf4j.Slf4j;

import com.jcraft.jsch.Logger;

@Slf4j
public class JschLogger implements Logger
{

	@Override
	public boolean isEnabled(int level)
	{
		switch (level)
		{
		case DEBUG:
			return LOG.isDebugEnabled();
		case INFO:
			return LOG.isInfoEnabled();
		case WARN:
			return LOG.isWarnEnabled();
		case ERROR:
		case FATAL:
			return LOG.isErrorEnabled();
		}

		return false;
	}

	@Override
	public void log(int level, String message)
	{
		switch (level)
		{
		case DEBUG:
			LOG.debug(message);
			break;
		case INFO:
			LOG.info(message);
			break;
		case WARN:
			LOG.warn(message);
			break;
		case ERROR:
		case FATAL:
			LOG.error(message);
			break;
		}
	}

}
