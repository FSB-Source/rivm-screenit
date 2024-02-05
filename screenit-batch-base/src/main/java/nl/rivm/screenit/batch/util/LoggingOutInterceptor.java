
package nl.rivm.screenit.batch.util;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-base
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

import org.apache.cxf.interceptor.LoggingMessage;

public class LoggingOutInterceptor extends org.apache.cxf.interceptor.LoggingOutInterceptor
{

	@Override
	protected String formatLoggingMessage(LoggingMessage buffer)
	{
		StringBuilder payload = buffer.getPayload();
		if (payload.indexOf("<UploadRequest") >= 0 && payload.indexOf("xml</DataType>") < 0 && payload.indexOf("XML</DataType>") < 0)
		{
			int uuidIndex = payload.indexOf("--uuid:");
			if (uuidIndex >= 0)
			{
				int endUuidIndex = payload.indexOf("\n", uuidIndex);
				String uuid = payload.substring(uuidIndex, endUuidIndex).trim();
				int end2deUuidIndex = payload.indexOf(uuid, endUuidIndex);
				payload.setLength(end2deUuidIndex + uuid.length());
				payload.append("\n-- binary non XML data skipped for ScreenIT --\n");
				payload.append(uuid).append("\n");
			}
		}
		return super.formatLoggingMessage(buffer);
	}

}
