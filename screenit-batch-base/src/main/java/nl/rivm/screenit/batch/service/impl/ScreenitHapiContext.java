package nl.rivm.screenit.batch.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-base
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

import nl.rivm.screenit.batch.model.HapiContextType;
import nl.rivm.screenit.hl7v3.cda.Charset;

import ca.uhn.hl7v2.DefaultHapiContext;
import ca.uhn.hl7v2.llp.MinLowerLayerProtocol;

public class ScreenitHapiContext
{
	private static DefaultHapiContext utf8Instance = null;

	private static DefaultHapiContext isoCharsetInstance = null;

	private ScreenitHapiContext()
	{
	}

	public static DefaultHapiContext getHapiContext(HapiContextType contextType)
	{
		switch (contextType)
		{
		case UTF_8:
			return getUTF8HapiContext();
		case ISO_8859_1:
			return getISOHapiContext();
		default:
			throw new IllegalStateException("Geen hl7 context type geselecteerd.");
		}
	}

	private static DefaultHapiContext getUTF8HapiContext()
	{
		if (utf8Instance == null)
		{
			utf8Instance = new DefaultHapiContext();
			MinLowerLayerProtocol mllp = new MinLowerLayerProtocol();
			mllp.setCharset(Charset.UTF_8.value());
			utf8Instance.setLowerLayerProtocol(mllp);

			utf8Instance.getExecutorService();
		}
		return utf8Instance;
	}

	private static DefaultHapiContext getISOHapiContext()
	{
		if (isoCharsetInstance == null)
		{
			isoCharsetInstance = new DefaultHapiContext();
			MinLowerLayerProtocol mllp = new MinLowerLayerProtocol();
			mllp.setCharset("ISO-8859-1");
			isoCharsetInstance.setLowerLayerProtocol(mllp);

			isoCharsetInstance.getExecutorService();
		}
		return isoCharsetInstance;
	}

}
