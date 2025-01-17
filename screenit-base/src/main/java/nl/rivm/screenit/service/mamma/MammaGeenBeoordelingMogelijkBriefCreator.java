package nl.rivm.screenit.service.mamma;

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

import nl.rivm.screenit.document.BaseDocumentCreator;

import com.aspose.words.Document;
import com.aspose.words.MailMerge;
import com.aspose.words.MailMergeCleanupOptions;
import com.aspose.words.net.System.Data.DataSet;

public class MammaGeenBeoordelingMogelijkBriefCreator extends BaseDocumentCreator
{
	private DataSet dataSet = new DataSet();

	public MammaGeenBeoordelingMogelijkBriefCreator()
	{
	}

	@Override
	public Document fillExecuteWithRegions(Document document) throws Exception
	{
		MailMerge mailMerge = document.getMailMerge();
		mailMerge.setCleanupOptions(MailMergeCleanupOptions.REMOVE_UNUSED_REGIONS);
		mailMerge.executeWithRegions(dataSet);
		return document;
	}
}
