package nl.rivm.screenit.service.colon.afmeldbrief;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.util.List;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.document.BaseDocumentCreator;

import com.aspose.words.Document;
import com.aspose.words.MailMergeCleanupOptions;
import com.aspose.words.net.System.Data.DataSet;

@Slf4j
public class ColonAfmeldDocumentCreator extends BaseDocumentCreator
{
	private static final String TIJDELIJK_AFMELDEN = "tijdelijk_afmelden";

	private static final String TIJDELIJK_AFMELDEN_JAARTALLEN = TIJDELIJK_AFMELDEN + "_jaartallen";

	private static final String AFMELDEN_JAARTAL = "afmelden_jaartal";

	private static final String ID_SUFFIX = "_id";

	private final List<Integer> tijdelijkAfmeldenJaartallen;

	private final DataSet tijdelijkAfmeldenDataSet = new DataSet();

	public ColonAfmeldDocumentCreator(List<Integer> tijdelijkAfmeldenJaartallen)
	{
		this.tijdelijkAfmeldenJaartallen = tijdelijkAfmeldenJaartallen;
		genereerDataSet();
	}

	private void genereerDataSet()
	{
		tijdelijkAfmeldenJaartallen.forEach(this::insertJaartalRij);
	}

	private void insertJaartalRij(Integer jaartal)
	{
		var tijdelijkAfmeldenWrapperTable = getDataTable(tijdelijkAfmeldenDataSet, TIJDELIJK_AFMELDEN);
		if (tijdelijkAfmeldenWrapperTable == null)
		{
			tijdelijkAfmeldenWrapperTable = getOrCreateDataTable(tijdelijkAfmeldenDataSet, TIJDELIJK_AFMELDEN, TIJDELIJK_AFMELDEN + ID_SUFFIX);
			insertRow(tijdelijkAfmeldenWrapperTable, getNextSequence());
		}

		var tijdelijkAfmeldenJaartallenTable = getDataTable(tijdelijkAfmeldenDataSet, TIJDELIJK_AFMELDEN_JAARTALLEN);
		if (tijdelijkAfmeldenJaartallenTable == null)
		{
			tijdelijkAfmeldenJaartallenTable = getOrCreateDataTable(tijdelijkAfmeldenDataSet, TIJDELIJK_AFMELDEN_JAARTALLEN, TIJDELIJK_AFMELDEN_JAARTALLEN + ID_SUFFIX,
				TIJDELIJK_AFMELDEN, AFMELDEN_JAARTAL);
			tijdelijkAfmeldenDataSet.getRelations().add(tijdelijkAfmeldenWrapperTable, tijdelijkAfmeldenJaartallenTable, TIJDELIJK_AFMELDEN + ID_SUFFIX, TIJDELIJK_AFMELDEN);
		}

		var rij = getFirstRow(tijdelijkAfmeldenWrapperTable);
		insertRow(tijdelijkAfmeldenJaartallenTable, getNextSequence(), rij.get(TIJDELIJK_AFMELDEN + ID_SUFFIX), jaartal);
	}

	@Override
	public Document fillExecuteWithRegions(Document document) throws Exception
	{
		log(LOG, tijdelijkAfmeldenDataSet);
		var mailMerge = document.getMailMerge();
		mailMerge.setCleanupOptions(MailMergeCleanupOptions.REMOVE_UNUSED_REGIONS);
		mailMerge.executeWithRegions(tijdelijkAfmeldenDataSet);
		return document;
	}
}
