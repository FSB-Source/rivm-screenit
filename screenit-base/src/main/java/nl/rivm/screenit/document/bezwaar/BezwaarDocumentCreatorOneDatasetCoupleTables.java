package nl.rivm.screenit.document.bezwaar;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.sql.SQLException;
import java.util.List;

import nl.rivm.screenit.document.BaseDocumentCreator;
import nl.rivm.screenit.model.algemeen.BezwaarGroupViewWrapper;
import nl.rivm.screenit.model.algemeen.BezwaarViewWrapper;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BezwaarType;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.aspose.words.Document;
import com.aspose.words.MailMerge;
import com.aspose.words.MailMergeCleanupOptions;
import com.aspose.words.net.System.Data.DataRow;
import com.aspose.words.net.System.Data.DataSet;
import com.aspose.words.net.System.Data.DataTable;

public class BezwaarDocumentCreatorOneDatasetCoupleTables extends BaseDocumentCreator
{
	private static final Logger LOG = LoggerFactory.getLogger(BezwaarDocumentCreatorOneDatasetCoupleTables.class);

	private final DataSet bezwaar = new DataSet();

	private static final String TOELICHTING = "toelichting_";

	private static final String INACTIEF = "inactief";

	private static final String VOORVOEGSEL = "in_";

	private static final String ALGEMEEN = "algemeen";

	private static final String ALG_BEZWAAR = "alg_bezwaar";

	private static final String BEZWAAR_NAAM = "bezwaar_naam";

	private static final String ONDERZOEK_NAAM = "onderzoek_naam";

	private static final String SPECIFIEK = "specifiek";

	private static final String ONDERZOEK = "onderzoek";

	private static final String SPEC_BEZWAAR = "spec_bezwaar";

	private static final String GEEN_ACTIEVE_BEZWAREN = "geen_actief";

	private static final String ACTIEF = "actief";

	private boolean zijnErActieveBezwaren = false;

	public BezwaarDocumentCreatorOneDatasetCoupleTables(List<BezwaarGroupViewWrapper> groupWrappers)
	{
		try
		{
			initDataSet(groupWrappers);
		}
		catch (IllegalStateException | SQLException e)
		{
			LOG.error(e.getMessage(), e);
		}
	}

	private void initDataSet(List<BezwaarGroupViewWrapper> groupWrappers) throws SQLException
	{
		if (groupWrappers == null)
		{
			throw new IllegalStateException("BezwaarGroupViewWrappers mag niet null zijn!");
		}

		for (BezwaarGroupViewWrapper groupWrapper : groupWrappers)
		{
			for (BezwaarViewWrapper wrapper : groupWrapper.getBezwaren())
			{
				if (wrapper.getType() != BezwaarType.GEEN_REGISTRATIE_GEBOORTELAND) 
				{
					insertWrapper(groupWrapper.getBevolkingsonderzoek(), wrapper);
				}
			}
		}

		if (!zijnErActieveBezwaren)
		{
			insertGeenActieveBezwaren();
		}
	}

	private void insertWrapper(Bevolkingsonderzoek onderzoek, BezwaarViewWrapper wrapper) throws SQLException
	{
		LOG.debug(wrapper.getNaam());
		createToelichtingTable(onderzoek);
		if (wrapper.getActief())
		{
			zijnErActieveBezwaren = true;
			insertActieveWrapper(onderzoek, wrapper);
		}

		else if (!BezwaarType.GEEN_UITWISSELING_MET_DE_HUISARTS.equals(wrapper.getType()))
		{
			insertInactieveWrapper(onderzoek, wrapper);
		}
	}

	private void createToelichtingTable(Bevolkingsonderzoek onderzoek) throws SQLException
	{
		String tableNaam = TOELICHTING;
		if (onderzoek == null)
		{
			tableNaam += "alg";
		}
		else
		{
			tableNaam += onderzoek.getAfkorting().toLowerCase();
		}
		DataTable toelichtingTable = getOrCreateDataTable(bezwaar, tableNaam, tableNaam + "_id");
		if (toelichtingTable.getRows().getCount() < 1)
		{
			toelichtingTable.getRows().add(getNextSequence());
		}
	}

	private void insertInactieveWrapper(Bevolkingsonderzoek onderzoek, BezwaarViewWrapper wrapper) throws SQLException
	{
		DataTable inactiefTable = getDataTable(bezwaar, INACTIEF);
		if (inactiefTable == null)
		{
			inactiefTable = getOrCreateDataTable(bezwaar, INACTIEF, INACTIEF + "_id");
			insertRow(inactiefTable, getNextSequence());
		}

		if (onderzoek == null)
		{
			DataTable algemeenTable = getDataTable(bezwaar, VOORVOEGSEL + ALGEMEEN);
			if (algemeenTable == null)
			{
				DataRow inactiefRow = getFirstRow(inactiefTable);
				algemeenTable = getOrCreateDataTable(bezwaar, VOORVOEGSEL + ALGEMEEN, VOORVOEGSEL + ALGEMEEN + "_id", INACTIEF);
				insertRow(algemeenTable, getNextSequence(), inactiefRow.get(INACTIEF + "_id"));
				bezwaar.getRelations().add(inactiefTable, algemeenTable, INACTIEF + "_id", INACTIEF);
			}

			DataTable algBezwaarTable = getDataTable(bezwaar, VOORVOEGSEL + ALG_BEZWAAR);
			if (algBezwaarTable == null)
			{
				algBezwaarTable = getOrCreateDataTable(bezwaar, VOORVOEGSEL + ALG_BEZWAAR, VOORVOEGSEL + ALG_BEZWAAR + "_id", VOORVOEGSEL + ALGEMEEN, BEZWAAR_NAAM);
				bezwaar.getRelations().add(algemeenTable, algBezwaarTable, VOORVOEGSEL + ALGEMEEN + "_id", VOORVOEGSEL + ALGEMEEN);
			}

			DataRow row = getFirstRow(algemeenTable);
			insertRow(algBezwaarTable, getNextSequence(), row.get(VOORVOEGSEL + ALGEMEEN + "_id"), wrapper.getNaam());
		}
		else
		{
			DataTable specifiekTable = getDataTable(bezwaar, VOORVOEGSEL + SPECIFIEK);
			if (specifiekTable == null)
			{
				DataRow inactiefRow = getFirstRow(inactiefTable);
				specifiekTable = getOrCreateDataTable(bezwaar, VOORVOEGSEL + SPECIFIEK, VOORVOEGSEL + SPECIFIEK + "_id", INACTIEF);
				insertRow(specifiekTable, getNextSequence(), inactiefRow.get(INACTIEF + "_id"));
				bezwaar.getRelations().add(inactiefTable, specifiekTable, INACTIEF + "_id", INACTIEF);
			}
			DataRow specifiekRow = getFirstRow(specifiekTable);

			DataTable onderzoekTable = getDataTable(bezwaar, VOORVOEGSEL + ONDERZOEK);
			if (onderzoekTable == null)
			{
				onderzoekTable = getOrCreateDataTable(bezwaar, VOORVOEGSEL + ONDERZOEK, VOORVOEGSEL + ONDERZOEK + "_id", VOORVOEGSEL + SPECIFIEK, ONDERZOEK_NAAM);
				bezwaar.getRelations().add(specifiekTable, onderzoekTable, VOORVOEGSEL + SPECIFIEK + "_id", VOORVOEGSEL + SPECIFIEK);
			}

			DataRow onderzoekRow = getOrCreateOnderzoekRow(onderzoekTable, onderzoek, specifiekRow, VOORVOEGSEL + SPECIFIEK + "_id");
			DataTable specBezwaarTable = getDataTable(bezwaar, VOORVOEGSEL + SPEC_BEZWAAR);
			if (specBezwaarTable == null)
			{
				specBezwaarTable = getOrCreateDataTable(bezwaar, VOORVOEGSEL + SPEC_BEZWAAR, VOORVOEGSEL + SPEC_BEZWAAR + "_id", VOORVOEGSEL + ONDERZOEK, BEZWAAR_NAAM);
				bezwaar.getRelations().add(onderzoekTable, specBezwaarTable, VOORVOEGSEL + ONDERZOEK + "_id", VOORVOEGSEL + ONDERZOEK);
			}
			insertRow(specBezwaarTable, getNextSequence(), onderzoekRow.get(VOORVOEGSEL + ONDERZOEK + "_id"), wrapper.getNaam());
		}
	}

	private DataRow getOrCreateOnderzoekRow(DataTable onderzoekenTable, Bevolkingsonderzoek onderzoek, DataRow specifiekeTableRow, String relationColumnName) throws SQLException
	{
		for (DataRow onderzoekRow : onderzoekenTable.getRows())
		{
			if (onderzoekRow.get(ONDERZOEK_NAAM).equals(onderzoek.getNaam()))
			{
				return onderzoekRow;
			}
		}
		DataRow row = onderzoekenTable.newRow();
		row.set(0, getNextSequence());
		row.set(1, specifiekeTableRow.get(relationColumnName));
		row.set(2, onderzoek.getNaam());
		onderzoekenTable.getRows().add(row);
		return row;
	}

	private void insertActieveWrapper(Bevolkingsonderzoek onderzoek, BezwaarViewWrapper wrapper) throws SQLException
	{
		DataTable actiefTable = getDataTable(bezwaar, ACTIEF);
		if (actiefTable == null)
		{
			actiefTable = getOrCreateDataTable(bezwaar, ACTIEF, ACTIEF + "_id");
			insertRow(actiefTable, getNextSequence());
		}

		if (onderzoek == null)
		{
			DataTable algemeenTable = getDataTable(bezwaar, ALGEMEEN);
			if (algemeenTable == null)
			{
				DataRow inactiefRow = getFirstRow(actiefTable);
				algemeenTable = getOrCreateDataTable(bezwaar, ALGEMEEN, ALGEMEEN + "_id", ACTIEF);
				insertRow(algemeenTable, getNextSequence(), inactiefRow.get(ACTIEF + "_id"));
				bezwaar.getRelations().add(actiefTable, algemeenTable, ACTIEF + "_id", ACTIEF);
			}

			DataTable algBezwaarTable = getDataTable(bezwaar, ALG_BEZWAAR);
			if (algBezwaarTable == null)
			{
				algBezwaarTable = getOrCreateDataTable(bezwaar, ALG_BEZWAAR, ALG_BEZWAAR + "_id", ALGEMEEN, BEZWAAR_NAAM);
				bezwaar.getRelations().add(algemeenTable, algBezwaarTable, ALGEMEEN + "_id", ALGEMEEN);
			}

			DataRow row = getFirstRow(algemeenTable);
			insertRow(algBezwaarTable, getNextSequence(), row.get(ALGEMEEN + "_id"), wrapper.getNaam());
		}
		else
		{
			DataTable specifiekTable = getDataTable(bezwaar, SPECIFIEK);
			if (specifiekTable == null)
			{
				DataRow inactiefRow = getFirstRow(actiefTable);
				specifiekTable = getOrCreateDataTable(bezwaar, SPECIFIEK, SPECIFIEK + "_id", ACTIEF);
				insertRow(specifiekTable, getNextSequence(), inactiefRow.get(ACTIEF + "_id"));
				bezwaar.getRelations().add(actiefTable, specifiekTable, ACTIEF + "_id", ACTIEF);
			}
			DataRow specifiekRow = getFirstRow(specifiekTable);

			DataTable onderzoekTable = getDataTable(bezwaar, ONDERZOEK);
			if (onderzoekTable == null)
			{
				onderzoekTable = getOrCreateDataTable(bezwaar, ONDERZOEK, ONDERZOEK + "_id", SPECIFIEK, ONDERZOEK_NAAM);
				bezwaar.getRelations().add(specifiekTable, onderzoekTable, SPECIFIEK + "_id", SPECIFIEK);
			}

			DataRow onderzoekRow = getOrCreateOnderzoekRow(onderzoekTable, onderzoek, specifiekRow, SPECIFIEK + "_id");
			DataTable specBezwaarTable = getDataTable(bezwaar, SPEC_BEZWAAR);
			if (specBezwaarTable == null)
			{
				specBezwaarTable = getOrCreateDataTable(bezwaar, SPEC_BEZWAAR, SPEC_BEZWAAR + "_id", ONDERZOEK, BEZWAAR_NAAM);
				bezwaar.getRelations().add(onderzoekTable, specBezwaarTable, ONDERZOEK + "_id", ONDERZOEK);
			}
			insertRow(specBezwaarTable, getNextSequence(), onderzoekRow.get(ONDERZOEK + "_id"), wrapper.getNaam());
		}
	}

	private void insertGeenActieveBezwaren() throws SQLException
	{
		DataTable inactiefTable = getDataTable(bezwaar, GEEN_ACTIEVE_BEZWAREN);
		if (inactiefTable == null)
		{
			inactiefTable = getOrCreateDataTable(bezwaar, GEEN_ACTIEVE_BEZWAREN, GEEN_ACTIEVE_BEZWAREN + "_id");
			insertRow(inactiefTable, getNextSequence());
		}
	}

	@Override
	public Document fillExecuteWithRegions(Document document) throws Exception
	{
		log(LOG, bezwaar);
		MailMerge mailMerge = document.getMailMerge();
		mailMerge.setCleanupOptions(MailMergeCleanupOptions.REMOVE_UNUSED_REGIONS);
		mailMerge.executeWithRegions(bezwaar);
		return document;
	}

}
