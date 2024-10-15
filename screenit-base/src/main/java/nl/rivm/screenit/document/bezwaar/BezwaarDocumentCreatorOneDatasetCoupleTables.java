package nl.rivm.screenit.document.bezwaar;

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

import java.sql.SQLException;
import java.util.List;
import java.util.stream.Collectors;

import nl.rivm.screenit.document.BaseDocumentCreator;
import nl.rivm.screenit.model.algemeen.BezwaarGroupViewWrapper;
import nl.rivm.screenit.model.algemeen.BezwaarViewWrapper;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BezwaarType;
import nl.rivm.screenit.model.enums.BriefType;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.aspose.words.Document;
import com.aspose.words.MailMergeCleanupOptions;
import com.aspose.words.net.System.Data.DataRow;
import com.aspose.words.net.System.Data.DataSet;
import com.aspose.words.net.System.Data.DataTable;

public class BezwaarDocumentCreatorOneDatasetCoupleTables extends BaseDocumentCreator
{
	private static final Logger LOG = LoggerFactory.getLogger(BezwaarDocumentCreatorOneDatasetCoupleTables.class);

	private final DataSet bezwaar = new DataSet();

	private static final String TOELICHTING = "toelichting_";

	private static final String ID = "_id";

	private static final String INACTIEF = "inactief";

	private static final String VOORVOEGSEL = "in_";

	private static final String ALGEMEEN = "algemeen";

	private static final String ALG_BEZWAAR = "alg_bezwaar";

	private static final String BEZWAAR_NAAM = "bezwaar_naam";

	private static final String BEZWAAR_SUBTITEL = "bezwaar_subtitel";

	private static final String ONDERZOEK_NAAM = "onderzoek_naam";

	private static final String SPECIFIEK = "specifiek";

	private static final String ONDERZOEK = "onderzoek";

	private static final String BVO = "bvo";

	private static final String SPEC_BEZWAAR = "spec_bezwaar";

	private static final String GEEN_ACTIEVE_BEZWAREN = "geen_actief";

	private static final String ACTIEF = "actief";

	private boolean zijnErActieveBezwaren = false;

	public BezwaarDocumentCreatorOneDatasetCoupleTables(List<BezwaarGroupViewWrapper> groupWrappers, BriefType briefType)
	{
		try
		{
			initDataSet(groupWrappers, briefType);
		}
		catch (IllegalStateException | SQLException e)
		{
			LOG.error(e.getMessage(), e);
		}
	}

	private void initDataSet(List<BezwaarGroupViewWrapper> groupWrappers, BriefType briefType) throws SQLException
	{
		if (groupWrappers == null)
		{
			throw new IllegalStateException("BezwaarGroupViewWrappers mag niet null zijn!");
		}

		for (var groupWrapper : groupWrappers)
		{
			for (var wrapper : groupWrapper.getBezwaren().stream().filter(wrapper -> BezwaarType.GEEN_REGISTRATIE_GEBOORTELAND != wrapper.getType()).collect(Collectors.toList()))
			{
				if (BezwaarType.ALGEMENE_BEZWAAR_TYPES.contains(wrapper.getType()) && BriefType.CLIENT_BEZWAAR_BEVESTIGING_ALGEMEEN == briefType)
				{
					insertWrapper(null, wrapper);
				}
				else if (wrapper.getType() != BezwaarType.GEEN_OPNAME_UIT_BPR)
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
		LOG.debug(wrapper.getNaam(), wrapper.getSubtitel());
		createToelichtingTable(onderzoek);

		if (wrapper.getActief())
		{
			zijnErActieveBezwaren = true;
			if (wrapper.getType() == BezwaarType.VERZOEK_TOT_VERWIJDERING_DOSSIER)
			{
				insertOnderzoekWrapper(onderzoek);
			}
			else
			{
				insertActieveWrapper(onderzoek, wrapper);
			}
		}
		else if (!List.of(BezwaarType.GEEN_UITWISSELING_MET_DE_HUISARTS, BezwaarType.VERZOEK_TOT_VERWIJDERING_DOSSIER).contains(wrapper.getType()))
		{
			insertInactieveWrapper(onderzoek, wrapper);
		}
	}

	private void createToelichtingTable(Bevolkingsonderzoek onderzoek)
	{
		var tableNaam = TOELICHTING;
		if (onderzoek == null)
		{
			tableNaam += "alg";
		}
		else
		{
			tableNaam += onderzoek.getAfkorting().toLowerCase();
		}
		var toelichtingTable = getOrCreateDataTable(bezwaar, tableNaam, tableNaam + ID);
		if (toelichtingTable.getRows().getCount() < 1)
		{
			toelichtingTable.getRows().add(getNextSequence());
		}
	}

	private void insertInactieveWrapper(Bevolkingsonderzoek onderzoek, BezwaarViewWrapper wrapper) throws SQLException
	{
		var inactiefTable = getDataTable(bezwaar, INACTIEF);
		if (inactiefTable == null)
		{
			inactiefTable = getOrCreateDataTable(bezwaar, INACTIEF, INACTIEF + ID);
			insertRow(inactiefTable, getNextSequence());
		}

		if (onderzoek == null)
		{
			var algemeenTable = getDataTable(bezwaar, VOORVOEGSEL + ALGEMEEN);
			if (algemeenTable == null)
			{
				var inactiefRow = getFirstRow(inactiefTable);
				algemeenTable = getOrCreateDataTable(bezwaar, VOORVOEGSEL + ALGEMEEN, VOORVOEGSEL + ALGEMEEN + ID, INACTIEF);
				insertRow(algemeenTable, getNextSequence(), inactiefRow.get(INACTIEF + ID));
				bezwaar.getRelations().add(inactiefTable, algemeenTable, INACTIEF + ID, INACTIEF);
			}

			var algBezwaarTable = getDataTable(bezwaar, VOORVOEGSEL + ALG_BEZWAAR);
			if (algBezwaarTable == null)
			{
				algBezwaarTable = getOrCreateDataTable(bezwaar, VOORVOEGSEL + ALG_BEZWAAR, VOORVOEGSEL + ALG_BEZWAAR + ID, VOORVOEGSEL + ALGEMEEN, BEZWAAR_NAAM,
					BEZWAAR_SUBTITEL);
				bezwaar.getRelations().add(algemeenTable, algBezwaarTable, VOORVOEGSEL + ALGEMEEN + ID, VOORVOEGSEL + ALGEMEEN);
			}

			var row = getFirstRow(algemeenTable);
			insertRow(algBezwaarTable, getNextSequence(), row.get(VOORVOEGSEL + ALGEMEEN + ID), wrapper.getNaam(), wrapper.getSubtitel());
		}
		else
		{
			var specifiekTable = getDataTable(bezwaar, VOORVOEGSEL + SPECIFIEK);
			if (specifiekTable == null)
			{
				var inactiefRow = getFirstRow(inactiefTable);
				specifiekTable = getOrCreateDataTable(bezwaar, VOORVOEGSEL + SPECIFIEK, VOORVOEGSEL + SPECIFIEK + ID, INACTIEF);
				insertRow(specifiekTable, getNextSequence(), inactiefRow.get(INACTIEF + ID));
				bezwaar.getRelations().add(inactiefTable, specifiekTable, INACTIEF + ID, INACTIEF);
			}
			var specifiekRow = getFirstRow(specifiekTable);

			var onderzoekTable = getDataTable(bezwaar, VOORVOEGSEL + ONDERZOEK);
			if (onderzoekTable == null)
			{
				onderzoekTable = getOrCreateDataTable(bezwaar, VOORVOEGSEL + ONDERZOEK, VOORVOEGSEL + ONDERZOEK + ID, VOORVOEGSEL + SPECIFIEK, ONDERZOEK_NAAM);
				bezwaar.getRelations().add(specifiekTable, onderzoekTable, VOORVOEGSEL + SPECIFIEK + ID, VOORVOEGSEL + SPECIFIEK);
			}

			var onderzoekRow = getOrCreateOnderzoekRow(onderzoekTable, onderzoek, specifiekRow, VOORVOEGSEL + SPECIFIEK + ID);
			var specBezwaarTable = getDataTable(bezwaar, VOORVOEGSEL + SPEC_BEZWAAR);
			if (specBezwaarTable == null)
			{
				specBezwaarTable = getOrCreateDataTable(bezwaar, VOORVOEGSEL + SPEC_BEZWAAR, VOORVOEGSEL + SPEC_BEZWAAR + ID, VOORVOEGSEL + ONDERZOEK, BEZWAAR_NAAM,
					BEZWAAR_SUBTITEL);
				bezwaar.getRelations().add(onderzoekTable, specBezwaarTable, VOORVOEGSEL + ONDERZOEK + ID, VOORVOEGSEL + ONDERZOEK);
			}
			insertRow(specBezwaarTable, getNextSequence(), onderzoekRow.get(VOORVOEGSEL + ONDERZOEK + ID), wrapper.getNaam(), wrapper.getSubtitel());
		}
	}

	private DataRow getOrCreateOnderzoekRow(DataTable onderzoekenTable, Bevolkingsonderzoek onderzoek, DataRow specifiekeTableRow, String relationColumnName)
	{
		for (var onderzoekRow : onderzoekenTable.getRows())
		{
			if (onderzoekRow.get(ONDERZOEK_NAAM).equals(onderzoek.getNaam()))
			{
				return onderzoekRow;
			}
		}
		var row = onderzoekenTable.newRow();
		row.set(0, getNextSequence());
		row.set(1, specifiekeTableRow.get(relationColumnName));
		row.set(2, onderzoek.getNaam());
		onderzoekenTable.getRows().add(row);
		return row;
	}

	private void insertOnderzoekWrapper(Bevolkingsonderzoek onderzoek)
	{
		var onderzoekTable = getDataTable(bezwaar, BVO);
		if (onderzoekTable == null)
		{
			onderzoekTable = getOrCreateDataTable(bezwaar, BVO, BVO + ID, ONDERZOEK_NAAM);
		}
		insertRow(onderzoekTable, getNextSequence(), onderzoek.getNaam());
	}

	private void insertActieveWrapper(Bevolkingsonderzoek onderzoek, BezwaarViewWrapper wrapper)
	{
		var actiefTable = getDataTable(bezwaar, ACTIEF);
		if (actiefTable == null)
		{
			actiefTable = getOrCreateDataTable(bezwaar, ACTIEF, ACTIEF + ID);
			insertRow(actiefTable, getNextSequence());
		}

		if (onderzoek == null)
		{
			var algemeenTable = getDataTable(bezwaar, ALGEMEEN);
			if (algemeenTable == null)
			{
				var actiefRow = getFirstRow(actiefTable);
				algemeenTable = getOrCreateDataTable(bezwaar, ALGEMEEN, ALGEMEEN + ID, ACTIEF);
				insertRow(algemeenTable, getNextSequence(), actiefRow.get(ACTIEF + ID));
				bezwaar.getRelations().add(actiefTable, algemeenTable, ACTIEF + ID, ACTIEF);
			}

			var algBezwaarTable = getDataTable(bezwaar, ALG_BEZWAAR);
			if (algBezwaarTable == null)
			{
				algBezwaarTable = getOrCreateDataTable(bezwaar, ALG_BEZWAAR, ALG_BEZWAAR + ID, ALGEMEEN, BEZWAAR_NAAM, BEZWAAR_SUBTITEL);
				bezwaar.getRelations().add(algemeenTable, algBezwaarTable, ALGEMEEN + ID, ALGEMEEN);
			}

			var row = getFirstRow(algemeenTable);
			insertRow(algBezwaarTable, getNextSequence(), row.get(ALGEMEEN + ID), wrapper.getNaam(), wrapper.getSubtitel());
		}
		else
		{
			var specifiekTable = getDataTable(bezwaar, SPECIFIEK);
			if (specifiekTable == null)
			{
				var actiefRow = getFirstRow(actiefTable);
				specifiekTable = getOrCreateDataTable(bezwaar, SPECIFIEK, SPECIFIEK + ID, ACTIEF);
				insertRow(specifiekTable, getNextSequence(), actiefRow.get(ACTIEF + ID));
				bezwaar.getRelations().add(actiefTable, specifiekTable, ACTIEF + ID, ACTIEF);
			}
			var specifiekRow = getFirstRow(specifiekTable);

			var onderzoekTable = getDataTable(bezwaar, ONDERZOEK);
			if (onderzoekTable == null)
			{
				onderzoekTable = getOrCreateDataTable(bezwaar, ONDERZOEK, ONDERZOEK + ID, SPECIFIEK, ONDERZOEK_NAAM);
				bezwaar.getRelations().add(specifiekTable, onderzoekTable, SPECIFIEK + ID, SPECIFIEK);
			}

			var onderzoekRow = getOrCreateOnderzoekRow(onderzoekTable, onderzoek, specifiekRow, SPECIFIEK + ID);
			var specBezwaarTable = getDataTable(bezwaar, SPEC_BEZWAAR);
			if (specBezwaarTable == null)
			{
				specBezwaarTable = getOrCreateDataTable(bezwaar, SPEC_BEZWAAR, SPEC_BEZWAAR + ID, ONDERZOEK, BEZWAAR_NAAM, BEZWAAR_SUBTITEL);
				bezwaar.getRelations().add(onderzoekTable, specBezwaarTable, ONDERZOEK + ID, ONDERZOEK);
			}
			insertRow(specBezwaarTable, getNextSequence(), onderzoekRow.get(ONDERZOEK + ID), wrapper.getNaam(), wrapper.getSubtitel());
		}
	}

	private void insertGeenActieveBezwaren()
	{
		var actiefTable = getDataTable(bezwaar, GEEN_ACTIEVE_BEZWAREN);
		if (actiefTable == null)
		{
			actiefTable = getOrCreateDataTable(bezwaar, GEEN_ACTIEVE_BEZWAREN, GEEN_ACTIEVE_BEZWAREN + ID);
			insertRow(actiefTable, getNextSequence());
		}
	}

	@Override
	public Document fillExecuteWithRegions(Document document) throws Exception
	{
		log(LOG, bezwaar);
		var mailMerge = document.getMailMerge();
		mailMerge.setCleanupOptions(MailMergeCleanupOptions.REMOVE_UNUSED_REGIONS);
		mailMerge.executeWithRegions(bezwaar);
		return document;
	}

}
