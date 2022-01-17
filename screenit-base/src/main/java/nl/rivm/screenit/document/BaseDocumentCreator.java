package nl.rivm.screenit.document;

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

import org.slf4j.Logger;

import com.aspose.words.Document;
import com.aspose.words.net.System.Data.DataColumn;
import com.aspose.words.net.System.Data.DataColumnCollection;
import com.aspose.words.net.System.Data.DataRelation;
import com.aspose.words.net.System.Data.DataRelationCollection;
import com.aspose.words.net.System.Data.DataRow;
import com.aspose.words.net.System.Data.DataRowCollection;
import com.aspose.words.net.System.Data.DataSet;
import com.aspose.words.net.System.Data.DataTable;
import com.aspose.words.net.System.Data.DataTableCollection;

public abstract class BaseDocumentCreator implements DocumentCreator
{
	private int sequence = 0;

	public BaseDocumentCreator()
	{
	}

	protected DataTable getDataTable(DataSet dataSet, String tableNaam)
	{
		return dataSet.getTables().get(tableNaam);
	}

	protected DataTable getOrCreateDataTable(DataSet dataSet, String tableNaam, String... columns)
	{
		DataTable table = getDataTable(dataSet, tableNaam);
		if (table == null && columns.length != 0)
		{
			table = new DataTable(tableNaam);
			for (String columnNaam : columns)
			{
				table.getColumns().add(columnNaam);
			}
			dataSet.getTables().add(table);
		}
		return table;
	}

	protected DataRow insertRow(DataTable table, Object... values)
	{
		DataRow row = table.newRow();
		for (int i = 0; i < values.length; i++)
		{
			row.set(i, values[i]);
		}
		table.getRows().add(row);
		return row;
	}

	protected DataRow getFirstRow(DataTable table)
	{
		return table.getRows().get(0);
	}

	protected int getNextSequence()
	{
		sequence = sequence + 1;
		return sequence;
	}

	@Override
	public abstract Document fillExecuteWithRegions(Document document) throws Exception;

	protected void log(Logger log, DataSet dataset)
	{
		try
		{
			log(log, dataset.getTables());
			log(log, dataset.getRelations());
		}
		catch (SQLException e)
		{
			log.error("Probleem met het loggen van de dataset");
		}
	}

	protected void log(Logger log, DataRelationCollection relationCollection)
	{
		log.debug("RELATIONS:");
		for (DataRelation dataRelation : relationCollection)
		{
			log(log, dataRelation);
		}
	}

	protected void log(Logger log, DataRelation relation)
	{
		String relations = relation.getParentTableName() + "." + relation.getParentColumnNames()[0];
		relations = relations + " -> " + relation.getChildTableName() + "." + relation.getChildColumnNames()[0];
		log.debug(relations);
	}

	protected void log(Logger log, DataTableCollection tableCollection) throws SQLException
	{
		log.debug("TABLES:");
		for (DataTable table : tableCollection)
		{
			log.debug("-----");
			log(log, table);
		}
	}

	protected void log(Logger log, DataTable table) throws SQLException
	{
		log.debug("Table naam: " + table.getTableName());
		log(log, table.getColumns());
		log(log, table.getRows());
	}

	protected void log(Logger log, DataColumnCollection columnCollection)
	{
		log.debug("COLUMNS:");
		String columns = "";
		for (DataColumn dataColumn : columnCollection)
		{
			columns = columns + dataColumn.getColumnName() + " | ";
		}
		log.debug(columns);
	}

	protected void log(Logger log, DataRowCollection rowCollection) throws SQLException
	{
		log.debug("DATA:");
		for (DataRow dataRow : rowCollection)
		{
			log(log, dataRow);
		}
	}

	protected void log(Logger log, DataRow dataRow) throws SQLException
	{
		String values = "";
		for (int i = 0; i < dataRow.getTable().getColumnsCount(); i++)
		{
			Object value = dataRow.get(i);
			values = values + value + " | ";
		}
		log.debug(values);
	}

}
