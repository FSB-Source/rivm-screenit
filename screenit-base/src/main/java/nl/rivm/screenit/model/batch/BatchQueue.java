
package nl.rivm.screenit.model.batch;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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

public final class BatchQueue
{
	private BatchQueue()
	{

	}

	public static final String OVERALL_LOCK = "batch_lock";

	private static final String CONTEXT_COLUMN = "context";

	private static final String TYPE_COLUMN = "type";

	private static final String ORDER_ID_COLUMN = "ORDER_ID";

	private static final String TABLE_BATCH_QUEUE = "gedeeld.batch_queue";

	public static final String CONTEX_PARAM = CONTEXT_COLUMN;

	public static final String TYPE_PARAM = TYPE_COLUMN;

	public static final String ID_PARAM = "id";

	public static final String TABLE_CREATE_BATCH_QUEUE = "CREATE TABLE " + TABLE_BATCH_QUEUE + " (" 
		+ TYPE_COLUMN + " VARCHAR(100) NOT NULL," 
		+ CONTEXT_COLUMN + " TEXT NOT NULL," 
		+ ORDER_ID_COLUMN + " BIGINT  NOT NULL PRIMARY KEY" 
		+ ");";

	public static final String SQL_INSERT_JOB = "insert into " + TABLE_BATCH_QUEUE + " (" + ORDER_ID_COLUMN + ", " + TYPE_COLUMN + ", " + CONTEXT_COLUMN
		+ ") select nextval('hibernate_sequence'), :" + TYPE_PARAM + ", :" + CONTEX_PARAM + " ;";

	public static final String SQL_POLL_BATCH = "delete from " + TABLE_BATCH_QUEUE + " where " + ORDER_ID_COLUMN + " = :" + ID_PARAM + " ;";

	public static final String SQL_INNER_PEEK_BATCH = "select min(" + ORDER_ID_COLUMN + ") from " + TABLE_BATCH_QUEUE + " WHERE " + TYPE_COLUMN + " = :" + TYPE_PARAM;

	public static final String SQL_PEEK_BATCH1 = "select " + TYPE_COLUMN + ", " + CONTEXT_COLUMN + " from " + TABLE_BATCH_QUEUE + " where " + ORDER_ID_COLUMN + " = :" + ID_PARAM
		+ " ;";

	public static final String SQL_GET_QUEUE = "select " + TYPE_COLUMN + " from " + TABLE_BATCH_QUEUE + " order by " + ORDER_ID_COLUMN + " asc";
}
