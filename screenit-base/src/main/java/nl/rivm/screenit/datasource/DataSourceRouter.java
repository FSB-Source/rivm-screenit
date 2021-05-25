package nl.rivm.screenit.datasource;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import com.zaxxer.hikari.HikariDataSource;
import javax.sql.DataSource;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.jdbc.datasource.lookup.AbstractRoutingDataSource;

public class DataSourceRouter extends AbstractRoutingDataSource
{

	private static final Logger LOG = LoggerFactory.getLogger(DataSourceRouter.class);

	private static final ThreadLocal<String> readOnly = new ThreadLocal<>();

	private static final String RW = "RW";

	private static final String RO = "RO";

	@Override
	protected Object determineCurrentLookupKey()
	{
		Object object;
		if (readOnly.get() == null)
		{
			object = RW;
		}
		else
		{
			object = readOnly.get();
		}
		LOG.trace("determineCurrentLookupKey " + object);
		return object;
	}

	public static void useReadOnly()
	{
		LOG.trace("useReadOnly");
		readOnly.set(RO);
	}

	public static void useReadWrite()
	{
		LOG.trace("useReadWrite");
		readOnly.set(RW);
	}

	public String getConnectionURL()
	{
		HikariDataSource dataSource = ((HikariDataSource) determineTargetDataSource());
		return dataSource.getJdbcUrl().replace("jdbc:", "").replace("://", "://" + dataSource.getUsername() + ":" + dataSource.getPassword() + "@");
	}

}
