package nl.rivm.screenit.mamma.se.proxy.configuration;

/*-
 * ========================LICENSE_START=================================
 * se-proxy
 * %%
 * Copyright (C) 2017 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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

import liquibase.database.Database;
import liquibase.exception.DatabaseException;
import liquibase.exception.LockException;
import liquibase.lockservice.DatabaseChangeLogLock;
import liquibase.lockservice.LockService;

@Slf4j
public class LiquibaseNoDbLockService implements LockService
{
	@Override
	public int getPriority()
	{
		return 1000;
	}

	@Override
	public boolean supports(Database database)
	{
		return true;
	}

	@Override
	public void setDatabase(Database database)
	{

	}

	@Override
	public void setChangeLogLockWaitTime(long changeLogLockWaitTime)
	{

	}

	@Override
	public void setChangeLogLockRecheckTime(long changeLogLocRecheckTime)
	{

	}

	@Override
	public boolean hasChangeLogLock()
	{
		return true;
	}

	@Override
	public void waitForLock() throws LockException
	{
		LOG.info("no waitForLock");
	}

	@Override
	public boolean acquireLock() throws LockException
	{
		return true;
	}

	@Override
	public void releaseLock() throws LockException
	{

	}

	@Override
	public DatabaseChangeLogLock[] listLocks() throws LockException
	{
		return new DatabaseChangeLogLock[0];
	}

	@Override
	public void forceReleaseLock() throws LockException, DatabaseException
	{

	}

	@Override
	public void reset()
	{

	}

	@Override
	public void init() throws DatabaseException
	{

	}

	@Override
	public void destroy() throws DatabaseException
	{

	}
}
