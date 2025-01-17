package nl.rivm.screenit.main.service.impl.zorgid;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.time.Clock;
import java.time.Duration;
import java.time.Instant;

public class ExpirableStateImpl implements ExpirableState
{

	private static final Duration DEFAULT_EXPIRE_TIME = Duration.ofMinutes(1);

	private static final Clock clock = Clock.systemUTC();

	private final Instant expireTime;

	protected ExpirableStateImpl()
	{
		this(DEFAULT_EXPIRE_TIME);
	}

	protected ExpirableStateImpl(Duration timeout)
	{
		this.expireTime = Instant.now(clock).plus(timeout);
	}

	@Override
	public boolean isExpired()
	{
		return Instant.now(clock).isAfter(expireTime);
	}
}
