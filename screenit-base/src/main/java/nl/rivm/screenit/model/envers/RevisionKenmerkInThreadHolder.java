package nl.rivm.screenit.model.envers;

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

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import org.springframework.context.ApplicationEventPublisher;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class RevisionKenmerkInThreadHolder
{
	private static final ThreadLocal<RevisionKenmerk> revisionKenmerkThreadLocal = new ThreadLocal<>();

	public static RevisionKenmerk getKenmerk()
	{
		return revisionKenmerkThreadLocal.get();
	}

	public static void setKenmerk(RevisionKenmerk kenmerk, ApplicationEventPublisher applicationEventPublisher)
	{
		if (kenmerk == null)
		{
			resetKenmerk();
		}
		else
		{
			applicationEventPublisher.publishEvent(RevisionKenmerk.DOSSIERVERWIJDERING_DOOR_BEZWAAR);
			revisionKenmerkThreadLocal.set(kenmerk);
		}
	}

	public static void resetKenmerk()
	{
		revisionKenmerkThreadLocal.remove();
	}

}
