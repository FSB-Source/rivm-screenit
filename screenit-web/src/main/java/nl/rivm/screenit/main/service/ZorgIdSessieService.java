package nl.rivm.screenit.main.service;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.util.UUID;

import nl.rivm.screenit.main.service.impl.zorgid.ClosedSessieState;
import nl.rivm.screenit.main.service.impl.zorgid.InitializedSessieState;
import nl.rivm.screenit.main.service.impl.zorgid.OpenCancelledSessieState;
import nl.rivm.screenit.main.service.impl.zorgid.OpenedSessieState;
import nl.rivm.screenit.main.service.impl.zorgid.SessieState;
import nl.topicuszorg.cloud.distributedsessions.RedisConfig;
import nl.topicuszorg.zorgid.model.ZorgidException;
import nl.topicuszorg.zorgid.model.sessie.ClosedReason;
import nl.topicuszorg.zorgid.model.sessie.OpenCancelledReason;
import nl.topicuszorg.zorgid.webservice.SessionInitializedEvent;

public interface ZorgIdSessieService
{
	UUID startSessie() throws ZorgidException;

	SessieState getSessieState(UUID uuid);

	void stopSessie(UUID uuid) throws ZorgidException;

	void refreshSessie(UUID uuid);

	default boolean isSessieGeinitialiseerd(SessieState sessieState)
	{
		return sessieState instanceof InitializedSessieState;
	}

	default OpenCancelledReason getSessionOpenCancelledReason(SessieState sessieState)
	{
		if (sessieState instanceof OpenCancelledSessieState)
		{
			return ((OpenCancelledSessieState) sessieState).getReason();
		}
		else
		{
			return null;
		}
	}

	default boolean isSessieOpen(SessieState sessieState)
	{
		return sessieState instanceof OpenedSessieState;
	}

	default ClosedReason getSessionClosedReason(SessieState sessieState)
	{
		if (sessieState instanceof ClosedSessieState)
		{
			return ((ClosedSessieState) sessieState).getReason();
		}
		else
		{
			return null;
		}
	}

	default boolean isUziPasVerwijderdDoorGebruiker(SessieState sessieState)
	{
		return ClosedReason.SIGNER_REQUESTED == getSessionClosedReason(sessieState);
	}

	void onSessionInitialized(SessionInitializedEvent sessionInitializedEvent);

	void onSessionOpened(UUID uuid, String secureElement);

	void onSessionOpenCancelled(UUID uuid, OpenCancelledReason reason);

	void onSessionClosed(UUID uuid, ClosedReason reason);

	void setRedisConfig(RedisConfig redisConfig);

}
