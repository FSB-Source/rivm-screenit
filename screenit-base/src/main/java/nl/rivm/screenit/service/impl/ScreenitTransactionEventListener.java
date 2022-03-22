package nl.rivm.screenit.service.impl;

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

import nl.rivm.screenit.model.envers.RevisionKenmerk;
import nl.rivm.screenit.model.envers.RevisionKenmerkInThreadHolder;

import org.springframework.stereotype.Component;
import org.springframework.transaction.event.TransactionPhase;
import org.springframework.transaction.event.TransactionalEventListener;

@Component
public class ScreenitTransactionEventListener
{

	@TransactionalEventListener(phase = TransactionPhase.AFTER_COMPLETION) 
	public void resetRevisionKenmerkAfterTransaction(RevisionKenmerk revisionKenmerk)
	{
		RevisionKenmerkInThreadHolder.resetKenmerk();
	}
}
