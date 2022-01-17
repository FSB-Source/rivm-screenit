package nl.rivm.screenit.batch.model.enums;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bk
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

import nl.rivm.screenit.batch.model.ScreenITHL7MessageContext;

public enum MammaHL7Connectie
{

	ORM("BK-ORM"),
	ADT("BK-ADT"),
	ORM_ILM("BK-ORM-ILM");

	private final String connectieNaam;

	private ScreenITHL7MessageContext messageContext;

	MammaHL7Connectie(String connectieNaam)
	{
		this.connectieNaam = connectieNaam;
	}

	public ScreenITHL7MessageContext getMessageContext()
	{
		return messageContext;
	}

	public void setMessageContext(ScreenITHL7MessageContext messageContext)
	{
		this.messageContext = messageContext;
	}

	public String getConnectieNaam()
	{
		return this.connectieNaam;
	}
}
