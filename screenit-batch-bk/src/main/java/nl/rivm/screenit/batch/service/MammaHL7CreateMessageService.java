package nl.rivm.screenit.batch.service;

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

import java.io.IOException;

import nl.rivm.screenit.dto.mamma.MammaHL7v24OrmBerichtTriggerMetClientDto;
import nl.rivm.screenit.dto.mamma.MammaHL7v24OrmBerichtTriggerMetKwaliteitsopnameDto;
import nl.rivm.screenit.model.Client;

import ca.uhn.hl7v2.HL7Exception;
import ca.uhn.hl7v2.model.v24.message.ADT_AXX;
import ca.uhn.hl7v2.model.v24.message.ORM_O01;

public interface MammaHL7CreateMessageService
{
	ORM_O01 maakClientORMBericht(MammaHL7v24OrmBerichtTriggerMetClientDto triggerDto, Client client) throws IOException, HL7Exception;

	ORM_O01 maakKwaliteitsopnameORMBericht(MammaHL7v24OrmBerichtTriggerMetKwaliteitsopnameDto hl7BerichtTrigger)
		throws IOException, HL7Exception;

	ADT_AXX maakADTBerichtPersoonsgegevensGewijzigd(Client client) throws IOException, HL7Exception;

	ADT_AXX maakADTBerichtGewijzigdBsn(Client client, String oudBsn, String nieuweBsn) throws IOException, HL7Exception;

}
