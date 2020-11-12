package nl.rivm.screenit.batch.service;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bk
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

import nl.rivm.screenit.batch.exception.HL7CreateMessageException;
import nl.rivm.screenit.batch.exception.HL7SendMessageException;
import nl.rivm.screenit.batch.model.enums.MammaHL7Connectie;
import nl.rivm.screenit.dto.mamma.MammaHL7v24AdtBerichtTriggerDto;
import nl.rivm.screenit.dto.mamma.MammaHL7v24OrmBerichtTriggerMetClientDto;
import nl.rivm.screenit.dto.mamma.MammaHL7v24OrmBerichtTriggerMetKwaliteitsopnameDto;

import ca.uhn.hl7v2.HL7Exception;
import nl.rivm.screenit.dto.mamma.MammaHL7v24OrmBerichtTriggerUploadBeeldenDto;

public interface MammaHL7v24SendService
{
	void sendADTBericht(MammaHL7v24AdtBerichtTriggerDto adtBerichtTriggerDto, MammaHL7Connectie messageContext)
		throws HL7CreateMessageException, HL7SendMessageException;

	void sendClientORMMessage(MammaHL7v24OrmBerichtTriggerMetClientDto hl7BerichtTrigger, MammaHL7Connectie ormConnectionContext)
		throws HL7CreateMessageException, HL7SendMessageException, HL7Exception;

	void verversConfiguratie();

    void sendUploadBeeldenORMMessage(MammaHL7v24OrmBerichtTriggerUploadBeeldenDto hl7BerichtTrigger, MammaHL7Connectie messageConnection)
            throws HL7CreateMessageException, HL7SendMessageException;

    void sendKwaliteitsopnameORMMessage(MammaHL7v24OrmBerichtTriggerMetKwaliteitsopnameDto hl7BerichtTrigger, MammaHL7Connectie messageContext)
		throws HL7CreateMessageException;
}
