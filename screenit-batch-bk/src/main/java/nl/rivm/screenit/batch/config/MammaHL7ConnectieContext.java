package nl.rivm.screenit.batch.config;

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

import static nl.rivm.screenit.model.mamma.enums.MammaHL7BerichtType.IMS_ADT;
import static nl.rivm.screenit.model.mamma.enums.MammaHL7BerichtType.IMS_ORM;
import static nl.rivm.screenit.model.mamma.enums.MammaHL7BerichtType.IMS_ORM_ILM;
import static nl.rivm.screenit.model.mamma.enums.MammaHL7BerichtType.IMS_ORM_ILM_UPLOAD_BEELDEN;
import static nl.rivm.screenit.model.mamma.enums.MammaHL7BerichtType.IMS_ORM_KWALITEITSOPNAME;
import static nl.rivm.screenit.model.mamma.enums.MammaHL7BerichtType.IMS_ORM_UPLOAD_BEELDEN;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import nl.rivm.screenit.batch.model.HapiContextType;
import nl.rivm.screenit.batch.model.ScreenITHL7MessageContext;
import nl.rivm.screenit.batch.model.enums.MammaHL7Connectie;
import nl.rivm.screenit.dto.mamma.MammaAbstractHL7v24OrmBerichtTriggerDto;
import nl.rivm.screenit.dto.mamma.MammaHL7v24OrmBerichtTriggerIlmDto;
import nl.rivm.screenit.dto.mamma.MammaHL7v24OrmBerichtTriggerMetClientDto;
import nl.rivm.screenit.model.mamma.MammaHL7v24Message;
import nl.rivm.screenit.model.mamma.enums.MammaHL7BerichtType;
import nl.rivm.screenit.model.mamma.enums.MammaHL7v24ORMBerichtStatus;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;

public class MammaHL7ConnectieContext
{
	private final ObjectMapper objectMapper = new ObjectMapper();

	private final Map<MammaHL7BerichtType, MammaHL7Connectie> connectieMap = new HashMap<>();

	public MammaHL7ConnectieContext()
	{
		connectieMap.put(IMS_ORM, MammaHL7Connectie.ORM);
		connectieMap.put(IMS_ORM_UPLOAD_BEELDEN, MammaHL7Connectie.ORM);
		connectieMap.put(IMS_ORM_ILM_UPLOAD_BEELDEN, MammaHL7Connectie.ORM_ILM);
		connectieMap.put(IMS_ORM_KWALITEITSOPNAME, MammaHL7Connectie.ORM);
		connectieMap.put(IMS_ORM_ILM, MammaHL7Connectie.ORM_ILM);
		connectieMap.put(IMS_ADT, MammaHL7Connectie.ADT);

		connectieMap.get(IMS_ORM).setMessageContext(new ScreenITHL7MessageContext(HapiContextType.ISO_8859_1));
		connectieMap.get(IMS_ORM_ILM).setMessageContext(new ScreenITHL7MessageContext(HapiContextType.ISO_8859_1));
		connectieMap.get(IMS_ADT).setMessageContext(new ScreenITHL7MessageContext(HapiContextType.ISO_8859_1));
	}

	public MammaHL7Connectie getConnectie(MammaHL7v24Message message) throws JsonProcessingException
	{
		MammaHL7BerichtType hl7BerichtType = message.getHl7BerichtType();
		MammaAbstractHL7v24OrmBerichtTriggerDto ilmTriggerDto = getIlmTriggerDto(message);

		return ilmTriggerDto != null && MammaHL7v24ORMBerichtStatus.GOINGTODELETE == ilmTriggerDto.getStatus() ? connectieMap.get(IMS_ORM) : connectieMap.get(hl7BerichtType);
	}

	private MammaAbstractHL7v24OrmBerichtTriggerDto getIlmTriggerDto(MammaHL7v24Message message)
		throws JsonProcessingException
	{
		MammaAbstractHL7v24OrmBerichtTriggerDto triggerDto = null;
		if (message.getHl7BerichtType() == IMS_ORM_ILM)
		{
			triggerDto = objectMapper.readValue(message.getDtoJson(), MammaHL7v24OrmBerichtTriggerMetClientDto.class);
		}
		else if (message.getHl7BerichtType() == IMS_ORM_ILM_UPLOAD_BEELDEN)
		{
			triggerDto = objectMapper.readValue(message.getDtoJson(), MammaHL7v24OrmBerichtTriggerIlmDto.class);
		}
		return triggerDto;
	}

	public Collection<MammaHL7Connectie> getConnecties()
	{
		return connectieMap.values();
	}
}
