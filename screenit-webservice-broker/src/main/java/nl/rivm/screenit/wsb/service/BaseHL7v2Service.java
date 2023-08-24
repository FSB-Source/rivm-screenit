package nl.rivm.screenit.wsb.service;

/*-
 * ========================LICENSE_START=================================
 * screenit-webservice-broker
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.ArrayList;
import java.util.List;

import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.Rivm;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Level;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.rivm.screenit.service.BerichtToBatchService;
import nl.rivm.screenit.service.LogService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.beans.factory.annotation.Autowired;

import ca.uhn.hl7v2.HL7Exception;
import ca.uhn.hl7v2.app.ApplicationException;
import ca.uhn.hl7v2.model.Message;

public abstract class BaseHL7v2Service<T extends Message>
{
	@Autowired
	protected LogService logService;

	@Autowired
	protected BerichtToBatchService verwerkBerichtService;

	@Autowired
	protected HibernateService hibernateService;

	protected abstract Message processTypedMessage(T message) throws ApplicationException, HL7Exception;

	protected void logging(LogGebeurtenis logGebeurtenis, Level level, Instelling instelling, String melding, Bevolkingsonderzoek bvo)
	{
		LogEvent event = new LogEvent();
		event.setLevel(level);
		event.setMelding(melding);
		List<Instelling> instellingen = addRivmInstelling(new ArrayList<>());
		if (instelling != null)
		{
			instellingen.add(instelling);
		}
		logService.logGebeurtenis(logGebeurtenis, instellingen, event, bvo);
	}

	private List<Instelling> addRivmInstelling(List<Instelling> instellingen)
	{
		List<Rivm> rivm = hibernateService.loadAll(Rivm.class);
		List<Instelling> rivmInstellingen = new ArrayList<>(rivm);
		instellingen.addAll(rivmInstellingen);
		return instellingen;
	}

}
