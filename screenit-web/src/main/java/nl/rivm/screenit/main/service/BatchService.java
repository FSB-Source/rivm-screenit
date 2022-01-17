
package nl.rivm.screenit.main.service;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.util.Date;
import java.util.List;

import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.batch.BatchServerStatus;
import nl.rivm.screenit.model.batch.Trigger;
import nl.rivm.screenit.model.enums.JobType;

import org.springframework.jms.UncategorizedJmsException;

public interface BatchService
{

	List<BatchServerStatus> getBatchServerStatus() throws UncategorizedJmsException;

	List<Trigger> getScheduledTriggers() throws UncategorizedJmsException;

	Date addTrigger(Trigger trigger, InstellingGebruiker instellingGebruiker);

	Boolean removeTrigger(final String triggerNaam, InstellingGebruiker instellingGebruiker);

	List<JobType> getBatchQueue();

	void getStopAllBatchJobs() throws UncategorizedJmsException;

}
