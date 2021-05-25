
package nl.rivm.screenit.model.batch;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.io.Serializable;
import java.util.Date;

import nl.rivm.screenit.model.enums.JobType;

public class Trigger implements Serializable
{

	private static final long serialVersionUID = 1L;

	private String cronExpressie;

	private String triggerNaam;

	private JobType jobType;

	private Date nextFireTime;

	public String getCronExpressie()
	{
		return cronExpressie;
	}

	public void setCronExpressie(String cronExpressie)
	{
		this.cronExpressie = cronExpressie;
	}

	public JobType getJobType()
	{
		return jobType;
	}

	public void setJobType(JobType jobType)
	{
		this.jobType = jobType;
	}

	public String getTriggerNaam()
	{
		return triggerNaam;
	}

	public void setTriggerNaam(String triggerNaam)
	{
		this.triggerNaam = triggerNaam;
	}

	public Date getNextFireTime()
	{
		return nextFireTime;
	}

	public void setNextFireTime(Date nextFireTime)
	{
		this.nextFireTime = nextFireTime;
	}

}
