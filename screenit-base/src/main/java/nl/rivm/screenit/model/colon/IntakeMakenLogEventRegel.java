
package nl.rivm.screenit.model.colon;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import javax.persistence.Entity;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import nl.rivm.screenit.model.logging.IntakeMakenLogEvent;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;

import com.fasterxml.jackson.annotation.JsonIgnore;

@Entity
@Table(schema = "colon")
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
public class IntakeMakenLogEventRegel extends AbstractHibernateObject
{
	@ManyToOne
	@JsonIgnore 
	private IntakeMakenLogEvent logEvent;

	private Long afspraakId;

	private Long clientId;

	private String vrijSlot;

	public IntakeMakenLogEvent getLogEvent()
	{
		return logEvent;
	}

	public void setLogEvent(IntakeMakenLogEvent logEvent)
	{
		this.logEvent = logEvent;
	}

	public String getVrijSlot()
	{
		return vrijSlot;
	}

	public void setVrijSlot(String vrijSlot)
	{
		this.vrijSlot = vrijSlot;
	}

	public Long getAfspraakId()
	{
		return afspraakId;
	}

	public void setAfspraakId(Long afspraakId)
	{
		this.afspraakId = afspraakId;
	}

	public Long getClientId()
	{
		return clientId;
	}

	public void setClientId(Long clientId)
	{
		this.clientId = clientId;
	}
}
