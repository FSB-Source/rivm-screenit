
package nl.rivm.screenit.model.colon.planning;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.util.List;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;
import javax.persistence.Transient;

import nl.rivm.screenit.model.colon.Kamer;
import nl.rivm.screenit.model.colon.enums.ColonTijdSlotType;
import nl.rivm.screenit.model.helper.HibernateMagicNumber;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.object.model.HibernateObject;
import nl.topicuszorg.planning.model.IEventType;
import nl.topicuszorg.planning.model.IParticipant;
import nl.topicuszorg.planning.model.IRecurrence;
import nl.topicuszorg.planning.model.enums.AppointmentType;
import nl.topicuszorg.wicket.planning.model.appointment.AbstractAppointment;
import nl.topicuszorg.wicket.planning.model.appointment.recurrence.AbstractRecurrence;

import org.apache.commons.lang.StringUtils;
import org.codehaus.jackson.JsonNode;
import org.codehaus.jackson.map.ObjectMapper;
import org.codehaus.jackson.node.ObjectNode;
import org.hibernate.Hibernate;
import org.hibernate.envers.Audited;

@Entity(name = "blokkade")
@Table(schema = "colon")
@Audited
public class ColonBlokkade extends AbstractAppointment implements Comparable<ColonBlokkade>
{

	private static final long serialVersionUID = 1L;

	@Column(length = HibernateMagicNumber.L256)
	private String description;

	public ColonBlokkade()
	{
		super();
		setTitle(ColonTijdSlotType.BLOKKADE.getTitle());
	}

	@Override
	public AppointmentType getAppointmentType()
	{
		return AppointmentType.ONBESCHIKBAARHEID;
	}

	@Override
	public IEventType getEventType()
	{
		return EventType.PURPLE;
	}

	@Override
	@Transient
	public ColonBlokkade transientClone()
	{
		ColonBlokkade item = (ColonBlokkade) super.transientClone();
		item.setDescription(description);
		return item;
	}

	@Override
	public Kamer getLocation()
	{
		return (Kamer) super.getLocation();
	}

	@Override
	public String getDescription()
	{
		return description;
	}

	@Override
	public void setDescription(String description)
	{
		this.description = description;
	}

	@Override
	public JsonNode toJson()
	{
		ObjectMapper mapper = new ObjectMapper();

		ObjectNode json = mapper.createObjectNode();
		json.put("id", getId());
		json.put("title", "Blokkade");
		json.put("description", getDescription());
		if (getEventType() == null)
		{
			json.put("eventColor", "Default");
		}
		else
		{
			json.put("eventColor", getEventType().getName());
		}
		json.put("appointmentType", StringUtils.capitalize(getAppointmentType().toString().toLowerCase()));

		json.put("start", getStartTime().getTime());
		json.put("end", getEndTime().getTime());

		return json;
	}

	@Override
	public void setRecurrence(IRecurrence recurrence)
	{
		super.setRecurrence((AbstractRecurrence) recurrence);
	}

	@Override
	protected String getTooltip()
	{
		StringBuilder sb = new StringBuilder();

		sb.append(getTitle());
		sb.append("<br />").append(getDescription());

		sb.append("<br />").append(DateUtil.formatTime(getStartTime()));
		sb.append(" - ").append(DateUtil.formatTime(getEndTime()));

		return sb.toString();
	}

	@Override
	public int compareTo(ColonBlokkade o)
	{
		int cmp = 0;
		if (o != null)
		{
			if (getStartTime() != null && o.getStartTime() != null)
			{
				cmp = getStartTime().compareTo(o.getStartTime());
			}
			else if (getStartTime() != null)
			{
				cmp = 1;
			}
			else
			{
				cmp = -1;
			}
		}
		else
		{
			cmp = 1;
		}

		return cmp;
	}

	@Override
	public int hashCode()
	{
		final int prime = 31;
		int result = 1;
		result = prime * result + (getId() == null ? 0 : getId().hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj)
	{
		if (this == obj)
		{
			return true;
		}
		if (obj == null)
		{
			return false;
		}
		if (Hibernate.getClass(this) != Hibernate.getClass(obj))
		{
			return false;
		}
		HibernateObject other = (HibernateObject) obj;
		if (getId() == null)
		{
			if (other.getId() != null)
			{
				return false;
			}
		}
		else if (!getId().equals(other.getId()))
		{
			return false;
		}
		return true;
	}

	@Override
	public List<IParticipant> getParticipants()
	{
		return null;
	}

	@Override
	public void removeParticipant(IParticipant participant)
	{

	}

	@Override
	public void addParticipant(IParticipant participant)
	{

	}
}
