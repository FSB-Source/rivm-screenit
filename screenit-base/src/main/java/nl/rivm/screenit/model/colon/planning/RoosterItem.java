package nl.rivm.screenit.model.colon.planning;

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

import java.util.ArrayList;
import java.util.List;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.OneToMany;
import javax.persistence.Table;
import javax.persistence.Transient;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.model.Afspraak;
import nl.rivm.screenit.model.colon.Kamer;
import nl.rivm.screenit.model.colon.enums.ColonTijdSlotType;
import nl.topicuszorg.hibernate.object.model.HibernateObject;
import nl.topicuszorg.wicket.planning.model.schedule.ScheduleItem;

import org.apache.commons.lang3.builder.EqualsBuilder;
import org.hibernate.Hibernate;
import org.hibernate.envers.Audited;

@Entity
@Table(schema = "colon")
@Audited
@Getter
@Setter
public class RoosterItem extends ScheduleItem implements ITijdObject
{
	@OneToMany(mappedBy = "roosterItem", fetch = FetchType.LAZY)
	private List<Afspraak> afspraken = new ArrayList<>();

	@Column(nullable = false)
	private Boolean capaciteitMeeBepaald = Boolean.FALSE;

	@Transient
	private transient EventType eventType;

	public RoosterItem()
	{
		super(true);
		setTitle(ColonTijdSlotType.ROOSTER_ITEM.getTitle());
	}

	@Override
	@Transient
	public String getDescription()
	{
		return "<ul class=simplelist></ul>";
	}

	@Override
	@Transient
	public RoosterItem transientClone()
	{
		RoosterItem item = (RoosterItem) super.transientClone();
		item.setAfspraken(new ArrayList<>());
		return item;
	}

	@Override
	public Kamer getLocation()
	{
		return (Kamer) super.getLocation();
	}

	@Override
	protected String getTooltip()
	{
		return String.format("%s<br/>%s<br/>%s - %s<br/>%s",
			getTitle(),
			getScheduleSet().getDisplayname(),
			Constants.getTimeFormat().format(getStartTime()),
			Constants.getTimeFormat().format(getEndTime()),
			getLocation().getCalendarDisplayName());
	}

	@Transient
	@Override
	public AfspraakStatus getStatus()
	{
		return null;
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
			var otherAfspraakslot = (RoosterItem) obj;
			return new EqualsBuilder()
				.append(getStartTime(), otherAfspraakslot.getStartTime())
				.append(getLocation(), otherAfspraakslot.getLocation())
				.isEquals();
		}
		else if (!getId().equals(other.getId()))
		{
			return false;
		}

		return true;
	}

}
