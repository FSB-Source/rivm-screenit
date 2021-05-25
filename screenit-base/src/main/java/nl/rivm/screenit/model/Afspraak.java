package nl.rivm.screenit.model;

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

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.ManyToOne;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;
import javax.persistence.Transient;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.model.colon.Kamer;
import nl.rivm.screenit.model.colon.planning.AfspraakDefinitie;
import nl.rivm.screenit.model.colon.planning.AfspraakStatus;
import nl.rivm.screenit.model.colon.planning.ITijdObject;
import nl.rivm.screenit.model.colon.planning.RoosterItem;
import nl.rivm.screenit.model.helper.HibernateMagicNumber;
import nl.rivm.screenit.util.NaamUtil;
import nl.topicuszorg.hibernate.object.annot.Range;
import nl.topicuszorg.hibernate.object.annot.Range.RangeCondition;
import nl.topicuszorg.hibernate.object.model.HibernateObject;
import nl.topicuszorg.planning.model.IParticipant;
import nl.topicuszorg.planning.model.IRecurrence;
import nl.topicuszorg.planning.model.enums.AppointmentType;
import nl.topicuszorg.wicket.planning.model.Discipline;
import nl.topicuszorg.wicket.planning.model.appointment.Action;
import nl.topicuszorg.wicket.planning.model.appointment.Task;
import nl.topicuszorg.wicket.planning.model.appointment.recurrence.AbstractRecurrence;

import org.apache.commons.lang.StringUtils;
import org.hibernate.Hibernate;
import org.hibernate.annotations.Cascade;
import org.hibernate.annotations.CascadeType;
import org.hibernate.envers.Audited;

@Entity
@Table(schema = "colon")
@Audited
public class Afspraak extends Action<Client> implements Cloneable, IActief, ITijdObject
{

	private static final long serialVersionUID = 1L;

	private static final String BREAK = "<br />";

	@Column
	private Boolean actief = Boolean.TRUE;

	@Column
	@Enumerated(EnumType.STRING)
	private AfspraakStatus status;

	@OneToOne(fetch = FetchType.LAZY)
	private Afspraak nieuweAfspraak;

	@OneToOne(fetch = FetchType.LAZY)
	private Afspraak oudeAfspraak;

	@ManyToOne
	@Cascade(CascadeType.SAVE_UPDATE)
	private Client client;

	@Column(nullable = false)
	private Long afspraaknummer;

	@Transient
	@Temporal(TemporalType.DATE)
	@Range(condition = RangeCondition.GREATER_OR_EQUAL, property = "richtdatum")
	private Date vanRichtdatum;

	@Transient
	@Temporal(TemporalType.DATE)
	@Range(condition = RangeCondition.LESS_OR_EQUAL, property = "richtdatum")
	private Date totRichtdatum;

	private Date ingevoerd = new Date();

	private Date datumLaatsteWijziging;

	@Column(length = HibernateMagicNumber.L4000)
	private String afzegreden;

	@ManyToOne(fetch = FetchType.LAZY)
	private RoosterItem roosterItem;

	@Override
	public void addParticipant(IParticipant participant)
	{
	}

	@Override
	public AfspraakStatus getStatus()
	{
		return status;
	}

	public Client getClient()
	{
		return client;
	}

	@Override
	@Transient
	public String getDisplayname()
	{
		if (StringUtils.isEmpty(super.getTitle()))
		{
			return "Actie toevoegen";
		}
		return super.getTitle();
	}

	@Override
	public List<Task<Client>> getTasks()
	{

		return null;
	}

	@Override
	public void removeParticipant(IParticipant participant)
	{
	}

	@Override
	public boolean removeTask(Task<Client> task)
	{

		return false;
	}

	public void setStatus(AfspraakStatus status)
	{
		this.status = status;
	}

	public void setClient(Client client)
	{
		this.client = client;
	}

	@Override
	public Kamer getLocation()
	{
		return (Kamer) super.getLocation();
	}

	@Override
	@Transient
	public String getDescription()
	{
		List<String> stringetjes = new ArrayList<String>();
		if (getClient() != null)
		{
			stringetjes.add("Client: " + getClient().getPersoon().getNaamVolledigMetVoornaam());
		}

		StringBuilder returnValue = new StringBuilder("<ul class=\"simplelist\">");
		for (String string : stringetjes)
		{
			returnValue.append("<li>" + string + "</li>");
		}
		returnValue.append("</ul>");
		return returnValue.toString();
	}

	@Override
	public void setRecurrence(IRecurrence recurrence)
	{
		super.setRecurrence((AbstractRecurrence) recurrence);
	}

	@Override
	public Boolean getActief()
	{
		return actief;
	}

	@Override
	public void setActief(Boolean actief)
	{
		this.actief = actief;
	}

	@Override
	@Transient
	public Afspraak transientClone()
	{
		Afspraak clone = (Afspraak) super.transientClone();

		clone.setStatus(getStatus());
		clone.setDisciplines(new ArrayList<Discipline>(getDisciplines()));
		clone.setDefinition(getDefinition());

		return clone;
	}

	@Override
	public AppointmentType getAppointmentType()
	{
		return AppointmentType.AFSPRAAK;
	}

	public Long getAfspraaknummer()
	{
		return afspraaknummer;
	}

	public void setAfspraaknummer(Long afspraaknummer)
	{
		this.afspraaknummer = afspraaknummer;
	}

	public Date getVanRichtdatum()
	{
		return vanRichtdatum;
	}

	public void setVanRichtdatum(Date vanRichtdatum)
	{
		this.vanRichtdatum = vanRichtdatum;
	}

	public Date getTotRichtdatum()
	{
		return totRichtdatum;
	}

	public void setTotRichtdatum(Date totRichtdatum)
	{
		this.totRichtdatum = totRichtdatum;
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
	public int hashCode()
	{
		final int prime = 31;
		int result = 1;
		result = prime * result + (this.getId() == null ? 0 : this.getId().hashCode());
		return result;
	}

	@Override
	public boolean addTask(Task<Client> task)
	{

		return false;
	}

	@Override
	public AfspraakDefinitie getDefinition()
	{
		return (AfspraakDefinitie) super.getDefinition();
	}

	@Override
	@Transient
	public String getTooltip()
	{
		StringBuilder sb = new StringBuilder();

		sb.append(getTitle());

		sb.append(BREAK).append(Constants.getTimeFormat().format(getStartTime()));
		sb.append(" - ").append(Constants.getTimeFormat().format(getEndTime()));

		if (getClient() != null)
		{
			sb.append(BREAK).append(NaamUtil.titelVoorlettersTussenvoegselEnAanspreekAchternaam(getClient()));
		}

		if (getLocation() instanceof Kamer)
		{
			sb.append(BREAK).append(getLocation().getCalendarDisplayName());
		}

		return sb.toString();
	}

	public Date getIngevoerd()
	{
		return ingevoerd;
	}

	public void setIngevoerd(Date ingevoerd)
	{
		this.ingevoerd = ingevoerd;
	}

	public Date getDatumLaatsteWijziging()
	{
		return datumLaatsteWijziging;
	}

	public void setDatumLaatsteWijziging(Date datumLaatsteWijziging)
	{
		this.datumLaatsteWijziging = datumLaatsteWijziging;
	}

	public String getAfzegreden()
	{
		return afzegreden;
	}

	public void setAfzegreden(String afzegreden)
	{
		this.afzegreden = afzegreden;
	}

	@Override
	public String toString()
	{
		return super.toString() + ", status: " + getStatus();
	}

	@Override
	public List<IParticipant> getParticipants()
	{

		return null;
	}

	public RoosterItem getRoosterItem()
	{
		return roosterItem;
	}

	public void setRoosterItem(RoosterItem roosterItem)
	{
		this.roosterItem = roosterItem;
	}

	public Afspraak getNieuweAfspraak()
	{
		return nieuweAfspraak;
	}

	public void setNieuweAfspraak(Afspraak nieuweAfspraak)
	{
		this.nieuweAfspraak = nieuweAfspraak;
	}

	public Afspraak getOudeAfspraak()
	{
		return oudeAfspraak;
	}

	public void setOudeAfspraak(Afspraak oudeAfspraak)
	{
		this.oudeAfspraak = oudeAfspraak;
	}
}
