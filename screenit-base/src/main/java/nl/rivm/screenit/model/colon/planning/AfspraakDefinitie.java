package nl.rivm.screenit.model.colon.planning;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.util.ArrayList;
import java.util.List;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.ManyToOne;
import javax.persistence.Transient;

import nl.rivm.screenit.model.IActief;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.colon.Kamer;
import nl.rivm.screenit.model.helper.HibernateMagicNumber;
import nl.topicuszorg.hibernate.object.model.HibernateObject;
import nl.topicuszorg.planning.solvers.model.IContactDefinitie;
import nl.topicuszorg.wicket.planning.model.appointment.Location;
import nl.topicuszorg.wicket.planning.model.appointment.definition.ActionDefinition;
import nl.topicuszorg.wicket.planning.model.appointment.definition.ActionType;

import org.hibernate.annotations.Proxy;

@Entity
@Proxy(lazy = true)
public class AfspraakDefinitie extends ActionDefinition implements HibernateObject, IContactDefinitie, IActief
{

	private static final long serialVersionUID = 1L;

	@Column(length = HibernateMagicNumber.L250)
	private String toelichting;

	@Column(length = HibernateMagicNumber.L250)
	private String instructie;

	private Boolean actief = Boolean.TRUE;

	@Enumerated(EnumType.STRING)
	private TypeAfspraak typeAfspraak = TypeAfspraak.AFSPRAAK;

	private Integer aantalClienten = Integer.valueOf(1);

	@ManyToOne
	private Instelling instelling;

	public AfspraakDefinitie(String naam, String omschrijving, Integer duurAfspraakInMinuten, ActionType actiontype, AfspraakDefinitie peilActie)
	{
		super.setDuurAfspraakInMinuten(duurAfspraakInMinuten);
		super.setType(actiontype);
		super.setPeilActie(peilActie);
		super.setCode(naam);
		super.setLabel(omschrijving);
	}

	public AfspraakDefinitie()
	{
	}

	public String getToelichting()
	{
		return toelichting;
	}

	public void setToelichting(String toelichting)
	{
		this.toelichting = toelichting;
	}

	public String getInstructie()
	{
		return instructie;
	}

	public void setInstructie(String instructie)
	{
		this.instructie = instructie;
	}

	@Override
	public String getNaam()
	{
		return getCode();
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

	public TypeAfspraak getTypeAfspraak()
	{
		return typeAfspraak;
	}

	public void setTypeAfspraak(TypeAfspraak typeAfspraak)
	{
		this.typeAfspraak = typeAfspraak;
	}

	public Integer getAantalClienten()
	{
		return aantalClienten;
	}

	public void setAantalClienten(Integer aantalClienten)
	{
		this.aantalClienten = aantalClienten;
	}

	public Instelling getInstelling()
	{
		return instelling;
	}

	public void setInstelling(Instelling instelling)
	{
		this.instelling = instelling;
	}

	@Transient
	public List<Location> getActievePossibleLocations()
	{
		List<Location> actieveKamers = new ArrayList<Location>();

		for (Location location : getPossibleLocations())
		{
			if (location instanceof Kamer)
			{
				Kamer kamer = (Kamer) location;
				if (kamer.getActief())
				{
					actieveKamers.add(kamer);
				}
			}
		}

		return actieveKamers;
	}

	@Override
	public String toString()
	{
		return getLabel() + " (" + getDuurAfspraakInMinuten() + " minuten)";
	}
}
