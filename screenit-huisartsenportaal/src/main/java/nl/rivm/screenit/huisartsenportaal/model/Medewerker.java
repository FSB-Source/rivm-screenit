package nl.rivm.screenit.huisartsenportaal.model;

/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
 * %%
 * Copyright (C) 2016 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.List;

import javax.persistence.Access;
import javax.persistence.AccessType;
import javax.persistence.CollectionTable;
import javax.persistence.ElementCollection;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Inheritance;
import javax.persistence.InheritanceType;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.huisartsenportaal.model.enums.InlogMethode;
import nl.rivm.screenit.huisartsenportaal.model.enums.Recht;

import org.hibernate.envers.Audited;
import org.joda.time.DateTime;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.userdetails.UserDetails;

@Entity
@Audited
@Table(name = "org_medewerker")
@Inheritance(strategy = InheritanceType.SINGLE_TABLE)
@Getter
@Setter
public abstract class Medewerker implements UserDetails, Serializable
{
	public static final int MAX_ATTEMPS = 4;

	public static final int MAX_LOCKED = 15;

	@Id
	@Access(AccessType.PROPERTY)
	@GeneratedValue(strategy = GenerationType.AUTO)
	private Long huisartsportaalId;

	@Access(AccessType.PROPERTY)
	private Long screenitId;

	private String gebruikersnaam;

	private String password;

	private Boolean actief;

	private Boolean isOvereenkomstGetekend;

	@Temporal(TemporalType.TIMESTAMP)
	private Date lastAttemptDate;

	private Integer attempts = 0;

	@Enumerated(EnumType.STRING)
	private InlogMethode inlogMethode;

	private String inlogCode;

	@ElementCollection(targetClass = Recht.class, fetch = FetchType.EAGER)
	@Enumerated(EnumType.STRING)
	@CollectionTable(name = "org_medewerker_rol")
	private List<Recht> rollen = new ArrayList<>();

	@Override
	public String getUsername()
	{
		return getHuisartsportaalId().toString();
	}

	@Override
	public Collection<? extends GrantedAuthority> getAuthorities()
	{
		return getRollen();
	}

	@Override
	public boolean isAccountNonExpired()
	{
		return actief;
	}

	@Override
	public boolean isAccountNonLocked()
	{
		return getAttempts() < MAX_ATTEMPS || (getLastAttemptDate() == null ||
			(getLastAttemptDate() != null && new DateTime().minusMinutes(MAX_LOCKED).isAfter(new DateTime(getLastAttemptDate()))));
	}

	@Override
	public boolean isCredentialsNonExpired()
	{
		return getInlogCode() == null || (getInlogCode() != null && getAttempts() < MAX_ATTEMPS);
	}

	@Override
	public boolean isEnabled()
	{
		return actief;
	}

}
