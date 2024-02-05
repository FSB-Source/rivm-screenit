package nl.rivm.screenit.model;

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

import javax.persistence.CascadeType;
import javax.persistence.CollectionTable;
import javax.persistence.Column;
import javax.persistence.ElementCollection;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.Table;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.annotations.Where;

@Entity
@Table(schema = "algemeen")
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "organisatie.cache")
@Getter
@Setter
public class Rol extends AbstractHibernateObject implements INaam, IActief, IBevolkingsonderzoek
{
	private String naam;

	private String description;

	@OneToMany(cascade = CascadeType.ALL, fetch = FetchType.EAGER, mappedBy = "rol")
	@Where(clause = "actief = 'true'")
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "organisatie.cache")
	private List<Permissie> permissies = new ArrayList<>();

	@ManyToOne(fetch = FetchType.LAZY)
	private Rol parentRol;

	@ElementCollection(targetClass = Bevolkingsonderzoek.class)
	@Enumerated(EnumType.STRING)
	@Column(nullable = false)
	@CollectionTable(schema = "algemeen", name = "rol_bevolkingsonderzoeken")
	private List<Bevolkingsonderzoek> bevolkingsonderzoeken = new ArrayList<>();

	private Boolean actief = true;

	@Override
	public Boolean getExactMatch()
	{
		return null; 
	}

	@Override
	public void setExctMatch(Boolean exactMatch)
	{

	}

	@Override
	public String toString()
	{
		return "Rol [naam=" + naam + "]";
	}
}
