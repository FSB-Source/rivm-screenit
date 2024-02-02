package nl.rivm.screenit.model.project;

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
import java.util.Date;
import java.util.List;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.IActief;
import nl.rivm.screenit.model.INaam;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.annotations.Cascade;
import org.hibernate.annotations.CascadeType;
import org.hibernate.envers.Audited;
import org.hibernate.envers.NotAudited;

@Entity
@Table(schema = "gedeeld")
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
@Audited
@Getter
@Setter
public class ProjectGroep extends AbstractHibernateObject implements INaam, IActief
{
	private String naam;

	private Boolean actief;

	@Temporal(TemporalType.TIMESTAMP)
	private Date actiefDatum;

	private Integer populatie;

	@Enumerated(EnumType.STRING)
	private GroepInvoer groepInvoer;

	@NotAudited
	@OneToOne(fetch = FetchType.LAZY, mappedBy = "groep", cascade = javax.persistence.CascadeType.REMOVE)
	@Cascade(CascadeType.DELETE)
	private ProjectImport projectImport;

	@OneToMany(fetch = FetchType.LAZY, mappedBy = "groep", cascade = javax.persistence.CascadeType.REMOVE)
	@Cascade(CascadeType.DELETE)
	private List<ProjectBestand> projectBestanden = new ArrayList<>();

	@ManyToOne(fetch = FetchType.LAZY)
	private Project project;

	@OneToMany(fetch = FetchType.LAZY, mappedBy = "groep", cascade = javax.persistence.CascadeType.REMOVE)
	@Cascade(CascadeType.DELETE)
	private List<ProjectClient> clienten = new ArrayList<>();

	@Temporal(TemporalType.DATE)
	private Date uitnodigenVoorDKvoor;

	@Column(nullable = true)
	@Temporal(TemporalType.DATE)
	private Date uitnodigingenPushenNa;

}
