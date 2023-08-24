
package nl.rivm.screenit.model.project;

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
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.IActief;
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
public class ProjectClient extends AbstractHibernateObject implements IActief
{

	@Column(nullable = false)
	@Temporal(TemporalType.TIMESTAMP)
	private Date toegevoegd;

	@ManyToOne(fetch = FetchType.LAZY)
	private Client client;

	@ManyToOne(fetch = FetchType.LAZY)
	private Project project;

	@ManyToOne(fetch = FetchType.LAZY)
	private ProjectGroep groep;

	@Cascade({ CascadeType.ALL })
	@OneToMany(fetch = FetchType.LAZY, mappedBy = "projectClient")
	private List<ProjectClientAttribuut> attributen = new ArrayList<>();

	@OneToMany(fetch = FetchType.LAZY, mappedBy = "projectClient")
	@Cascade(CascadeType.DELETE)
	private List<ProjectBrief> brieven = new ArrayList<>();

	private Boolean actief = Boolean.TRUE;

	@Enumerated(EnumType.STRING)
	private ProjectInactiefReden projectInactiefReden;

	@Temporal(TemporalType.TIMESTAMP)
	private Date projectInactiefDatum;

	@Deprecated
	@ManyToOne(fetch = FetchType.LAZY, optional = true)
	@NotAudited
	@Cascade(CascadeType.DELETE)
	private ProjectInactiveerDocument projectInactiveerDocument;

	private Boolean isUitgenodigdInProjectPeriode = Boolean.FALSE;

}
