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
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.Table;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.IActief;
import nl.rivm.screenit.model.colon.ColonIntakelocatie;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.envers.Audited;

@Setter
@Getter
@Entity
@Table(schema = "colon", name = "intakekamer")
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "colon.cache")
@Audited
public class ColonIntakekamer extends AbstractHibernateObject implements IActief
{
	@Column(nullable = false)
	private String naam;

	@Column
	private String omschrijving;

	@OneToMany(fetch = FetchType.LAZY, mappedBy = "kamer")
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "colon.cache")
	private List<ColonTijdslot> tijdslots = new ArrayList<>();

	@Column(nullable = false)
	private Boolean actief;

	@ManyToOne(fetch = FetchType.EAGER, optional = false)
	private ColonIntakelocatie intakelocatie;
}
