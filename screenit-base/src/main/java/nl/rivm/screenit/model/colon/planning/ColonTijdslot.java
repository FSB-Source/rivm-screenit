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

import java.time.LocalDateTime;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.Transient;

import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.model.colon.enums.ColonTijdslotType;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.apache.commons.lang3.builder.EqualsBuilder;
import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.annotations.Index;
import org.hibernate.envers.Audited;

@Setter
@Getter
@Slf4j
@Entity
@Table(schema = "colon", name = "tijdslot")
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "colon.cache")
@Audited
public abstract class ColonTijdslot extends AbstractHibernateObject implements Cloneable
{
	@ManyToOne(optional = false)
	private ColonIntakekamer kamer;

	@Column(nullable = false)
	@Index(name = "idx_tijdslot_vanaf")
	private LocalDateTime vanaf;

	@Column(nullable = false)
	@Index(name = "idx_tijdslot_tot")
	private LocalDateTime tot;

	@Enumerated(EnumType.STRING)
	@Column(nullable = false)
	private ColonTijdslotType type;

	@Transient
	public ColonTijdslot transientClone()
	{
		ColonTijdslot clone = null;
		try
		{
			clone = (ColonTijdslot) super.clone();
			clone.setId(null);
			clone.setVanaf(getVanaf());
			clone.setTot(getTot());
			clone.setKamer(getKamer());
			clone.setType(getType());
		}
		catch (CloneNotSupportedException e)
		{
			LOG.error("Clone not supported", e);
			throw new RuntimeException(e);
		}

		return clone;
	}

	@Override
	protected boolean concreateEquals(AbstractHibernateObject obj)
	{
		if (getId() == null || obj.getId() == null)
		{
			var otherAfspraakslot = (ColonTijdslot) obj;
			return new EqualsBuilder()
				.append(getType(), otherAfspraakslot.getType())
				.append(getVanaf(), otherAfspraakslot.getVanaf())
				.append(getTot(), otherAfspraakslot.getTot())
				.append(getKamer(), otherAfspraakslot.getKamer())
				.isEquals();
		}
		return super.concreateEquals(obj);
	}
}
