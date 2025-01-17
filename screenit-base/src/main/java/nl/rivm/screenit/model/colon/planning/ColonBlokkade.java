package nl.rivm.screenit.model.colon.planning;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;
import javax.persistence.Transient;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.colon.enums.ColonTijdslotType;
import nl.rivm.screenit.model.helper.HibernateMagicNumber;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.envers.Audited;

@Getter
@Setter
@Entity
@Table(schema = "colon", name = "blokkade")
@Audited
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "colon.cache")
public class ColonBlokkade extends ColonTijdslot implements Comparable<ColonBlokkade>
{
	public ColonBlokkade()
	{
		setType(ColonTijdslotType.BLOKKADE);
	}

	@Column(length = HibernateMagicNumber.L256)
	private String omschrijving;

	@Override
	@Transient
	public ColonBlokkade transientClone()
	{
		ColonBlokkade item = (ColonBlokkade) super.transientClone();
		item.setOmschrijving(omschrijving);
		return item;
	}

	@Override
	public int compareTo(ColonBlokkade o)
	{
		int cmp = 0;
		if (o != null)
		{
			if (getVanaf() != null && o.getVanaf() != null)
			{
				cmp = getVanaf().compareTo(o.getVanaf());
			}
			else if (getVanaf() != null)
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
}
