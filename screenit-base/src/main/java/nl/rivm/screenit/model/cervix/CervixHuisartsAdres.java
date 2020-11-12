package nl.rivm.screenit.model.cervix;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.ManyToOne;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

import nl.rivm.screenit.model.BagAdres;
import nl.rivm.screenit.model.Woonplaats;

import org.hibernate.envers.Audited;
import org.hibernate.envers.NotAudited;

@Entity
@Audited
public class CervixHuisartsAdres extends BagAdres implements ICervixHuisartsportaalObject
{
	
	private static final long serialVersionUID = 1L;

	@Column
	private Long huisartsportaalId;

	@NotAudited
	@ManyToOne(fetch = FetchType.LAZY, optional = true)
	private Woonplaats woonplaats;

	@Temporal(TemporalType.TIMESTAMP)
	private Date mutatiedatum;

	@Override
	public Long getHuisartsportaalId()
	{
		return huisartsportaalId;
	}

	@Override
	public void setHuisartsportaalId(Long huisartsportaalId)
	{
		this.huisartsportaalId = huisartsportaalId;
	}

	@Override
	public void setScreenitId(Long id)
	{
		if (id != null)
		{
			setId(id);
		}
	}

	@Override
	public Long getScreenitId()
	{
		return getId();
	}

	@Override
	public Date getMutatiedatum()
	{
		return mutatiedatum;
	}

	@Override
	public void setMutatiedatum(Date mutatiedatum)
	{
		this.mutatiedatum = mutatiedatum;
	}

	public Woonplaats getWoonplaats()
	{
		return woonplaats;
	}

	public void setWoonplaats(Woonplaats woonplaats)
	{
		this.woonplaats = woonplaats;
	}
}
