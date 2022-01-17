package nl.rivm.screenit.model.verwerkingverslag;

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
import java.util.Date;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Entity;
import javax.persistence.OneToMany;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;

@Entity
@Table(schema = "gedeeld")
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
public class BrievenGenererenRapportage extends AbstractHibernateObject
{

	@OneToMany(mappedBy = "rapportage", cascade = CascadeType.ALL)
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
	private List<BrievenGenererenRapportageEntry> entries = new ArrayList<>();

	@Temporal(TemporalType.TIMESTAMP)
	private Date datumVerwerking;

	public List<BrievenGenererenRapportageEntry> getEntries()
	{
		return entries;
	}

	public void setEntries(List<BrievenGenererenRapportageEntry> entries)
	{
		this.entries = entries;
	}

	public Date getDatumVerwerking()
	{
		return datumVerwerking;
	}

	public void setDatumVerwerking(Date datumVerwerking)
	{
		this.datumVerwerking = datumVerwerking;
	}

}
