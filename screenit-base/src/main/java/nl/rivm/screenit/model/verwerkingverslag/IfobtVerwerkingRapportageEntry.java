package nl.rivm.screenit.model.verwerkingverslag;

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

import javax.persistence.Entity;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;

@Entity
@Table(schema = "colon")
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
public class IfobtVerwerkingRapportageEntry extends AbstractHibernateObject
{

	@ManyToOne
	private IfobtVerwerkingRapportage rapportage;

	private Long ifobtBestandId;

	private String bestandsNaam;

	private Long aantalVerwerkingen = Long.valueOf(0);

	public IfobtVerwerkingRapportage getRapportage()
	{
		return rapportage;
	}

	public void setRapportage(IfobtVerwerkingRapportage rapportage)
	{
		this.rapportage = rapportage;
	}

	public Long getAantalVerwerkingen()
	{
		return aantalVerwerkingen;
	}

	public void setAantalVerwerkingen(Long aantalVerwerkingen)
	{
		this.aantalVerwerkingen = aantalVerwerkingen;
	}

	public Long getIfobtBestandId()
	{
		return ifobtBestandId;
	}

	public void setIfobtBestandId(Long ifobtBestandId)
	{
		this.ifobtBestandId = ifobtBestandId;
	}

	public String getBestandsNaam()
	{
		return bestandsNaam;
	}

	public void setBestandsNaam(String bestandsNaam)
	{
		this.bestandsNaam = bestandsNaam;
	}
}
