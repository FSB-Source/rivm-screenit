
package nl.rivm.screenit.model.verwerkingverslag;

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

import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import nl.rivm.screenit.model.colon.enums.ColonUitnodigingCategorie;
import nl.rivm.screenit.model.enums.SelectieType;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;

@Entity
@Table(schema = "colon")
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
public class SelectieRapportageEntry extends AbstractHibernateObject
{
	@ManyToOne
	private SelectieRapportage rapportage;

	@Enumerated(EnumType.STRING)
	private ColonUitnodigingCategorie colonUitnodigingCategorie;

	@Enumerated(EnumType.STRING)
	private SelectieType selectieType;

	private Long aantal;

	private Long waarvanGepusht;

	public SelectieRapportage getRapportage()
	{
		return rapportage;
	}

	public void setRapportage(SelectieRapportage rapportage)
	{
		this.rapportage = rapportage;
	}

	public ColonUitnodigingCategorie getColonUitnodigingCategorie()
	{
		return colonUitnodigingCategorie;
	}

	public void setColonUitnodigingCategorie(ColonUitnodigingCategorie colonUitnodigingCategorie)
	{
		this.colonUitnodigingCategorie = colonUitnodigingCategorie;
	}

	public Long getAantal()
	{
		return aantal;
	}

	public void setAantal(Long aantal)
	{
		this.aantal = aantal;
	}

	public SelectieType getSelectieType()
	{
		return selectieType;
	}

	public void setSelectieType(SelectieType selectieType)
	{
		this.selectieType = selectieType;
	}

	public Long getWaarvanGepusht()
	{
		return waarvanGepusht;
	}

	public void setWaarvanGepusht(Long waarvanGepusht)
	{
		this.waarvanGepusht = waarvanGepusht;
	}
}
