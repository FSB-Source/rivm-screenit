package nl.rivm.screenit.model.verwerkingverslag.cervix;

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

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import nl.rivm.screenit.model.enums.HuisartsBerichtType;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;

@Entity
@Table(schema = "cervix", name = "gevolgen_labproces_verwerken_rapportage_huisartsbericht_type")
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
public class CervixGevolgenLabprocesVerwerkenRapportageHuisartsberichtType extends AbstractHibernateObject
{
	@ManyToOne(optional = false)
	private CervixGevolgenLabprocesVerwerkenRapportage rapportage;

	@Enumerated(EnumType.STRING)
	@Column(nullable = false)
	private HuisartsBerichtType huisartsBerichtType;

	private long aantal;

	public CervixGevolgenLabprocesVerwerkenRapportageHuisartsberichtType()
	{
	}

	public CervixGevolgenLabprocesVerwerkenRapportageHuisartsberichtType(CervixGevolgenLabprocesVerwerkenRapportage rapportage, HuisartsBerichtType huisartsBerichtType,
		long aantal)
	{
		this.rapportage = rapportage;
		this.huisartsBerichtType = huisartsBerichtType;
		this.aantal = aantal;
	}

	public HuisartsBerichtType getHuisartsBerichtType()
	{
		return huisartsBerichtType;
	}

	public void setHuisartsBerichtType(HuisartsBerichtType huisartsBerichtType)
	{
		this.huisartsBerichtType = huisartsBerichtType;
	}

	public CervixGevolgenLabprocesVerwerkenRapportage getRapportage()
	{
		return rapportage;
	}

	public void setRapportage(CervixGevolgenLabprocesVerwerkenRapportage rapportage)
	{
		this.rapportage = rapportage;
	}

	public long getAantal()
	{
		return aantal;
	}

	public void setAantal(long aantal)
	{
		this.aantal = aantal;
	}
}
