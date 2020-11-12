package nl.rivm.screenit.model.cervix.facturatie;

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

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.Index;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.cervix.CervixHuisartsLocatie;
import nl.rivm.screenit.model.cervix.CervixMonster;
import nl.rivm.screenit.model.cervix.enums.CervixTariefType;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.envers.Audited;

@Entity
@Table(
	schema = "cervix",
	name = "verrichting",
	indexes = { @Index(name = "idx_CERVIX_VERRICHTING_VERRICHTINGS_DATUM", columnList = "verrichtingsDatum"), @Index(name = "idx_CERVIX_VERRICHTING_TYPE", columnList = "type") })
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
@Audited
public class CervixVerrichting extends AbstractHibernateObject
{
	@ManyToOne(fetch = FetchType.LAZY, optional = false)
	private CervixMonster monster;

	@Column(nullable = false)
	@Temporal(TemporalType.TIMESTAMP)
	private Date verrichtingsDatum;

	@ManyToOne(fetch = FetchType.LAZY, optional = false)
	private ScreeningOrganisatie regio;

	@ManyToOne(fetch = FetchType.LAZY, optional = true)
	private CervixHuisartsLocatie huisartsLocatie;

	@ManyToOne(fetch = FetchType.LAZY, optional = false)
	private Client client;

	@OneToMany(fetch = FetchType.LAZY, mappedBy = "verrichting")
	private List<CervixBoekRegel> boekRegels = new ArrayList<>();

	@OneToOne(fetch = FetchType.LAZY)
	private CervixBoekRegel laatsteBoekRegel;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private CervixTariefType type;

	public Client getClient()
	{
		return client;
	}

	public void setClient(Client client)
	{
		this.client = client;
	}

	public CervixMonster getMonster()
	{
		return monster;
	}

	public void setMonster(CervixMonster monster)
	{
		this.monster = monster;
	}

	public Date getVerrichtingsDatum()
	{
		return verrichtingsDatum;
	}

	public void setVerrichtingsDatum(Date verrichtingsDatum)
	{
		this.verrichtingsDatum = verrichtingsDatum;
	}

	public CervixHuisartsLocatie getHuisartsLocatie()
	{
		return huisartsLocatie;
	}

	public void setHuisartsLocatie(CervixHuisartsLocatie huisartsLocatie)
	{
		this.huisartsLocatie = huisartsLocatie;
	}

	public ScreeningOrganisatie getRegio()
	{
		return regio;
	}

	public void setRegio(ScreeningOrganisatie regio)
	{
		this.regio = regio;
	}

	public CervixTariefType getType()
	{
		return type;
	}

	public void setType(CervixTariefType type)
	{
		this.type = type;
	}

	public List<CervixBoekRegel> getBoekRegels()
	{
		return boekRegels;
	}

	public void setBoekRegels(List<CervixBoekRegel> boekRegels)
	{
		this.boekRegels = boekRegels;
	}

	public CervixBoekRegel getLaatsteBoekRegel()
	{
		return laatsteBoekRegel;
	}

	public void setLaatsteBoekRegel(CervixBoekRegel laatsteBoekRegel)
	{
		this.laatsteBoekRegel = laatsteBoekRegel;
	}

}
