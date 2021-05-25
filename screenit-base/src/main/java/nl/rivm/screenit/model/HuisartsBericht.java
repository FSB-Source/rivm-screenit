package nl.rivm.screenit.model;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.ManyToOne;

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.TablePerClassHibernateObject;
import nl.rivm.screenit.model.enums.HuisartsBerichtType;
import nl.rivm.screenit.model.helper.HibernateMagicNumber;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.envers.Audited;

@Entity
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
@Audited
public abstract class HuisartsBericht extends TablePerClassHibernateObject
{

	private static final long serialVersionUID = 1L;

	@ManyToOne(fetch = FetchType.LAZY, optional = false)
	private Client client;

	@ManyToOne(fetch = FetchType.LAZY, optional = false)
	private ScreeningOrganisatie screeningsOrganisatie;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private HuisartsBerichtType berichtType;

	@Column(length = 7000)
	private String berichtInhoud;

	private Date aanmaakDatum;

	public Client getClient()
	{
		return client;
	}

	public void setClient(Client client)
	{
		this.client = client;
	}

	public HuisartsBerichtType getBerichtType()
	{
		return berichtType;
	}

	public void setBerichtType(HuisartsBerichtType berichtType)
	{
		this.berichtType = berichtType;
	}

	public String getBerichtInhoud()
	{
		return berichtInhoud;
	}

	public void setBerichtInhoud(String berichtInhoud)
	{
		this.berichtInhoud = berichtInhoud;
	}

	public ScreeningOrganisatie getScreeningsOrganisatie()
	{
		return screeningsOrganisatie;
	}

	public void setScreeningsOrganisatie(ScreeningOrganisatie screeningsOrganisatie)
	{
		this.screeningsOrganisatie = screeningsOrganisatie;
	}

	public Date getAanmaakDatum()
	{
		return aanmaakDatum;
	}

	public void setAanmaakDatum(Date aanmaakDatum)
	{
		this.aanmaakDatum = aanmaakDatum;
	}
}
