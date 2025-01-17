package nl.rivm.screenit.model.nieuws;

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

import java.util.Date;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.helper.HibernateMagicNumber;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.envers.Audited;

@Entity
@Table(schema = "algemeen")
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
@Audited
public class NieuwsItem extends AbstractHibernateObject
{

	private static final long serialVersionUID = 1L;

	@OneToMany(fetch = FetchType.LAZY, mappedBy = "nieuwsItem", cascade = CascadeType.ALL)
	private List<GebruikerNieuwsItem> gebruikerNieuwsItems;

	@Column(nullable = false)
	private String titel;

	@Column(nullable = false, length = HibernateMagicNumber.L4096)
	private String tekst;

	@Column(nullable = false)
	@Temporal(TemporalType.DATE)
	private Date publicerenVanaf;

	@Column
	@Temporal(TemporalType.DATE)
	private Date publicerenTot;

	@ManyToOne(fetch = FetchType.LAZY, optional = false)
	private InstellingGebruiker gemaaktDoor;

	@Column(nullable = false)
	@Temporal(TemporalType.TIMESTAMP)
	private Date gemaakt;

	@ManyToOne(fetch = FetchType.LAZY)
	private InstellingGebruiker gewijzigdDoor;

	@Column
	@Temporal(TemporalType.TIMESTAMP)
	private Date gewijzigd;

	public String getTitel()
	{
		return titel;
	}

	public void setTitel(String titel)
	{
		this.titel = titel;
	}

	public String getTekst()
	{
		return tekst;
	}

	public void setTekst(String tekst)
	{
		this.tekst = tekst;
	}

	public Date getPublicerenVanaf()
	{
		return publicerenVanaf;
	}

	public void setPublicerenVanaf(Date publicerenVanaf)
	{
		this.publicerenVanaf = publicerenVanaf;
	}

	public Date getPublicerenTot()
	{
		return publicerenTot;
	}

	public void setPublicerenTot(Date publicerenTot)
	{
		this.publicerenTot = publicerenTot;
	}

	public Date getGewijzigd()
	{
		return gewijzigd;
	}

	public void setGewijzigd(Date gewijzigd)
	{
		this.gewijzigd = gewijzigd;
	}

	public InstellingGebruiker getGemaaktDoor()
	{
		return gemaaktDoor;
	}

	public void setGemaaktDoor(InstellingGebruiker gemaaktDoor)
	{
		this.gemaaktDoor = gemaaktDoor;
	}

	public Date getGemaakt()
	{
		return gemaakt;
	}

	public void setGemaakt(Date gemaakt)
	{
		this.gemaakt = gemaakt;
	}

	public InstellingGebruiker getGewijzigdDoor()
	{
		return gewijzigdDoor;
	}

	public void setGewijzigdDoor(InstellingGebruiker gewijzigdDoor)
	{
		this.gewijzigdDoor = gewijzigdDoor;
	}

	public List<GebruikerNieuwsItem> getGebruikerNieuwsItems()
	{
		return gebruikerNieuwsItems;
	}

	public void setGebruikerNieuwsItems(List<GebruikerNieuwsItem> gebruikerNieuwsItems)
	{
		this.gebruikerNieuwsItems = gebruikerNieuwsItems;
	}
}
