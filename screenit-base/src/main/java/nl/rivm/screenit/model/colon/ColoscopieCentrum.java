
package nl.rivm.screenit.model.colon;

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

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.Transient;

import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.PostcodeCoordinaten;
import nl.rivm.screenit.model.helper.HibernateMagicNumber;
import nl.topicuszorg.organisatie.model.Adres;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.envers.Audited;
import org.hibernate.envers.NotAudited;
import org.joda.time.MutableDateTime;

@Entity
@Audited
public class ColoscopieCentrum extends Instelling
{

	private static final long serialVersionUID = 1L;

	@OneToMany(mappedBy = "coloscopieCentrum")
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
	@NotAudited
	private List<ColoscopieCentrumColonCapaciteitVerdeling> capaciteitVerdeling = new ArrayList<>();

	@OneToMany(mappedBy = "coloscopieCentrum", fetch = FetchType.LAZY, cascade = CascadeType.ALL)
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "organisatie.cache")
	private List<Kamer> kamers = new ArrayList<>();

	@ManyToOne
	@NotAudited
	private PostcodeCoordinaten postcodeCoordinaten;

	@Column(length = HibernateMagicNumber.L2048)
	private String locatieBeschrijving;

	private Integer aantalGeprognostiseerdeRoosterblokken;

	private Integer aantalGeprognostiseerdeRoosterblokkenVolgendJaar;

	public List<ColoscopieCentrumColonCapaciteitVerdeling> getCapaciteitVerdeling()
	{
		return capaciteitVerdeling;
	}

	public void setCapaciteitVerdeling(List<ColoscopieCentrumColonCapaciteitVerdeling> capaciteitVerdeling)
	{
		this.capaciteitVerdeling = capaciteitVerdeling;
	}

	@Transient
	public Date getOpeningVan()
	{
		MutableDateTime openingVan = new MutableDateTime(2010, 3, 28, 8, 0, 0, 0);
		return openingVan.toDate();
	}

	@Transient
	public Date getOpeningTot()
	{
		MutableDateTime openingTot = new MutableDateTime(2010, 3, 28, 18, 0, 0, 0);
		return openingTot.toDate();
	}

	public List<Kamer> getKamers()
	{
		return kamers;
	}

	public void setKamers(List<Kamer> kamers)
	{
		this.kamers = kamers;
	}

	public PostcodeCoordinaten getPostcodeCoordinaten()
	{
		return postcodeCoordinaten;
	}

	public void setPostcodeCoordinaten(PostcodeCoordinaten postcodeCoordinaten)
	{
		this.postcodeCoordinaten = postcodeCoordinaten;
	}

	public String getLocatieBeschrijving()
	{
		return locatieBeschrijving;
	}

	public void setLocatieBeschrijving(String locatieBeschrijving)
	{
		this.locatieBeschrijving = locatieBeschrijving;
	}

	public Integer getAantalGeprognostiseerdeRoosterblokken()
	{
		return aantalGeprognostiseerdeRoosterblokken;
	}

	public void setAantalGeprognostiseerdeRoosterblokken(Integer aantalGeprognostiseerdeRoosterblokken)
	{
		this.aantalGeprognostiseerdeRoosterblokken = aantalGeprognostiseerdeRoosterblokken;
	}

	public Adres getEersteAdres()
	{
		Adres adres = null;
		List<Adres> adressen = getAdressen();
		if (adressen != null && !adressen.isEmpty())
		{
			adres = adressen.get(0);
		}
		return adres;
	}

	public String getEerstePlaats()
	{
		Adres adres = getEersteAdres();

		String plaats = null;
		if (adres != null)
		{
			plaats = adres.getPlaats();
		}
		return plaats;
	}

	public Integer getAantalGeprognostiseerdeRoosterblokkenVolgendJaar()
	{
		return aantalGeprognostiseerdeRoosterblokkenVolgendJaar;
	}

	public void setAantalGeprognostiseerdeRoosterblokkenVolgendJaar(Integer aantalGeprognostiseerdeRoosterblokkenVolgendJaar)
	{
		this.aantalGeprognostiseerdeRoosterblokkenVolgendJaar = aantalGeprognostiseerdeRoosterblokkenVolgendJaar;
	}

}
