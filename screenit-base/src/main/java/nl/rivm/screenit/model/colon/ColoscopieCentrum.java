package nl.rivm.screenit.model.colon;

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

import java.util.ArrayList;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.PostcodeCoordinaten;
import nl.rivm.screenit.model.helper.HibernateMagicNumber;
import nl.topicuszorg.organisatie.model.Adres;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.envers.Audited;
import org.hibernate.envers.NotAudited;

@Entity
@Audited
@Getter
@Setter
public class ColoscopieCentrum extends Instelling
{

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

}
