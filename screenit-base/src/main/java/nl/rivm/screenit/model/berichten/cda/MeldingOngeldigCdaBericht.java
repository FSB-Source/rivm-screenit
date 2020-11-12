
package nl.rivm.screenit.model.berichten.cda;

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
import javax.persistence.Index;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.helper.HibernateMagicNumber;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;

@Entity(name = "cda_melding_ongeldig_bericht")
@Table(schema = "gedeeld", indexes = @Index(name = "idx_cda_melding_ongeldig_bericht_actief", columnList = "actief") )
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "verslag.cache")
public class MeldingOngeldigCdaBericht extends AbstractHibernateObject
{

	private static final long serialVersionUID = 1L;

	@ManyToOne
	private OntvangenCdaBericht ontvangenCdaBericht;

	@ManyToOne
	private Instelling uitvoerendeOrganisatie;

	@ManyToOne
	private Gebruiker uitvoerder;

	@Column(length = HibernateMagicNumber.L512)
	private String melding;

	@Column(nullable = false)
	private Boolean herstelbaar;

	@Column(nullable = false)
	private Boolean actief;

	@Column(length = HibernateMagicNumber.L12)
	private String bsn;

	@ManyToOne
	private ScreeningOrganisatie screeningOrganisatie;

	@Temporal(TemporalType.TIMESTAMP)
	private Date datum;

	public OntvangenCdaBericht getOntvangenCdaBericht()
	{
		return ontvangenCdaBericht;
	}

	public void setOntvangenCdaBericht(OntvangenCdaBericht ontvangenCdaBericht)
	{
		this.ontvangenCdaBericht = ontvangenCdaBericht;
	}

	public Gebruiker getUitvoerder()
	{
		return uitvoerder;
	}

	public void setUitvoerder(Gebruiker uitvoerder)
	{
		this.uitvoerder = uitvoerder;
	}

	public String getMelding()
	{
		return melding;
	}

	public void setMelding(String melding)
	{
		this.melding = melding;
	}

	public Boolean getHerstelbaar()
	{
		return herstelbaar;
	}

	public void setHerstelbaar(Boolean herstelbaar)
	{
		this.herstelbaar = herstelbaar;
	}

	public Instelling getUitvoerendeOrganisatie()
	{
		return uitvoerendeOrganisatie;
	}

	public void setUitvoerendeOrganisatie(Instelling uitvoerendeOrganisatie)
	{
		this.uitvoerendeOrganisatie = uitvoerendeOrganisatie;
	}

	public Boolean getActief()
	{
		return actief;
	}

	public void setActief(Boolean actief)
	{
		this.actief = actief;
	}

	public String getBsn()
	{
		return bsn;
	}

	public void setBsn(String bsn)
	{
		this.bsn = bsn;
	}

	public ScreeningOrganisatie getScreeningOrganisatie()
	{
		return screeningOrganisatie;
	}

	public void setScreeningOrganisatie(ScreeningOrganisatie screeningOrganisatie)
	{
		this.screeningOrganisatie = screeningOrganisatie;
	}

	public Date getDatum()
	{
		return datum;
	}

	public void setDatum(Date datum)
	{
		this.datum = datum;
	}

}
