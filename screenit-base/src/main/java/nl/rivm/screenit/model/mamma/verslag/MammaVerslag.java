package nl.rivm.screenit.model.mamma.verslag;

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

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.ManyToOne;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;
import javax.persistence.UniqueConstraint;

import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.SingleTableHibernateObject;
import nl.rivm.screenit.model.berichten.Verslag;
import nl.rivm.screenit.model.berichten.cda.OntvangenCdaBericht;
import nl.rivm.screenit.model.berichten.enums.VerslagStatus;
import nl.rivm.screenit.model.berichten.enums.VerslagType;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.verslag.VerslagContent;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.envers.Audited;
import org.hibernate.envers.NotAudited;

@Entity
@Table(schema = "mamma", name = "cda_verslag", uniqueConstraints = { @UniqueConstraint(columnNames = "ontvangen_cda_bericht") })
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "verslag.cache")
@Audited
public abstract class MammaVerslag<T extends VerslagContent<?>> extends SingleTableHibernateObject implements Verslag<T, MammaScreeningRonde>
{
	@ManyToOne(fetch = FetchType.LAZY, optional = false)
	private MammaScreeningRonde screeningRonde;

	@OneToOne(fetch = FetchType.LAZY, optional = true)
	@NotAudited
	private OntvangenCdaBericht ontvangenCdaBericht;

	@Column(nullable = true)
	private String labCode;

	@Temporal(TemporalType.TIMESTAMP)
	private Date datumVerwerkt;

	@Temporal(TemporalType.TIMESTAMP)
	private Date datumOnderzoek;

	@ManyToOne(fetch = FetchType.LAZY)
	private InstellingGebruiker invoerder;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private VerslagStatus status;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private VerslagType type;

	@Override
	public T getVerslagContent()
	{
		return null;
	}

	@Override
	public void setVerslagContent(T verslagContent)
	{

	}

	@Override
	public OntvangenCdaBericht getOntvangenBericht()
	{
		return ontvangenCdaBericht;
	}

	@Override
	public void setOntvangenBericht(OntvangenCdaBericht ontvangencdaBericht)
	{
		this.ontvangenCdaBericht = ontvangencdaBericht;
	}

	@Override
	public Instelling getUitvoerderOrganisatie()
	{
		return null;
	}

	@Override
	public void setUitvoerderOrganisatie(Instelling uitvoerderOrganisatie)
	{
	}

	@Override
	public Gebruiker getUitvoerderMedewerker()
	{
		return null;
	}

	@Override
	public void setUitvoerderMedewerker(Gebruiker uitvoerderMedewerker)
	{
	}

	@Override
	public Date getDatumVerwerkt()
	{
		return datumVerwerkt;
	}

	@Override
	public void setDatumVerwerkt(Date datumVerwerkt)
	{
		this.datumVerwerkt = datumVerwerkt;
	}

	@Override
	public InstellingGebruiker getInvoerder()
	{
		return invoerder;
	}

	@Override
	public void setInvoerder(InstellingGebruiker invoerder)
	{
		this.invoerder = invoerder;
	}

	@Override
	public Date getDatumOnderzoek()
	{
		return datumOnderzoek;
	}

	@Override
	public void setDatumOnderzoek(Date datumOnderzoek)
	{
		this.datumOnderzoek = datumOnderzoek;
	}

	@Override
	public VerslagStatus getStatus()
	{
		return status;
	}

	@Override
	public void setStatus(VerslagStatus status)
	{
		this.status = status;
	}

	@Override
	public VerslagType getType()
	{
		return type;
	}

	@Override
	public void setType(VerslagType type)
	{
		this.type = type;
	}

	@Override
	public MammaScreeningRonde getScreeningRonde()
	{
		return screeningRonde;
	}

	@Override
	public void setScreeningRonde(MammaScreeningRonde screeningRonde)
	{
		this.screeningRonde = screeningRonde;
	}

	public OntvangenCdaBericht getOntvangenCdaBericht()
	{
		return ontvangenCdaBericht;
	}

	public void setOntvangenCdaBericht(OntvangenCdaBericht ontvangenCdaBericht)
	{
		this.ontvangenCdaBericht = ontvangenCdaBericht;
	}

	public String getLabCode()
	{
		return labCode;
	}

	public void setLabCode(String labCode)
	{
		this.labCode = labCode;
	}

}
