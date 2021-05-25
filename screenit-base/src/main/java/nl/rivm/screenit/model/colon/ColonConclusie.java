package nl.rivm.screenit.model.colon;

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
import javax.persistence.Index;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.colon.enums.ColonCTColografieReden;
import nl.rivm.screenit.model.colon.enums.ColonConclusieType;
import nl.rivm.screenit.model.colon.enums.ColonGeenOnderzoekReden;
import nl.rivm.screenit.model.helper.HibernateMagicNumber;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.envers.Audited;

@Entity
@Table(schema = "colon", indexes = { @Index(name = "idx_colon_conclusie_no_show_coloscopie", columnList = "noShowColoscopie"),
	@Index(name = "idx_colon_conclusie_type", columnList = "type") })
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
@Audited
public class ColonConclusie extends AbstractHibernateObject
{

	private static final long serialVersionUID = 1L;

	@Enumerated(EnumType.STRING)
	@Column(nullable = false)
	private ColonConclusieType type;

	@ManyToOne(fetch = FetchType.LAZY, optional = false)
	private InstellingGebruiker instellingGebruiker;

	@Temporal(TemporalType.TIMESTAMP)
	@Column(nullable = false)
	private Date datum;

	@Temporal(TemporalType.TIMESTAMP)
	@Column(nullable = true)
	private Date datumColoscopie;

	@Column(nullable = true)
	private Boolean noShowColoscopie;

	@Column(nullable = true)
	private Boolean coloscopieDatumOpVerzoekClient;

	@Enumerated(EnumType.STRING)
	@Column(nullable = true)
	@Deprecated
	private ColonCTColografieReden redenCTColografie;

	@Column(length = HibernateMagicNumber.L500, nullable = true)
	@Deprecated
	private String tekstCTColografieAnders;

	@Column(nullable = true)
	private Integer asaScore;

	@Enumerated(EnumType.STRING)
	@Column(nullable = true)
	private ColonGeenOnderzoekReden geenOnderzoekReden;

	@ManyToOne(fetch = FetchType.LAZY, optional = true)
	private Kamer locatieNieuweAfspraak;

	@Temporal(TemporalType.TIMESTAMP)
	@Column(nullable = true)
	private Date datumTijdNieuweAfspraak;

	@Temporal(TemporalType.TIMESTAMP)
	@Column(nullable = true)
	private Date noShowBericht;

	public ColonConclusieType getType()
	{
		return type;
	}

	public void setType(ColonConclusieType type)
	{
		this.type = type;
	}

	public ColonGeenOnderzoekReden getGeenOnderzoekReden()
	{
		return geenOnderzoekReden;
	}

	public void setGeenOnderzoekReden(ColonGeenOnderzoekReden geenOnderzoekReden)
	{
		this.geenOnderzoekReden = geenOnderzoekReden;
	}

	public InstellingGebruiker getInstellingGebruiker()
	{
		return instellingGebruiker;
	}

	public void setInstellingGebruiker(InstellingGebruiker instellingGebruiker)
	{
		this.instellingGebruiker = instellingGebruiker;
	}

	public Date getDatum()
	{
		return datum;
	}

	public void setDatum(Date datum)
	{
		this.datum = datum;
	}

	public Date getDatumColoscopie()
	{
		return datumColoscopie;
	}

	public void setDatumColoscopie(Date datumColoscopie)
	{
		this.datumColoscopie = datumColoscopie;
	}

	public ColonCTColografieReden getRedenCTColografie()
	{
		return redenCTColografie;
	}

	public void setRedenCTColografie(ColonCTColografieReden redenCTColografie)
	{
		this.redenCTColografie = redenCTColografie;
	}

	public String getTekstCTColografieAnders()
	{
		return tekstCTColografieAnders;
	}

	public void setTekstCTColografieAnders(String tekstCTColografieAnders)
	{
		this.tekstCTColografieAnders = tekstCTColografieAnders;
	}

	public Integer getAsaScore()
	{
		return asaScore;
	}

	public void setAsaScore(Integer asaScore)
	{
		this.asaScore = asaScore;
	}

	public Date getDatumTijdNieuweAfspraak()
	{
		return datumTijdNieuweAfspraak;
	}

	public void setDatumTijdNieuweAfspraak(Date datumTijdNieuweAfspraak)
	{
		this.datumTijdNieuweAfspraak = datumTijdNieuweAfspraak;
	}

	public Kamer getLocatieNieuweAfspraak()
	{
		return locatieNieuweAfspraak;
	}

	public void setLocatieNieuweAfspraak(Kamer locatieNieuweAfspraak)
	{
		this.locatieNieuweAfspraak = locatieNieuweAfspraak;
	}

	public Date getNoShowBericht()
	{
		return noShowBericht;
	}

	public void setNoShowBericht(Date noShowBericht)
	{
		this.noShowBericht = noShowBericht;
	}

	public Boolean getNoShowColoscopie()
	{
		return noShowColoscopie;
	}

	public void setNoShowColoscopie(Boolean noShowColoscopie)
	{
		this.noShowColoscopie = noShowColoscopie;
	}

	public Boolean getColoscopieDatumOpVerzoekClient()
	{
		return coloscopieDatumOpVerzoekClient;
	}

	public void setColoscopieDatumOpVerzoekClient(Boolean opVerzoekClient)
	{
		this.coloscopieDatumOpVerzoekClient = opVerzoekClient;
	}
}
