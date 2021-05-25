package nl.rivm.screenit.model.mamma;

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

import java.math.BigDecimal;
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
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;
import javax.persistence.Transient;

import nl.rivm.screenit.model.mamma.enums.MammaCapaciteitBlokType;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.envers.Audited;
import org.joda.time.DateTime;

@Entity
@Table(
	schema = "mamma",
	name = "capaciteit_blok",
	indexes = { @Index(name = "idx_mamma_capaciteit_blok_vanaf", columnList = "vanaf"),
		@Index(name = "idx_mamma_capaciteit_blok_tot", columnList = "tot"), @Index(name = "idx_mamma_capaciteit_blok_blok_type", columnList = "blokType") })
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "mamma.cache")
@Audited
public class MammaCapaciteitBlok extends AbstractHibernateObject
{
	public MammaCapaciteitBlok()
	{
	}

	public MammaCapaciteitBlok(MammaCapaciteitBlok copy, int daysOffSet)
	{
		this.aantalOnderzoeken = copy.aantalOnderzoeken;
		this.blokType = copy.blokType;
		this.opmerkingen = copy.opmerkingen;
		this.screeningsEenheid = copy.screeningsEenheid;
		this.vanaf = new DateTime(copy.vanaf).plusDays(daysOffSet).toDate();
		this.tot = new DateTime(copy.tot).plusDays(daysOffSet).toDate();
		this.minderValideAfspraakMogelijk = copy.minderValideAfspraakMogelijk;
	}

	private static final long serialVersionUID = 1L;

	@ManyToOne(optional = false, fetch = FetchType.LAZY)
	private MammaScreeningsEenheid screeningsEenheid;

	@Temporal(TemporalType.TIMESTAMP)
	@Column(nullable = false)
	private Date vanaf;

	@Temporal(TemporalType.TIMESTAMP)
	@Column(nullable = false)
	private Date tot;

	@Enumerated(EnumType.STRING)
	@Column(nullable = false)
	private MammaCapaciteitBlokType blokType;

	@Column(nullable = false)
	private Integer aantalOnderzoeken;

	@Column
	private String opmerkingen;

	@Transient
	private BigDecimal beschikbareCapaciteit;

	@Transient
	private BigDecimal vrijeCapaciteit;

	@Column(nullable = false)
	private Boolean minderValideAfspraakMogelijk;

	@OneToMany(mappedBy = "capaciteitBlok", fetch = FetchType.LAZY)
	private final List<MammaAfspraak> afspraken = new ArrayList<>();

	public MammaScreeningsEenheid getScreeningsEenheid()
	{
		return screeningsEenheid;
	}

	public void setScreeningsEenheid(MammaScreeningsEenheid screeningsEenheid)
	{
		this.screeningsEenheid = screeningsEenheid;
	}

	public Date getVanaf()
	{
		return vanaf;
	}

	public void setVanaf(Date vanaf)
	{
		this.vanaf = vanaf;
	}

	public Date getTot()
	{
		return tot;
	}

	public void setTot(Date tot)
	{
		this.tot = tot;
	}

	public Integer getAantalOnderzoeken()
	{
		return aantalOnderzoeken;
	}

	public void setAantalOnderzoeken(Integer aantalOnderzoeken)
	{
		this.aantalOnderzoeken = aantalOnderzoeken;
	}

	public MammaCapaciteitBlokType getBlokType()
	{
		return blokType;
	}

	public void setBlokType(MammaCapaciteitBlokType blokType)
	{
		this.blokType = blokType;
	}

	public String getOpmerkingen()
	{
		return opmerkingen;
	}

	public void setOpmerkingen(String opmerkingen)
	{
		this.opmerkingen = opmerkingen;
	}

	public List<MammaAfspraak> getAfspraken()
	{
		return afspraken;
	}

	public BigDecimal getBeschikbareCapaciteit()
	{
		return beschikbareCapaciteit;
	}

	public void setBeschikbareCapaciteit(BigDecimal beschikbareCapaciteit)
	{
		this.beschikbareCapaciteit = beschikbareCapaciteit;
	}

	public BigDecimal getVrijeCapaciteit()
	{
		return vrijeCapaciteit;
	}

	public void setVrijeCapaciteit(BigDecimal vrijeCapaciteit)
	{
		this.vrijeCapaciteit = vrijeCapaciteit;
	}

	public Boolean getMinderValideAfspraakMogelijk()
	{
		return minderValideAfspraakMogelijk;
	}

	public void setMinderValideAfspraakMogelijk(Boolean minderValideAfspraakMogelijk)
	{
		this.minderValideAfspraakMogelijk = minderValideAfspraakMogelijk;
	}
}
