package nl.rivm.screenit.model.cervix;

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
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.Index;
import javax.persistence.ManyToOne;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;
import javax.persistence.UniqueConstraint;

import nl.rivm.screenit.model.InpakbareUitnodiging;
import nl.rivm.screenit.model.cervix.enums.CervixMonsterType;

import org.hibernate.envers.Audited;
import org.hibernate.envers.NotAudited;

@Entity
@Table(
	schema = "cervix",
	name = "uitnodiging",
	uniqueConstraints = { @UniqueConstraint(columnNames = "uitnodigingsId"), @UniqueConstraint(columnNames = "brief"), @UniqueConstraint(columnNames = "monster") },
	indexes = {
		@Index(name = "idx_cervix_uitnodiging_geannuleerd_datum", columnList = "geannuleerdDatum"), @Index(name = "idx_cervix_uitnodiging_herinneren", columnList = "herinneren"),
		@Index(name = "idx_cervix_uitnodiging_monster_type", columnList = "monsterType"), @Index(name = "idx_cervix_uitnodiging_verstuurd", columnList = "verstuurd"),
		@Index(name = "idx_cervix_uitnodiging_herinneren_geannuleerd_datum", columnList = "herinnerenGeannuleerdDatum"),
		@Index(name = "idx_cervix_uitnodiging_trackid", columnList = "trackTraceId") })
@Audited
public class CervixUitnodiging extends InpakbareUitnodiging<CervixScreeningRonde>
{

	@ManyToOne(optional = false, fetch = FetchType.LAZY)
	private CervixScreeningRonde screeningRonde;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private CervixMonsterType monsterType;

	@OneToOne(optional = false, fetch = FetchType.LAZY)
	private CervixBrief brief;

	@OneToOne(fetch = FetchType.EAGER)
	private CervixMonster monster;

	@Temporal(TemporalType.TIMESTAMP)
	private Date geannuleerdDatum;

	@Column(nullable = false)
	private Boolean herinneren;

	@Temporal(TemporalType.TIMESTAMP)
	private Date herinnerenGeannuleerdDatum;

	@Column(nullable = false)
	private Boolean herinnering;

	@Column(nullable = false)
	private Boolean uitgesteld;

	@Column(nullable = false)
	@NotAudited
	private Boolean aangevraagdeHerdruk;

	@NotAudited
	private Boolean zasAangevraagdDoorClient;

	@Override
	public CervixScreeningRonde getScreeningRonde()
	{
		return screeningRonde;
	}

	@Override
	public void setScreeningRonde(CervixScreeningRonde screeningRonde)
	{
		this.screeningRonde = screeningRonde;
	}

	public CervixBrief getBrief()
	{
		return brief;
	}

	public void setBrief(CervixBrief brief)
	{
		this.brief = brief;
	}

	public CervixMonster getMonster()
	{
		return monster;
	}

	public void setMonster(CervixMonster monster)
	{
		this.monster = monster;
	}

	public CervixMonsterType getMonsterType()
	{
		return monsterType;
	}

	public void setMonsterType(CervixMonsterType monsterType)
	{
		this.monsterType = monsterType;
	}

	public Date getGeannuleerdDatum()
	{
		return geannuleerdDatum;
	}

	public void setGeannuleerdDatum(Date geannuleerdDatum)
	{
		this.geannuleerdDatum = geannuleerdDatum;
	}

	public Boolean getHerinneren()
	{
		return herinneren;
	}

	public void setHerinneren(Boolean herinneren)
	{
		this.herinneren = herinneren;
	}

	public Boolean getHerinnering()
	{
		return herinnering;
	}

	public void setHerinnering(Boolean herinnering)
	{
		this.herinnering = herinnering;
	}

	public Date getHerinnerenGeannuleerdDatum()
	{
		return herinnerenGeannuleerdDatum;
	}

	public void setHerinnerenGeannuleerdDatum(Date herinnerenGeannuleerdDatum)
	{
		this.herinnerenGeannuleerdDatum = herinnerenGeannuleerdDatum;
	}

	public Boolean getUitgesteld()
	{
		return uitgesteld;
	}

	public void setUitgesteld(Boolean uitgesteld)
	{
		this.uitgesteld = uitgesteld;
	}

	public Boolean getZasAangevraagdDoorClient()
	{
		return zasAangevraagdDoorClient;
	}

	public void setZasAangevraagdDoorClient(Boolean zasAangevraagdDoorClient)
	{
		this.zasAangevraagdDoorClient = zasAangevraagdDoorClient;
	}

	public Boolean getAangevraagdeHerdruk()
	{
		return aangevraagdeHerdruk;
	}

	public void setAangevraagdeHerdruk(Boolean aangevraagdeHerdruk)
	{
		this.aangevraagdeHerdruk = aangevraagdeHerdruk;
	}

}
