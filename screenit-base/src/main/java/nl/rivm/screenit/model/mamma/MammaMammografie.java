package nl.rivm.screenit.model.mamma;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.mamma.enums.MammaMammografieIlmStatus;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.annotations.Cascade;
import org.hibernate.annotations.CascadeType;
import org.hibernate.envers.Audited;

@Entity
@Table(
	schema = "mamma",
	name = "mammografie",
	uniqueConstraints = { @UniqueConstraint(columnNames = "visuele_inspectie_afbeelding") })
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
@Audited
public class MammaMammografie extends AbstractHibernateObject
{

	@OneToOne(optional = false, mappedBy = "mammografie", fetch = FetchType.LAZY)
	private MammaOnderzoek onderzoek;

	@OneToOne(fetch = FetchType.LAZY, cascade = { javax.persistence.CascadeType.REMOVE, javax.persistence.CascadeType.PERSIST, javax.persistence.CascadeType.MERGE })
	@Cascade({ CascadeType.DELETE, CascadeType.SAVE_UPDATE })
	private MammaAnnotatieAfbeelding visueleInspectieAfbeelding;

	@ManyToOne(optional = true, fetch = FetchType.LAZY)
	private InstellingGebruiker afgerondDoor;

	@Temporal(TemporalType.TIMESTAMP)
	@Column(nullable = true)
	private Date afgerondOp;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private MammaMammografieIlmStatus ilmStatus;

	@Column(nullable = false)
	@Temporal(TemporalType.TIMESTAMP)
	private Date ilmStatusDatum;

	public MammaOnderzoek getOnderzoek()
	{
		return onderzoek;
	}

	public void setOnderzoek(MammaOnderzoek onderzoek)
	{
		this.onderzoek = onderzoek;
	}

	public MammaAnnotatieAfbeelding getVisueleInspectieAfbeelding()
	{
		return visueleInspectieAfbeelding;
	}

	public void setVisueleInspectieAfbeelding(MammaAnnotatieAfbeelding visueleInspectieAfbeelding)
	{
		this.visueleInspectieAfbeelding = visueleInspectieAfbeelding;
	}

	public InstellingGebruiker getAfgerondDoor()
	{
		return afgerondDoor;
	}

	public void setAfgerondDoor(InstellingGebruiker afgerondDoor)
	{
		this.afgerondDoor = afgerondDoor;
	}

	public Date getAfgerondOp()
	{
		return afgerondOp;
	}

	public void setAfgerondOp(Date afgerondOp)
	{
		this.afgerondOp = afgerondOp;
	}

	public MammaMammografieIlmStatus getIlmStatus()
	{
		return ilmStatus;
	}

	public void setIlmStatus(MammaMammografieIlmStatus ilmStatus)
	{
		this.ilmStatus = ilmStatus;
	}

	public Date getIlmStatusDatum()
	{
		return ilmStatusDatum;
	}

	public void setIlmStatusDatum(Date ilmStatusDatum)
	{
		this.ilmStatusDatum = ilmStatusDatum;
	}
}
