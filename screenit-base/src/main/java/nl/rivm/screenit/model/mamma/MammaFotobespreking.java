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

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

import nl.rivm.screenit.model.BeoordelingsEenheid;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.SingleTableHibernateObject;
import nl.rivm.screenit.model.helper.HibernateMagicNumber;
import nl.rivm.screenit.model.mamma.enums.MammaFotobesprekingType;
import nl.rivm.screenit.model.mamma.enums.MammobridgeRole;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.annotations.Cascade;
import org.hibernate.annotations.CascadeType;
import org.hibernate.envers.Audited;
import org.hibernate.envers.NotAudited;

@Entity
@Table(schema = "mamma", name = "fotobespreking")
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "mamma.cache")
@Audited
public class MammaFotobespreking extends SingleTableHibernateObject implements MammaIKwaliteitscontrole
{
	@Column(nullable = false, length = HibernateMagicNumber.L256, unique = true)
	private String omschrijving;

	@Column(nullable = false)
	@Temporal(TemporalType.TIMESTAMP)
	private Date aangemaaktOp;

	@ManyToOne(optional = false, fetch = FetchType.LAZY)
	@NotAudited
	private InstellingGebruiker aangemaaktDoor;

	@ManyToOne(optional = true, fetch = FetchType.LAZY)
	private BeoordelingsEenheid beoordelingsEenheid;

	@ManyToOne(optional = true, fetch = FetchType.LAZY)
	private MammaScreeningsEenheid screeningsEenheid;

	@Column(nullable = true)
	@Temporal(TemporalType.TIMESTAMP)
	private Date gestartOp;

	@OneToMany(fetch = FetchType.LAZY, mappedBy = "fotobespreking", cascade = { javax.persistence.CascadeType.REMOVE })
	@Cascade(CascadeType.DELETE)
	private List<MammaFotobesprekingOnderzoek> onderzoeken = new ArrayList<>();

	@Column(nullable = true)
	@Temporal(TemporalType.TIMESTAMP)
	private Date afgerondOp;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private MammaFotobesprekingType type;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private MammobridgeRole role;

	public BeoordelingsEenheid getBeoordelingsEenheid()
	{
		return beoordelingsEenheid;
	}

	public void setBeoordelingsEenheid(BeoordelingsEenheid beoordelingsEenheid)
	{
		this.beoordelingsEenheid = beoordelingsEenheid;
	}

	public MammaScreeningsEenheid getScreeningsEenheid()
	{
		return screeningsEenheid;
	}

	public void setScreeningsEenheid(MammaScreeningsEenheid screeningsEenheid)
	{
		this.screeningsEenheid = screeningsEenheid;
	}

	public MammaFotobesprekingType getType()
	{
		return type;
	}

	public void setType(MammaFotobesprekingType type)
	{
		this.type = type;
	}

	public String getOmschrijving()
	{
		return omschrijving;
	}

	public void setOmschrijving(String omschrijving)
	{
		this.omschrijving = omschrijving;
	}

	public Date getAfgerondOp()
	{
		return afgerondOp;
	}

	@Override
	public void setAfgerondOp(Date afgerondOp)
	{
		this.afgerondOp = afgerondOp;
	}

	public Date getAangemaaktOp()
	{
		return aangemaaktOp;
	}

	public void setAangemaaktOp(Date aangemaaktOp)
	{
		this.aangemaaktOp = aangemaaktOp;
	}

	public InstellingGebruiker getAangemaaktDoor()
	{
		return aangemaaktDoor;
	}

	public void setAangemaaktDoor(InstellingGebruiker aangemaaktDoor)
	{
		this.aangemaaktDoor = aangemaaktDoor;
	}

	public Date getGestartOp()
	{
		return gestartOp;
	}

	@Override
	public void setGestartOp(Date gestartOp)
	{
		this.gestartOp = gestartOp;
	}

	public List<MammaFotobesprekingOnderzoek> getOnderzoeken()
	{
		return onderzoeken;
	}

	public void setOnderzoeken(List<MammaFotobesprekingOnderzoek> onderzoeken)
	{
		this.onderzoeken = onderzoeken;
	}

	public MammobridgeRole getRole()
	{
		return role;
	}

	public void setRole(MammobridgeRole role)
	{
		this.role = role;
	}
}
