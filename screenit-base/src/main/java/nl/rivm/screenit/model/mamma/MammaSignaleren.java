package nl.rivm.screenit.model.mamma;

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
import javax.persistence.FetchType;
import javax.persistence.ManyToOne;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;
import javax.persistence.UniqueConstraint;

import nl.rivm.screenit.model.InstellingGebruiker;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.annotations.Cascade;
import org.hibernate.annotations.CascadeType;
import org.hibernate.envers.Audited;

@Entity
@Table(
	schema = "mamma",
	name = "signaleren",
	uniqueConstraints = { @UniqueConstraint(columnNames = "rechts_verticale_doorsnede"), @UniqueConstraint(columnNames = "links_verticale_doorsnede"),
		@UniqueConstraint(columnNames = "rechts_horizontale_doorsnede"), @UniqueConstraint(columnNames = "links_horizontale_doorsnede") })
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
@Audited
public class MammaSignaleren extends AbstractHibernateObject
{
	private static final long serialVersionUID = 1L;

	@OneToOne(optional = false, mappedBy = "signaleren", fetch = FetchType.LAZY)
	private MammaOnderzoek onderzoek;

	@OneToOne(fetch = FetchType.LAZY)
	@Cascade({ CascadeType.DELETE, CascadeType.SAVE_UPDATE })
	private MammaAnnotatieAfbeelding rechtsVerticaleDoorsnede;

	@OneToOne(fetch = FetchType.LAZY)
	@Cascade({ CascadeType.DELETE, CascadeType.SAVE_UPDATE })
	private MammaAnnotatieAfbeelding linksVerticaleDoorsnede;

	@OneToOne(fetch = FetchType.LAZY)
	@Cascade({ CascadeType.DELETE, CascadeType.SAVE_UPDATE })
	private MammaAnnotatieAfbeelding rechtsHorizontaleDoorsnede;

	@OneToOne(fetch = FetchType.LAZY)
	@Cascade({ CascadeType.DELETE, CascadeType.SAVE_UPDATE })
	private MammaAnnotatieAfbeelding linksHorizontaleDoorsnede;

	@ManyToOne(optional = true, fetch = FetchType.LAZY)
	private InstellingGebruiker afgerondDoor;

	@Temporal(TemporalType.TIMESTAMP)
	@Column(nullable = true)
	private Date afgerondOp;

	@Column(nullable = false)
	private Boolean heeftAfwijkingen;

	public MammaOnderzoek getOnderzoek()
	{
		return onderzoek;
	}

	public void setOnderzoek(MammaOnderzoek onderzoek)
	{
		this.onderzoek = onderzoek;
	}

	public MammaAnnotatieAfbeelding getRechtsVerticaleDoorsnede()
	{
		return rechtsVerticaleDoorsnede;
	}

	public void setRechtsVerticaleDoorsnede(MammaAnnotatieAfbeelding rechtsVerticaleDoorsnede)
	{
		this.rechtsVerticaleDoorsnede = rechtsVerticaleDoorsnede;
	}

	public MammaAnnotatieAfbeelding getLinksVerticaleDoorsnede()
	{
		return linksVerticaleDoorsnede;
	}

	public void setLinksVerticaleDoorsnede(MammaAnnotatieAfbeelding linksVerticaleDoorsnede)
	{
		this.linksVerticaleDoorsnede = linksVerticaleDoorsnede;
	}

	public MammaAnnotatieAfbeelding getRechtsHorizontaleDoorsnede()
	{
		return rechtsHorizontaleDoorsnede;
	}

	public void setRechtsHorizontaleDoorsnede(MammaAnnotatieAfbeelding rechtsHorizontaleDoorsnede)
	{
		this.rechtsHorizontaleDoorsnede = rechtsHorizontaleDoorsnede;
	}

	public MammaAnnotatieAfbeelding getLinksHorizontaleDoorsnede()
	{
		return linksHorizontaleDoorsnede;
	}

	public void setLinksHorizontaleDoorsnede(MammaAnnotatieAfbeelding linksHorizontaleDoorsnede)
	{
		this.linksHorizontaleDoorsnede = linksHorizontaleDoorsnede;
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

	public Boolean getHeeftAfwijkingen()
	{
		return heeftAfwijkingen;
	}

	public void setHeeftAfwijkingen(Boolean heeftAfwijkingen)
	{
		this.heeftAfwijkingen = heeftAfwijkingen;
	}
}
