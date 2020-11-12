package nl.rivm.screenit.model;

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
import java.util.List;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;
import javax.persistence.Transient;

import nl.rivm.screenit.model.cervix.CervixDossier;
import nl.rivm.screenit.model.colon.ColonDossier;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.mamma.MammaDossier;

import org.hibernate.Hibernate;
import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.envers.Audited;

@Entity
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
@Audited
public abstract class Dossier<SR extends ScreeningRonde<?, ?, ?, ?>, AF extends Afmelding<?, ?, ?>> extends TablePerClassHibernateObject
{
	
	private static final long serialVersionUID = 1L;

	@Enumerated(EnumType.STRING)
	private DossierStatus status;

	@Column(nullable = false)
	private Boolean aangemeld;

	@Temporal(TemporalType.DATE)
	private Date inactiefVanaf;

	@Temporal(TemporalType.DATE)
	private Date inactiefTotMet;

	public abstract Client getClient();

	public abstract void setClient(Client client);

	public abstract List<SR> getScreeningRondes();

	public abstract void setScreeningRondes(List<SR> screeningRondes);

	public abstract SR getLaatsteScreeningRonde();

	public abstract void setLaatsteScreeningRonde(SR laatsteScreeningRonde);

	public abstract List<AF> getAfmeldingen();

	public abstract void setAfmeldingen(List<AF> afmeldingen);

	public abstract AF getLaatsteAfmelding();

	public abstract void setLaatsteAfmelding(AF laatsteAfmelding);

	public DossierStatus getStatus()
	{
		return status;
	}

	public void setStatus(DossierStatus status)
	{
		this.status = status;
	}

	public Boolean getAangemeld()
	{
		return aangemeld;
	}

	public void setAangemeld(Boolean aangemeld)
	{
		this.aangemeld = aangemeld;
	}

	public Date getInactiefVanaf()
	{
		return inactiefVanaf;
	}

	public void setInactiefVanaf(Date inactiefVanaf)
	{
		this.inactiefVanaf = inactiefVanaf;
	}

	@Transient
	public Bevolkingsonderzoek getBevolkingsonderzoek()
	{
		Class aClass = Hibernate.getClass(this);
		Bevolkingsonderzoek bevolkingsonderzoek = null;
		if (aClass == ColonDossier.class)
		{
			bevolkingsonderzoek = Bevolkingsonderzoek.COLON;
		}
		else if (aClass == CervixDossier.class)
		{
			bevolkingsonderzoek = Bevolkingsonderzoek.CERVIX;
		}
		else if (aClass == MammaDossier.class)
		{
			bevolkingsonderzoek = Bevolkingsonderzoek.MAMMA;
		}
		return bevolkingsonderzoek;
	}

	public Date getInactiefTotMet()
	{
		return inactiefTotMet;
	}

	public void setInactiefTotMet(Date inactiefTotMet)
	{
		this.inactiefTotMet = inactiefTotMet;
	}
}
