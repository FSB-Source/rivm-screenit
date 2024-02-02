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

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.OneToOne;
import javax.persistence.Table;

import org.hibernate.envers.Audited;

@Entity
@Table(schema = "mamma", name = "kansberekening_screening_ronde_event")
@Audited
public class MammaKansberekeningScreeningRondeEvent extends MammaKansberekeningEvent
{
	private static final long serialVersionUID = 1L;

	@OneToOne(optional = true, fetch = FetchType.LAZY, mappedBy = "screeningRondeEvent")
	private MammaDossier dossier;

	@OneToOne(optional = true, fetch = FetchType.LAZY, mappedBy = "screeningRondeEvent")
	private MammaScreeningRonde screeningRonde;

	@Column(nullable = true)
	private Boolean deelname;

	@Column(nullable = false)
	private Boolean isSuspect;

	public MammaScreeningRonde getScreeningRonde()
	{
		return screeningRonde;
	}

	public void setScreeningRonde(MammaScreeningRonde screeningRonde)
	{
		this.screeningRonde = screeningRonde;
	}

	public Boolean getDeelname()
	{
		return deelname;
	}

	public void setDeelname(Boolean deelname)
	{
		this.deelname = deelname;
	}

	public MammaDossier getDossier()
	{
		return dossier;
	}

	public void setDossier(MammaDossier dossier)
	{
		this.dossier = dossier;
	}

	public Boolean getSuspect()
	{
		return isSuspect;
	}

	public void setSuspect(Boolean suspect)
	{
		isSuspect = suspect;
	}
}
