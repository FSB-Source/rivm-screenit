package nl.rivm.screenit.model.mamma;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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
import javax.persistence.UniqueConstraint;

import nl.rivm.screenit.model.TijdelijkAdres;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.util.DiffSpecs;

import org.hibernate.envers.Audited;

@Entity
@Audited
@Table(uniqueConstraints = { @UniqueConstraint(columnNames = "standplaats_locatie_bijlage") })
public class MammaStandplaatsLocatie extends TijdelijkAdres
{
	@OneToOne(optional = true, fetch = FetchType.LAZY)
	@DiffSpecs(displayProperty = "naam")
	private UploadDocument standplaatsLocatieBijlage;

	@Column(nullable = true)
	private Boolean brievenApartPrinten;

	@Column(nullable = true)
	private Boolean toonHuisnummerInBrieven;

	public UploadDocument getStandplaatsLocatieBijlage()
	{
		return standplaatsLocatieBijlage;
	}

	public void setStandplaatsLocatieBijlage(UploadDocument standplaatsLocatieBijlage)
	{
		this.standplaatsLocatieBijlage = standplaatsLocatieBijlage;
	}

	public Boolean getToonHuisnummerInBrieven()
	{
		return toonHuisnummerInBrieven;
	}

	public void setToonHuisnummerInBrieven(Boolean toonHuisnummerInBrieven)
	{
		this.toonHuisnummerInBrieven = toonHuisnummerInBrieven;
	}

	public Boolean getBrievenApartPrinten()
	{
		return brievenApartPrinten;
	}

	public void setBrievenApartPrinten(Boolean briefApartPrinten)
	{
		this.brievenApartPrinten = briefApartPrinten;
	}
}
