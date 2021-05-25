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

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import nl.rivm.screenit.model.ScreenitAdres;
import nl.rivm.screenit.util.DiffSpecs;

import org.hibernate.envers.Audited;

@Entity
@Audited
@Table
public class MammaTehuisAdres extends ScreenitAdres
{

	@Column(nullable = true)
	private Boolean locatieVanTehuis = true;

	@ManyToOne(optional = true, fetch = FetchType.LAZY)
	@DiffSpecs(displayProperty = "naam")
	private MammaTehuis tehuis;

	public MammaTehuis getTehuis()
	{
		return tehuis;
	}

	public void setTehuis(MammaTehuis tehuis)
	{
		this.tehuis = tehuis;
	}

	public Boolean getLocatieVanTehuis()
	{
		return locatieVanTehuis;
	}

	public void setLocatieVanTehuis(Boolean locatieVanTehuis)
	{
		this.locatieVanTehuis = locatieVanTehuis;
	}
}
