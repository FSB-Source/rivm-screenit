
package nl.rivm.screenit.main.model;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import nl.rivm.screenit.model.enums.GebeurtenisBron;

import java.io.Serializable;
import java.util.Date;

public abstract class DossierGebeurtenis implements Serializable
{

	private static final long serialVersionUID = 1L;

	private Date tijd;

	private DossierGebeurtenisType dossierGebeurtenisType;

	private GebeurtenisBron bron;

	public DossierGebeurtenis()
	{

	}

	public DossierGebeurtenis(Date tijd)
	{
		this.tijd = tijd;
	}

	public Date getTijd()
	{
		return tijd;
	}

	public void setTijd(Date tijd)
	{
		this.tijd = tijd;
	}

	public DossierGebeurtenisType getDossierGebeurtenisType()
	{
		return dossierGebeurtenisType;
	}

	public void setDossierGebeurtenisType(DossierGebeurtenisType dossierGebeurtenisType)
	{
		this.dossierGebeurtenisType = dossierGebeurtenisType;
	}

	public GebeurtenisBron getBron()
	{
		return bron;
	}

	public void setBron(GebeurtenisBron bron)
	{
		this.bron = bron;
	}

}
