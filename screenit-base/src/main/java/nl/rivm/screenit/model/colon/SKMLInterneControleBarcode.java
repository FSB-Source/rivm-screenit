
package nl.rivm.screenit.model.colon;

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

import javax.persistence.Entity;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import nl.rivm.screenit.model.colon.enums.IFOBTUitslagType;

@Entity
@Table(schema = "colon")
public class SKMLInterneControleBarcode extends SKMLControleBarcode
{

	private static final long serialVersionUID = 1L;

	@ManyToOne(optional = false)
	private IFobtLaboratorium laboratorium;

	private Integer volgorde;

	private Integer qbaseId;

	private String controleTekst;

	public SKMLInterneControleBarcode()
	{
		super();
		setType(IFOBTUitslagType.INTERN);
	}

	public IFobtLaboratorium getLaboratorium()
	{
		return laboratorium;
	}

	public void setLaboratorium(IFobtLaboratorium laboratorium)
	{
		this.laboratorium = laboratorium;
	}

	public Integer getQbaseId()
	{
		return qbaseId;
	}

	public void setQbaseId(Integer qbaseId)
	{
		this.qbaseId = qbaseId;
	}

	public Integer getVolgorde()
	{
		return volgorde;
	}

	public void setVolgorde(Integer volgorde)
	{
		this.volgorde = volgorde;
	}

	public String getControleTekst()
	{
		return controleTekst;
	}

	public void setControleTekst(String controleTekst)
	{
		this.controleTekst = controleTekst;
	}

}
