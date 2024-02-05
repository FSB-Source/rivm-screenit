
package nl.rivm.screenit.model;

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

import java.util.List;

import nl.rivm.screenit.model.IActief;
import nl.rivm.screenit.model.MergedBrieven;
import nl.rivm.screenit.model.batch.BvoZoekCriteria;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;

public class MergedBrievenFilter<MB extends MergedBrieven<?>> extends BvoZoekCriteria implements IActief
{

	private static final long serialVersionUID = 1L;

	private Boolean controle;

	private Boolean geprint;

	private Class<MB> mergedBrievenClass;

	public void setControle(Boolean controle)
	{
		this.controle = controle;
	}

	public Boolean getControle()
	{
		return controle;
	}

	public void setGeprint(Boolean geprint)
	{
		this.geprint = geprint;
	}

	public Boolean setGeprint()
	{
		return geprint;
	}

	@Override
	public Boolean getActief()
	{
		return geprint;
	}

	@Override
	public void setActief(Boolean actief)
	{
		this.geprint = actief;
	}

	public Class<MB> getMergedBrievenClass()
	{
		return mergedBrievenClass;
	}

	public void setMergedBrievenClass(Class<MB> mergedBrievenClass)
	{
		this.mergedBrievenClass = mergedBrievenClass;
	}

	@Override
	public List<Bevolkingsonderzoek> getBevolkingsonderzoeken()
	{
		return null;
	}
}
