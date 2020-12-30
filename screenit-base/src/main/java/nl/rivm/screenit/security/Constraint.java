
package nl.rivm.screenit.security;

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

import java.io.Serializable;
import java.util.List;

import nl.rivm.screenit.model.Permissie;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.enums.ToegangLevel;

public class Constraint extends Permissie
{

	private static final long serialVersionUID = 1L;

	private Class<?> scopeObjectClass;

	private Serializable scopeObjectId;

	private boolean checkScope;

	private List<Bevolkingsonderzoek> bevolkingsonderzoek;

	public Constraint()
	{
		super();
	}

	public Constraint(Actie actie, Recht recht, ToegangLevel toegangLevel, Class<?> scopeObjectClass, Serializable scopeObjectId, boolean checkScope)
	{
		super(actie, recht, toegangLevel);
		this.scopeObjectClass = scopeObjectClass;
		this.scopeObjectId = scopeObjectId;
		this.checkScope = checkScope;
	}

	public Class<?> getScopeObjectClass()
	{
		return scopeObjectClass;
	}

	public void setScopeObjectClass(Class<?> scopeObjectClass)
	{
		this.scopeObjectClass = scopeObjectClass;
	}

	public Serializable getScopeObjectId()
	{
		return scopeObjectId;
	}

	public void setScopeObjectId(Serializable scopeObjectId)
	{
		this.scopeObjectId = scopeObjectId;
	}

	public boolean isCheckScope()
	{
		return checkScope;
	}

	public void setCheckScope(boolean checkScope)
	{
		this.checkScope = checkScope;
	}

	public List<Bevolkingsonderzoek> getBevolkingsonderzoek()
	{
		return bevolkingsonderzoek;
	}

	public void setBevolkingsonderzoek(List<Bevolkingsonderzoek> bevolkingsonderzoek)
	{
		this.bevolkingsonderzoek = bevolkingsonderzoek;
	}
}
