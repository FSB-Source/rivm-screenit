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

import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.enums.ToegangLevel;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.apache.shiro.authz.Permission;
import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;

@Entity
@Table(schema = "gedeeld")
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "organisatie.cache")
public class Permissie extends AbstractHibernateObject implements Permission
{
	
	private static final long serialVersionUID = 1L;

	@Enumerated(EnumType.STRING)
	private Actie actie;

	@Enumerated(EnumType.STRING)
	private Recht recht;

	@Enumerated(EnumType.STRING)
	private ToegangLevel toegangLevel;

	@ManyToOne(fetch = FetchType.EAGER)
	private Rol rol;

	private Boolean actief = true;

	public Permissie()
	{

	}

	public Permissie(Rol rol)
	{
		this.rol = rol;
	}

	public Permissie(Actie actie, Recht recht, ToegangLevel toegangLevel)
	{
		this.actie = actie;
		this.recht = recht;
		this.toegangLevel = toegangLevel;
	}

	public Permissie(Rol rol, Actie actie, Recht recht, ToegangLevel toegangLevel)
	{
		this.rol = rol;
		this.actie = actie;
		this.recht = recht;
		this.toegangLevel = toegangLevel;
	}

	public Actie getActie()
	{
		return actie;
	}

	public void setActie(Actie actie)
	{
		this.actie = actie;
	}

	public Recht getRecht()
	{
		return recht;
	}

	public void setRecht(Recht recht)
	{
		this.recht = recht;
	}

	public ToegangLevel getToegangLevel()
	{
		return toegangLevel;
	}

	public void setToegangLevel(ToegangLevel toegangLevel)
	{
		this.toegangLevel = toegangLevel;
	}

	@Override
	public boolean implies(Permission p)
	{
		if (p instanceof Permissie)
		{
			Permissie permissie = (Permissie) p;
			if (permissie.getRecht().equals(recht))
			{
				if (permissie.getActie() == null || permissie.getActie().getNiveau() <= actie.getNiveau()) 
				{
					if (permissie.getToegangLevel() == null 
						|| permissie.getToegangLevel().getNiveau() <= toegangLevel.getNiveau())
					{
						return true;
					}
				}
			}
		}
		return false;
	}

	@Override
	public String toString()
	{
		return getRecht().name();
	}

	public Boolean getActief()
	{
		return actief;
	}

	public void setActief(Boolean actief)
	{
		this.actief = actief;
	}

	public Rol getRol()
	{
		return rol;
	}

	public void setRol(Rol rol)
	{
		this.rol = rol;
	}

}
