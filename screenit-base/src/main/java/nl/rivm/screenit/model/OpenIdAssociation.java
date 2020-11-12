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

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;

import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

@Entity(name = "openidassociation")
@Table(schema = "algemeen")
public class OpenIdAssociation extends AbstractHibernateObject
{
	
	private static final long serialVersionUID = 1L;

	@Column(unique = true, nullable = false)
	private String handle;

	@Column(nullable = false)
	private String type;

	@Column(nullable = false)
	private String mackey;

	@Column(nullable = false)
	private Date expdate;

	public String getHandle()
	{
		return handle;
	}

	public void setHandle(String handle)
	{
		this.handle = handle;
	}

	public String getType()
	{
		return type;
	}

	public void setType(String type)
	{
		this.type = type;
	}

	public String getMackey()
	{
		return mackey;
	}

	public void setMackey(String mackey)
	{
		this.mackey = mackey;
	}

	public Date getExpdate()
	{
		return expdate;
	}

	public void setExpdate(Date expdate)
	{
		this.expdate = expdate;
	}
}
