package nl.rivm.screenit.model;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

import javax.persistence.CascadeType;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.envers.Audited;

@Entity
@Table(schema = "algemeen")
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
@Audited
public class ClientContactActie extends SingleTableHibernateObject
{

	private static final long serialVersionUID = 1L;

	@ManyToOne(fetch = FetchType.EAGER, cascade = CascadeType.ALL)
	private ClientContact contact;

	@Enumerated(EnumType.STRING)
	private ClientContactActieType type;

	@Enumerated(EnumType.STRING)
	private RedenOpnieuwAanvragenClientgegevens opnieuwAanvragenClientgegevensReden;

	public ClientContactActie()
	{

	}

	public ClientContactActie(ClientContactActieType type)
	{
		this.type = type;
	}

	public ClientContact getContact()
	{
		return contact;
	}

	public void setContact(ClientContact contact)
	{
		this.contact = contact;
	}

	public ClientContactActieType getType()
	{
		return type;
	}

	public void setType(ClientContactActieType type)
	{
		this.type = type;
	}

	public RedenOpnieuwAanvragenClientgegevens getOpnieuwAanvragenClientgegevensReden()
	{
		return opnieuwAanvragenClientgegevensReden;
	}

	public void setOpnieuwAanvragenClientgegevensReden(RedenOpnieuwAanvragenClientgegevens opnieuwAanvragenClientgegevensReden)
	{
		this.opnieuwAanvragenClientgegevensReden = opnieuwAanvragenClientgegevensReden;
	}
}
