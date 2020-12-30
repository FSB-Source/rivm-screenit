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
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.Table;

import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;

@Entity
@Table(schema = "gedeeld")
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
public class ClientTooltip extends AbstractHibernateObject
{

	private static final long serialVersionUID = 1L;

	@Column(nullable = false)
	private String titel;

	@Column(nullable = false)
	private String tekst;

	@Enumerated(EnumType.STRING)
	@Column(nullable = false)
	private ClientTooltipType type;

	@Column(nullable = false)
	private Date aangepast;

	public String getTitel()
	{
		return titel;
	}

	public void setTitel(String titel)
	{
		this.titel = titel;
	}

	public String getTekst()
	{
		return tekst;
	}

	public void setTekst(String tekst)
	{
		this.tekst = tekst;
	}

	public ClientTooltipType getType()
	{
		return type;
	}

	public void setType(ClientTooltipType type)
	{
		this.type = type;
	}

	public Date getAangepast()
	{
		return aangepast;
	}

	public void setAangepast(Date aangepast)
	{
		this.aangepast = aangepast;
	}

}
