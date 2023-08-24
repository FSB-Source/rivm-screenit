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

import javax.persistence.Column;
import javax.persistence.Entity;

import nl.rivm.screenit.model.helper.HibernateMagicNumber;

import org.hibernate.envers.Audited;

@Entity
@Audited
public class CentraleEenheid extends Instelling
{
	private static final long serialVersionUID = 1L;

	@Column(length = HibernateMagicNumber.L20)
	private String telefoon3;

	@Column(length = HibernateMagicNumber.L20)
	private String telefoon4;

	@Column(length = 100)
	private String email2;

	@Column(length = 100)
	private String email3;

	@Column(length = 100)
	private String email4;

	@Column(length = HibernateMagicNumber.L256)
	private String clientPortaalVrijeTekst;

	public String getTelefoon3()
	{
		return telefoon3;
	}

	public void setTelefoon3(String telefoon3)
	{
		this.telefoon3 = telefoon3;
	}

	public String getTelefoon4()
	{
		return telefoon4;
	}

	public void setTelefoon4(String telefoon4)
	{
		this.telefoon4 = telefoon4;
	}

	public String getEmail2()
	{
		return email2;
	}

	public void setEmail2(String email2)
	{
		this.email2 = email2;
	}

	public String getEmail3()
	{
		return email3;
	}

	public void setEmail3(String email3)
	{
		this.email3 = email3;
	}

	public String getEmail4()
	{
		return email4;
	}

	public void setEmail4(String email4)
	{
		this.email4 = email4;
	}

	public String getClientPortaalVrijeTekst()
	{
		return clientPortaalVrijeTekst;
	}

	public void setClientPortaalVrijeTekst(String clientPortaalVrijeTekst)
	{
		this.clientPortaalVrijeTekst = clientPortaalVrijeTekst;
	}
}
