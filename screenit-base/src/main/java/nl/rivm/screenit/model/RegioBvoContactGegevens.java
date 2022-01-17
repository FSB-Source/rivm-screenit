package nl.rivm.screenit.model;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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
import javax.persistence.OneToOne;
import javax.persistence.Table;

import nl.rivm.screenit.model.helper.HibernateMagicNumber;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;
import nl.topicuszorg.organisatie.model.Adres;

import org.hibernate.envers.Audited;
import org.hibernate.envers.NotAudited;

@Entity(name = "regio_bvo_contact_gegevens")
@Table(schema = "algemeen")
@Audited
public class RegioBvoContactGegevens extends AbstractHibernateObject
{

	private static final long serialVersionUID = 1L;

	@Column(length = HibernateMagicNumber.L20)
	private String telefoon;

	@Column(length = HibernateMagicNumber.L100)
	private String email;

	@Column(length = HibernateMagicNumber.L256)
	private String clientPortaalVrijeTekst;

	@NotAudited
	@OneToOne(fetch = FetchType.LAZY, cascade = { javax.persistence.CascadeType.ALL })
	private Adres antwoordnummerAdres;

	@NotAudited
	@OneToOne(fetch = FetchType.LAZY, cascade = { javax.persistence.CascadeType.ALL })
	private Adres postbusnummerAdres;

	public RegioBvoContactGegevens()
	{
	}

	public String getTelefoon()
	{
		return telefoon;
	}

	public void setTelefoon(String telefoon)
	{
		this.telefoon = telefoon;
	}

	public String getEmail()
	{
		return email;
	}

	public void setEmail(String email)
	{
		this.email = email;
	}

	public String getClientPortaalVrijeTekst()
	{
		return clientPortaalVrijeTekst;
	}

	public void setClientPortaalVrijeTekst(String openingstijdentekst)
	{
		this.clientPortaalVrijeTekst = openingstijdentekst;
	}

	public Adres getAntwoordnummerAdres()
	{
		return antwoordnummerAdres;
	}

	public void setAntwoordnummerAdres(Adres antwoordnummer)
	{
		this.antwoordnummerAdres = antwoordnummer;
	}

	public Adres getPostbusnummerAdres()
	{
		return postbusnummerAdres;
	}

	public void setPostbusnummerAdres(Adres postbusnummer)
	{
		this.postbusnummerAdres = postbusnummer;
	}
}
