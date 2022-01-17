package nl.rivm.screenit.model.cervix.cis;

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

import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.Lob;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.annotations.Type;

@Entity
@Table(schema = "cervix", name = "cis_historie_ongestructureerd_regel")
public class CervixCISHistorieOngestructureerdRegel extends AbstractHibernateObject
{

	private static final long serialVersionUID = 1L;

	@ManyToOne(optional = false, fetch = FetchType.LAZY)
	private CervixCISHistorie cisHistorie;

	@Temporal(TemporalType.TIMESTAMP)
	private Date datum;

	private String ronde;

	@Column(nullable = false)
	@Lob
	@Type(type = "org.hibernate.type.TextType")
	private String tekst;

	public CervixCISHistorie getCisHistorie()
	{
		return cisHistorie;
	}

	public void setCisHistorie(CervixCISHistorie cisHistorie)
	{
		this.cisHistorie = cisHistorie;
	}

	public Date getDatum()
	{
		return datum;
	}

	public void setDatum(Date datum)
	{
		this.datum = datum;
	}

	public String getRonde()
	{
		return ronde;
	}

	public void setRonde(String ronde)
	{
		this.ronde = ronde;
	}

	public String getTekst()
	{
		return tekst;
	}

	public void setTekst(String tekst)
	{
		this.tekst = tekst;
	}
}
