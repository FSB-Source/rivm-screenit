
package nl.rivm.screenit.model.overeenkomsten;

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
import java.util.List;

import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

import nl.rivm.screenit.model.IActief;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.UploadDocument;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;

@Entity
@Table(schema = "gedeeld")
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "organisatie.cache")
public class Overeenkomst extends AbstractHibernateObject implements IActief
{

	private static final long serialVersionUID = 1L;

	@Enumerated(EnumType.STRING)
	private OvereenkomstType overeenkomst;

	private String naam;

	@ManyToOne
	private UploadDocument document;

	@Temporal(TemporalType.TIMESTAMP)
	private Date laatsteUpdateDocument;

	private Boolean actief = Boolean.TRUE;

	@OneToMany(mappedBy = "overeenkomst")
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "organisatie.cache")
	private List<AbstractAfgeslotenOvereenkomst> afgeslotenOvereenkomsten;

	@Enumerated(EnumType.STRING)
	private OrganisatieType organisatieType;

	public OvereenkomstType getOvereenkomst()
	{
		return overeenkomst;
	}

	public void setOvereenkomst(OvereenkomstType overeenkomst)
	{
		this.overeenkomst = overeenkomst;
	}

	public String getNaam()
	{
		return naam;
	}

	public void setNaam(String naam)
	{
		this.naam = naam;
	}

	public UploadDocument getDocument()
	{
		return document;
	}

	public void setDocument(UploadDocument document)
	{
		this.document = document;
	}

	public Date getLaatsteUpdateDocument()
	{
		return laatsteUpdateDocument;
	}

	public void setLaatsteUpdateDocument(Date laatsteUpdateDocument)
	{
		this.laatsteUpdateDocument = laatsteUpdateDocument;
	}

	public boolean isActief()
	{
		return actief;
	}

	public void setActief(boolean actief)
	{
		this.actief = actief;
	}

	@Override
	public Boolean getActief()
	{
		return actief;
	}

	@Override
	public void setActief(Boolean actief)
	{
		this.actief = actief;
	}

	public List<AbstractAfgeslotenOvereenkomst> getAfgeslotenOvereenkomsten()
	{
		return afgeslotenOvereenkomsten;
	}

	public void setAfgeslotenOvereenkomsten(List<AbstractAfgeslotenOvereenkomst> afgeslotenOvereenkomsten)
	{
		this.afgeslotenOvereenkomsten = afgeslotenOvereenkomsten;
	}

	public OrganisatieType getOrganisatieType()
	{
		return organisatieType;
	}

	public void setOrganisatieType(OrganisatieType organisatieType)
	{
		this.organisatieType = organisatieType;
	}
}
