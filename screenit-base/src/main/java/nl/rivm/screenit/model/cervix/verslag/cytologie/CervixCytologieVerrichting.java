package nl.rivm.screenit.model.cervix.verslag.cytologie;

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

import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

import nl.rivm.screenit.model.verslag.VraagElement;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import com.fasterxml.jackson.annotation.JsonIgnore;

@Entity
@Table(schema = "cervix")
public class CervixCytologieVerrichting
	extends AbstractHibernateObject
{

	private final static long serialVersionUID = 1L;

	@ManyToOne(fetch = FetchType.LAZY)
	@JsonIgnore
	private CervixCytologieVerslagContent verslagContent;

	@Temporal(TemporalType.DATE)
	@Column
	@VraagElement(displayName = "Einde verrichting", extraTekst = "", code = "2.16.840.1.113883.2.4.3.36.77.2.11.73", isVerplicht = true)
	private Date eindeVerrichting;

	public CervixCytologieVerslagContent getVerslagContent()
	{
		return verslagContent;
	}

	public void setVerslagContent(CervixCytologieVerslagContent verslagContent)
	{
		this.verslagContent = verslagContent;
	}

	public Date getEindeVerrichting()
	{
		return eindeVerrichting;
	}

	public void setEindeVerrichting(Date eindeVerrichting)
	{
		this.eindeVerrichting = eindeVerrichting;
	}

}
