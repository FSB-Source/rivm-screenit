
package nl.rivm.screenit.model.gba;

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

import java.util.Date;

import javax.persistence.Entity;
import javax.persistence.Index;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.apache.commons.lang.StringUtils;
import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;

@Entity
@Table(schema = "gedeeld", indexes = { @Index(name = "IDX_gba_mutaties_aanvullende_informatie", columnList = "aanvullendeInformatie") })
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
public class GbaMutatie extends AbstractHibernateObject
{

	private static final long serialVersionUID = 1L;

	@Temporal(TemporalType.TIMESTAMP)
	private Date mutatieDatum;

	private String typeBericht;

	private String berichtEref;

	private String aanvullendeInformatie = StringUtils.EMPTY;

	public Date getMutatieDatum()
	{
		return mutatieDatum;
	}

	public void setMutatieDatum(Date mutatieDatum)
	{
		this.mutatieDatum = mutatieDatum;
	}

	public String getTypeBericht()
	{
		return typeBericht;
	}

	public void setTypeBericht(String typeBericht)
	{
		this.typeBericht = typeBericht;
	}

	public String getBerichtEref()
	{
		return berichtEref;
	}

	public void setBerichtEref(String berichtEref)
	{
		this.berichtEref = berichtEref;
	}

	public String getAanvullendeInformatie()
	{
		return aanvullendeInformatie;
	}

	public void setAanvullendeInformatie(String aanvullendeInformatie)
	{
		this.aanvullendeInformatie = aanvullendeInformatie;
	}

}
