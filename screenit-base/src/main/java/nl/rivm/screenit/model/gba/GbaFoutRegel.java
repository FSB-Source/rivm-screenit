
package nl.rivm.screenit.model.gba;

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
import javax.persistence.Enumerated;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import nl.rivm.screenit.model.helper.HibernateMagicNumber;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;

import com.fasterxml.jackson.annotation.JsonIgnore;

@Entity
@Table(schema = "algemeen")
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
public class GbaFoutRegel extends AbstractHibernateObject
{
	@Column(length = HibernateMagicNumber.L1024)
	private String fout;

	@Enumerated
	private GbaFoutCategorie foutCategorie;

	private Long client;

	@ManyToOne
	@JsonIgnore
	private GbaVerwerkingsLog verwerkingsLog;

	public String getFout()
	{
		return fout;
	}

	public void setFout(String fout)
	{
		this.fout = fout;
	}

	public GbaFoutCategorie getFoutCategorie()
	{
		return foutCategorie;
	}

	public void setFoutCategorie(GbaFoutCategorie foutCategorie)
	{
		this.foutCategorie = foutCategorie;
	}

	public Long getClient()
	{
		return client;
	}

	public void setClient(Long client)
	{
		this.client = client;
	}

	public GbaVerwerkingsLog getVerwerkingsLog()
	{
		return verwerkingsLog;
	}

	public void setVerwerkingsLog(GbaVerwerkingsLog verwerkingsLog)
	{
		this.verwerkingsLog = verwerkingsLog;
	}

	@Override
	public String toString()
	{
		return "GbaFoutRegel [fout=" + fout + ", foutCategorie=" + foutCategorie + "]";
	}

}
