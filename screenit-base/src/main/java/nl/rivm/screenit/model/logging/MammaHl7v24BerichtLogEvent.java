package nl.rivm.screenit.model.logging;

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

import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.Lob;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import nl.rivm.screenit.model.Client;

import org.hibernate.annotations.Type;

@Entity
@Table(schema = "gedeeld")
public class MammaHl7v24BerichtLogEvent extends LogEvent
{
	@ManyToOne(fetch = FetchType.LAZY)
	private Client client;

	@Lob
	@Type(type = "org.hibernate.type.TextType")
	private String hl7MessageStructure;

	public Client getClient()
	{
		return client;
	}

	public void setClient(Client client)
	{
		this.client = client;
	}

	public String getHl7MessageStructure()
	{
		return hl7MessageStructure;
	}

	public void setHl7MessageStructure(String hl7v24MessageStructure)
	{
		this.hl7MessageStructure = hl7v24MessageStructure;
	}
}
