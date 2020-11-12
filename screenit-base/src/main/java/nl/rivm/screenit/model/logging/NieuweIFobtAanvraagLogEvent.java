
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
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.enums.NieuweIfobtResultaat;

@Entity
@Table(schema = "gedeeld")
public class NieuweIFobtAanvraagLogEvent extends LogEvent
{
	@ManyToOne
	private Client client;

	@Enumerated(EnumType.STRING)
	private NieuweIfobtResultaat resultaat;

	public Client getClient()
	{
		return client;
	}

	public void setClient(Client client)
	{
		this.client = client;
	}

	public NieuweIfobtResultaat getResultaat()
	{
		return resultaat;
	}

	public void setResultaat(NieuweIfobtResultaat resultaat)
	{
		this.resultaat = resultaat;
	}
}
