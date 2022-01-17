package nl.rivm.screenit.model.logging;

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

import javax.persistence.Entity;
import javax.persistence.OneToOne;
import javax.persistence.Table;

import nl.rivm.screenit.model.verwerkingverslag.cervix.CervixHerinnerenRapportage;

import org.hibernate.annotations.Cascade;
import org.hibernate.annotations.CascadeType;

@Entity
@Table(schema = "gedeeld")
public class CervixHerinnerenBeeindigdLogEvent extends LogEvent
{
	@OneToOne
	@Cascade(CascadeType.DELETE)
	private CervixHerinnerenRapportage rapportage;

	public CervixHerinnerenRapportage getRapportage()
	{
		return rapportage;
	}

	public void setRapportage(CervixHerinnerenRapportage rapportage)
	{
		this.rapportage = rapportage;
	}
}
