
package nl.rivm.screenit.model.formulieren;

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
import javax.persistence.ManyToOne;

import nl.rivm.screenit.model.Gebruiker;
import nl.topicuszorg.formulieren2.persistence.resultaat.AbstractEnkelvoudigAntwoord;

@Entity
public class GebruikerAntwoord extends AbstractEnkelvoudigAntwoord<Gebruiker>
{

	private static final long serialVersionUID = 1L;

	@ManyToOne
	private Gebruiker medewerker;

	@Override
	public String getStringValue()
	{
		return medewerker.getNaamVolledig();
	}

	@Override
	public Gebruiker getValue()
	{
		return medewerker;
	}

	@Override
	public void setValue(Gebruiker value)
	{
		medewerker = value;
	}

}
