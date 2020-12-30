package nl.rivm.screenit.main.web.gebruiker.screening.colon.overeenkomstenzoeken;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.io.Serializable;
import java.util.List;

import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.overeenkomsten.Overeenkomst;

public class OvereenkomstZoekWrapper implements Serializable
{

	private static final long serialVersionUID = 1L;

	private Instelling instelling;

	private List<Overeenkomst> overeenkomsten;

	public Instelling getInstelling()
	{
		return instelling;
	}

	public void setInstelling(Instelling instelling)
	{
		this.instelling = instelling;
	}

	public List<Overeenkomst> getOvereenkomsten()
	{
		return overeenkomsten;
	}

	public void setOvereenkomsten(List<Overeenkomst> overeenkomsten)
	{
		this.overeenkomsten = overeenkomsten;
	}

}
