
package nl.rivm.screenit.model.colon;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.model.colon.enums.ColonUitnodigingCategorie;

public class ClientCategorieEntry implements Serializable
{

	private static final long serialVersionUID = 1L;

	private final Long clientId;

	private final ColonUitnodigingCategorie categorie;

	private final Long screeningOrganisatieId;

	private final Long projectGroepId;

	private final Boolean gepusht;

	public ClientCategorieEntry()
	{
		this(null, null, null);
	}

	public ClientCategorieEntry(Long clientId, ColonUitnodigingCategorie categorie, Long screeningOrganisatieId)
	{
		this(clientId, categorie, screeningOrganisatieId, null, Boolean.FALSE);

	}

	public ClientCategorieEntry(Long clientId, ColonUitnodigingCategorie categorie, Long screeningOrganisatieId, Boolean gepusht)
	{
		this(clientId, categorie, screeningOrganisatieId, null, gepusht);

	}

	public ClientCategorieEntry(Long clientId, ColonUitnodigingCategorie categorie, Long screeningOrganisatieId, Long projectGroepId)
	{
		this(clientId, categorie, screeningOrganisatieId, projectGroepId, Boolean.FALSE);
	}

	public ClientCategorieEntry(Long clientId, ColonUitnodigingCategorie categorie, Long screeningOrganisatieId, Long projectGroepId, Boolean gepusht)
	{
		this.clientId = clientId;
		this.categorie = categorie;
		this.screeningOrganisatieId = screeningOrganisatieId;
		this.projectGroepId = projectGroepId;
		this.gepusht = gepusht;
	}

	public Long getClientId()
	{
		return clientId;
	}

	public Long getScreeningOrganisatieId()
	{
		return screeningOrganisatieId;
	}

	public ColonUitnodigingCategorie getCategorie()
	{
		return categorie;
	}

	public Long getProjectGroepId()
	{
		return projectGroepId;
	}

	public Boolean getGepusht()
	{
		return gepusht;
	}
}
