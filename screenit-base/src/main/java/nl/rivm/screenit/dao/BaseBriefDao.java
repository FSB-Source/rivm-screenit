package nl.rivm.screenit.dao;

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

import nl.rivm.screenit.model.Brief;
import nl.rivm.screenit.model.BriefDefinitie;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientBrief;
import nl.rivm.screenit.model.cervix.CervixBrief;
import nl.rivm.screenit.model.cervix.CervixHuisarts;
import nl.rivm.screenit.model.cervix.CervixRegioBrief;
import nl.rivm.screenit.model.enums.BriefType;

import org.hibernate.ScrollableResults;

public interface BaseBriefDao
{
	BriefDefinitie getBriefDefinitie(BriefType briefType, Date geldigOp);

	BriefDefinitie getNieuwsteBriefDefinitie(BriefType briefType);

	ScrollableResults getBriefDefinities(BriefType briefType);

	<B extends ClientBrief<?, ?, ?>> List<B> getDubbeleAangemaakteBrieven(List<BriefType> vervangenTypes, Client client, Class<B> briefClass);

	List<CervixRegioBrief> getDubbeleAangemaakteBrieven(List<BriefType> vervangenTypes, CervixHuisarts arts);

	boolean clientHeeftOngegenereerdeBriefVanType(BriefType type, Client client, Class<? extends ClientBrief<?, ?, ?>> briefClass);

	List<ClientBrief<?, ?, ?>> getClientBrieven(Client client);
}
