package nl.rivm.screenit.main.dao;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.util.List;

import nl.rivm.screenit.main.web.gebruiker.screening.colon.overeenkomstenzoeken.OvereenkomstZoekFilter;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.overeenkomsten.AbstractAfgeslotenOvereenkomst;
import nl.rivm.screenit.model.overeenkomsten.AfgeslotenInstellingOvereenkomst;
import nl.rivm.screenit.model.overeenkomsten.Overeenkomst;
import nl.rivm.screenit.model.overeenkomsten.OvereenkomstType;

public interface OvereenkomstDao
{

	List<Overeenkomst> getOvereenkomsten(Boolean actief, Long first, Long size, String sortProperty, boolean asc);

	long countOvereenkomsten(Boolean actief);

	<T> List<AbstractAfgeslotenOvereenkomst> getAfgeslotenOvereenkomsten(Class<? extends AbstractAfgeslotenOvereenkomst> returnType, T filter, Boolean actief, Long first,
		Long size, String sortProperty, boolean asc);

	<T> long countAfgeslotenOvereenkomsten(Class<? extends AbstractAfgeslotenOvereenkomst> returnType, T filter, Boolean actief);

	List<Overeenkomst> getOvereenkomsten(OrganisatieType organisatieType, OvereenkomstType... overeenkomstTypes);

	int getVolgnummerOvereenkomst();

	List<AbstractAfgeslotenOvereenkomst> getTeAccoderenOvereenkomsten(InstellingGebruiker inTeLoggenInstellingGebruiker);

	long countTeAccoderenOvereenkomsten(InstellingGebruiker inTeLoggenInstellingGebruiker);

	List<Instelling> getAfgeslotenOvereenkomsten(OvereenkomstZoekFilter filter, String sortProperty, boolean ascending, int first, int count);

	long countAfgeslotenOvereenkomsten(OvereenkomstZoekFilter filter);

	List<AfgeslotenInstellingOvereenkomst> getAfgeslotenOvereenkomstenBijInstelling(OvereenkomstZoekFilter filter, Instelling instelling);

	List<Overeenkomst> getAlleOvereenkomstenVanTypeOvereenkomst();

	boolean isErAlEenZakelijkOvereenkomst(Overeenkomst overeenkomst);
}
