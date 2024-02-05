
package nl.rivm.screenit.main.service;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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
import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.overeenkomsten.AbstractAfgeslotenOvereenkomst;
import nl.rivm.screenit.model.overeenkomsten.AfgeslotenInstellingOvereenkomst;
import nl.rivm.screenit.model.overeenkomsten.Overeenkomst;
import nl.rivm.screenit.model.overeenkomsten.OvereenkomstType;

public interface OvereenkomstService
{

	void saveOrUpdateOvereenkomst(Overeenkomst overeenkomst, UploadDocument uploadDocument, Account account);

	void saveOrUpdateOvereenkomst(AbstractAfgeslotenOvereenkomst overeenkomst, UploadDocument uploadDocument, Account account);

	List<Overeenkomst> getOvereenkomsten(Boolean actief, long first, long size, String sortProperty, boolean asc);

	long countOvereenkomsten(Boolean actief);

	void updateOvereenkomst(Overeenkomst overeenkomst, Account account);

	<T> List<AbstractAfgeslotenOvereenkomst> getAfgeslotenOvereenkomsten(Class<? extends AbstractAfgeslotenOvereenkomst> returnType, T filter, Boolean actief, Long first,
		Long size, String sortProperty, boolean asc);

	<T> long countAfgeslotenOvereenkomsten(Class<? extends AbstractAfgeslotenOvereenkomst> returnType, T filter, Boolean actief);

	List<Overeenkomst> getOvereenkomsten(OrganisatieType organisatieType, OvereenkomstType... overeenkomstTypes);

	List<AbstractAfgeslotenOvereenkomst> getTeAccoderenOvereenkomsten(InstellingGebruiker inTeLoggenInstellingGebruiker);

	long countTeAccoderenOvereenkomsten(InstellingGebruiker inTeLoggenInstellingGebruiker);

	void accodeerOvereenkomsten(InstellingGebruiker instellingGebruiker, Account account);

	List<Instelling> getAfgeslotenOvereenkomsten(OvereenkomstZoekFilter filter, String sortProperty, boolean ascending, int first, int count);

	long countAfgeslotenOvereenkomsten(OvereenkomstZoekFilter filter);

	List<AfgeslotenInstellingOvereenkomst> getAfgeslotenOvereenkomstenBijInstelling(OvereenkomstZoekFilter filter, Instelling instelling);

	List<Overeenkomst> getAlleOvereenkomstenVanTypeOvereenkomst();

}
