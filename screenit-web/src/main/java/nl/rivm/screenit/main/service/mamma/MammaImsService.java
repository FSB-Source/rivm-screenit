package nl.rivm.screenit.main.service.mamma;

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

import java.io.IOException;
import java.util.List;

import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;
import nl.rivm.screenit.model.mamma.enums.MammobridgeFocusMode;
import nl.rivm.screenit.model.mamma.enums.MammobridgeRole;
import nl.rivm.screenit.model.mamma.imsapi.FhirUserSession;
import nl.rivm.screenit.util.functionalinterfaces.StringResolver;

public interface MammaImsService
{
	String createAllImagesSeenMessage(Gebruiker gebruiker, MammobridgeRole mammobridgeRole, MammaOnderzoek onderzoek, MammobridgeFocusMode mammobridgeFocusMode);

	FhirUserSession parseFhirMessage(String json) throws IOException;

	String createLogonMessage(Gebruiker gebruiker, MammobridgeRole mammobridgeRole);

	String createLogoffMessage(Gebruiker gebruiker, MammobridgeRole mammobridgeRole);

	String createDesktopSyncMessage(Gebruiker gebruiker, MammobridgeRole mammobridgeRole, Long huidigeOnderzoekId, List<Long> komendeBeoordelingIds, MammobridgeFocusMode focusMode);

	String createEmptyDesktopSyncMessage(Gebruiker gebruiker, MammobridgeRole mammobridgeRole);

	String handleError(String error, InstellingGebruiker gebruiker, StringResolver stringResolver, Long onderzoekId);

}
