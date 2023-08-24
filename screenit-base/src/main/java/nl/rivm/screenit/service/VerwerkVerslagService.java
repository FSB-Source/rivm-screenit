
package nl.rivm.screenit.service;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.util.Set;

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.ScreeningRonde;
import nl.rivm.screenit.model.berichten.Verslag;
import nl.rivm.screenit.model.berichten.enums.VerslagType;
import nl.rivm.screenit.model.colon.MdlVerslag;
import nl.topicuszorg.formulieren2.api.resultaat.Antwoord;

public interface VerwerkVerslagService
{

	void verwerkInDossier(Verslag verslag);

	void onAfterVerwerkVerslagContent(Verslag verslag);

	ScreeningRonde getValideScreeningsRonde(VerslagType type, Client client, Verslag olderVerslag, Date onderzoeksdatum);

	void valideerVerslagVoorAfronden(Verslag verslag, Set<Antwoord<?>> antwoorden, InstellingGebruiker instellingGebruiker);

	void ontkoppelOfVerwijderComplicaties(MdlVerslag mdlVerslag);

}
