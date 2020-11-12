package nl.rivm.screenit.service.colon;

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

import java.util.Date;
import java.util.Set;

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.berichten.Verslag;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.colon.MdlVerslag;
import nl.rivm.screenit.model.colon.PaVerslag;
import nl.topicuszorg.formulieren2.api.resultaat.Antwoord;

public interface ColonVerwerkVerslagService
{
	void verwerkInDossier(MdlVerslag verslag);

	void onAfterVerwerkVerslagContent(MdlVerslag verslag);

	void onAfterVerwerkVerslagContent(PaVerslag verslag);

	void ontkoppelOfVerwijderComplicaties(MdlVerslag mdlVerslag);

	ColonScreeningRonde getValideScreeningsRonde(Client client, Verslag olderVerslag, Date onderzoeksdatum);

	void valideerVerslagVoorAfronden(PaVerslag verslag, InstellingGebruiker instellingGebruiker);

	void valideerVerslagVoorAfronden(MdlVerslag verslag, Set<Antwoord<?>> antwoorden, InstellingGebruiker instellingGebruiker);

}
