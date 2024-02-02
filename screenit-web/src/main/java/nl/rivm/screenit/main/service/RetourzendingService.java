
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

import java.io.File;
import java.io.IOException;

import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.InpakbareUitnodiging;
import nl.rivm.screenit.model.ScreeningRonde;
import nl.rivm.screenit.model.Uitnodiging;
import nl.rivm.screenit.model.cervix.CervixUitnodiging;
import nl.rivm.screenit.model.colon.ColonUitnodiging;
import nl.rivm.screenit.model.logging.RetourzendingLogEvent;

public interface RetourzendingService
{

	RetourzendingLogEvent verwerkBestandMetRetourzendingen(InstellingGebruiker ingelogdeGebruiker, String contentType, File file, String fileName) throws IOException;

	<U extends InpakbareUitnodiging<S>, S extends ScreeningRonde<?, ?, ?, ?>> void verwerkRetourzendingHandmatig(InstellingGebruiker ingelogdeGebruiker, U uitnodiging,
                                                                                                                 String retourzendingReden);

	String isValideCervixUitnodiging(CervixUitnodiging uitnodiging);

	String isValideColonUitnodiging(ColonUitnodiging uitnodiging);

	<U extends Uitnodiging> boolean isDossierInactiefOfRondeAfgerond(U uitnodiging);

}
