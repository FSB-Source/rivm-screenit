package nl.rivm.screenit.main.service.cervix;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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
import java.util.List;
import java.util.function.UnaryOperator;

import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde;
import nl.rivm.screenit.model.cervix.CervixUitnodiging;

public interface CervixUitnodigingService
{

	String zoekMonsters(Instelling ingelogdNamensOrganisatie, String monsterId, String bsn, Date geboortedatum, List<CervixUitnodiging> uitnodigingen,
		UnaryOperator<String> getString);

	char getVerwachteEersteZASMonsterIdLetter(Instelling ingelogdNamensOrganisatie);

	boolean magMonsterVerwerktWordenDoorLab(Instelling ingelogdNamensOrganisatie, CervixUitnodiging uitnodiging);

	boolean uitnodigingHeeftZasMetNieuweBarcode(CervixUitnodiging uitnodiging);

	boolean magRedenUitnodigingKiezen(CervixScreeningRonde laatsteScreeningRonde);
}
