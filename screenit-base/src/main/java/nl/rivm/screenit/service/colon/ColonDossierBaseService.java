package nl.rivm.screenit.service.colon;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.time.LocalDate;

import nl.rivm.screenit.model.colon.ColonDossier;
import nl.rivm.screenit.model.colon.ColonIntakeAfspraak;
import nl.rivm.screenit.model.colon.MdlVerslag;
import nl.rivm.screenit.model.colon.enums.ColonUitnodigingsintervalType;
import nl.rivm.screenit.model.colon.enums.MdlVervolgbeleid;

public interface ColonDossierBaseService
{
	LocalDate getDatumVolgendeUitnodiging(ColonDossier dossier);

	void setDatumVolgendeUitnodiging(ColonDossier dossier, ColonUitnodigingsintervalType interval);

	void updateIntervalReferentieDatums();

	void setVolgendeUitnodingVoorConclusie(ColonIntakeAfspraak afspraak);

	LocalDate getTheoretischeDatumVolgendeUitnodiging(ColonDossier dossier, ColonUitnodigingsintervalType interval);

	void setVolgendeUitnodigingVoorVerslag(MdlVerslag verslag);

	void maakDossierLeeg(ColonDossier dossier);

	MdlVervolgbeleid getVervolgbeleid(MdlVerslag mdlVerslag);
}
