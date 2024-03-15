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

import java.util.Date;
import java.util.Optional;

import nl.rivm.screenit.main.model.mamma.beoordeling.MammaConclusieReviewZoekObject;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.enums.MammaConclusieReviewFilterOptie;
import nl.rivm.screenit.model.mamma.MammaConclusieReview;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.enums.MammobridgeRole;

public interface MammaConclusieReviewService
{
	MammaConclusieReview getConclusieReview(MammaScreeningRonde screeningRonde, InstellingGebruiker radioloog);

	MammaConclusieReview getConclusieReviewCoordinerendRadioloog(MammaScreeningRonde screeningRonde, InstellingGebruiker radioloog);

	void saveConclusieReviewCoordinerendRadioloog(MammaConclusieReview conclusieReview, MammaScreeningRonde screeningRonde,
		InstellingGebruiker radioloog);

	void maakConclusieReviewVoorBetrokkenRadiologen(MammaScreeningRonde screeningRonde);

	void conclusieReviewAfronden(MammaConclusieReview conclusieReview);

	MammobridgeRole getMammobridgeRoleBijConclusieReviewFilter(MammaConclusieReviewFilterOptie filterOptie);

	Date bepaalInitieleConclusieReviewSorteerDatumCoordinerendRadioloog(MammaConclusieReviewZoekObject zoekObject);

	void logConclusieReviewAfgerond(InstellingGebruiker gebruiker, Client client, MammaConclusieReview conclusieReview, boolean doorCoordinerendRadioloog);

	Optional<MammaConclusieReview> getReviewAfgerondDoorCoordinerendRadioloog(InstellingGebruiker radioloog, MammaScreeningRonde screeningRonde);
}
