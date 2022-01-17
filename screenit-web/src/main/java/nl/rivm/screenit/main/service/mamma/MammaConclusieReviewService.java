package nl.rivm.screenit.main.service.mamma;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.util.List;

import nl.rivm.screenit.main.model.mamma.beoordeling.MammaConclusieReviewZoekObject;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.enums.MammaConclusieReviewFilterOptie;
import nl.rivm.screenit.model.mamma.MammaConclusieReview;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.enums.MammaFollowUpConclusieStatus;
import nl.rivm.screenit.model.mamma.enums.MammobridgeRole;

public interface MammaConclusieReviewService
{
	long countConclusieReviewsVanRadioloog(MammaConclusieReviewZoekObject zoekObject);

	List<MammaConclusieReview> zoekConclusieReviewsVanRadioloog(MammaConclusieReviewZoekObject zoekObject, int first, int count, String sortProperty, boolean asc);

	List<Long> zoekBeoordelingIdsMetConclusie(MammaConclusieReviewZoekObject zoekObject, String sortProperty, boolean asc);

	MammaConclusieReview getConclusieReview(MammaScreeningRonde screeningRonde, InstellingGebruiker radioloog);

	void maakConclusieReviewVoorBetrokkenRadiologen(MammaScreeningRonde screeningRonde);

	void conclusieReviewAfronden(MammaConclusieReview conclusieReview);

	MammobridgeRole getMammobridgeRoleBijConclusieReviewFilter(MammaConclusieReviewFilterOptie filterOptie);
}
