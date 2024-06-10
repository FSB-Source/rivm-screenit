package nl.rivm.screenit.repository.mamma;

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

import java.util.List;
import java.util.Optional;

import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.mamma.MammaConclusieReview;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.repository.BaseJpaRepository;

import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

public interface MammaConclusieReviewRepository extends BaseJpaRepository<MammaConclusieReview>
{
	Optional<MammaConclusieReview> findByRadioloogAndScreeningRondeAndReviewAlsCoordinerendRadioloog(InstellingGebruiker radioloog,
		MammaScreeningRonde screeningRonde, boolean reviewdAlsCoordinerendRadioloog);

	@Query("select distinct ig"
		+ " from InstellingGebruiker ig"
		+ " join MammaLezing l on ig = l.beoordelaar"
		+ " join MammaBeoordeling b on l = b.eersteLezing or l = b.tweedeLezing or l = b.arbitrageLezing or l = b.discrepantieLezing"
		+ " join MammaOnderzoek o on b.onderzoek = o"
		+ " join MammaAfspraak a on o = a.onderzoek"
		+ " join MammaUitnodiging u on a.uitnodiging = u"
		+ " join MammaScreeningRonde sr on u.screeningRonde = sr"
		+ " left join MammaConclusieReview cr on sr = cr.screeningRonde and cr.radioloog = ig"
		+ " where sr = :screeningRonde"
		+ " and cr.id is null")
	List<InstellingGebruiker> getRadiologenMetLezingVanRondeEnZonderReview(@Param("screeningRonde") MammaScreeningRonde screeningRonde);
}
