package nl.rivm.screenit.mamma.planning.repository;

/*-
 * ========================LICENSE_START=================================
 * screenit-planning-bk
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
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import nl.rivm.screenit.mamma.planning.dao.dto.PlanningCapaciteitBlokProjectie;
import nl.rivm.screenit.mamma.planning.model.PlanningBlok;
import nl.rivm.screenit.mamma.planning.model.PlanningScreeningsEenheid;
import nl.rivm.screenit.model.mamma.MammaCapaciteitBlok;
import nl.rivm.screenit.model.mamma.MammaCapaciteitBlok_;
import nl.rivm.screenit.repository.BaseJpaRepository;
import nl.rivm.screenit.util.RangeUtil;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject_;

import org.springframework.data.jpa.repository.Query;

import com.google.common.collect.BoundType;

import static nl.rivm.screenit.specification.DateSpecification.bevatLocalDateToDate;
import static nl.rivm.screenit.specification.mamma.MammaCapaciteitBlokSpecification.heeftScreeningsEenheidId;
import static nl.rivm.screenit.util.DateUtil.toLocalDateTime;
import static nl.rivm.screenit.util.DateUtil.toLocalTime;

public interface PlanningCapaciteitBlokRepository extends BaseJpaRepository<MammaCapaciteitBlok>
{
	@Query("SELECT CAST( MAX(vanaf) as LocalDate) FROM MammaCapaciteitBlok")
	Optional<LocalDate> findMaxDatumVanAlleCapaciteitsBlokken();

	default Set<PlanningBlok> leesCapaciteitBlokken(PlanningScreeningsEenheid screeningsEenheid, LocalDate vanaf, LocalDate totEnMet)
	{
		var nullSafeRange = RangeUtil.range(vanaf, BoundType.CLOSED, totEnMet, BoundType.CLOSED);

		var capaciteitblokken = findWith(
			heeftScreeningsEenheidId(screeningsEenheid.getId())
				.and(bevatLocalDateToDate(nullSafeRange, r -> r.get(MammaCapaciteitBlok_.vanaf))),
			PlanningCapaciteitBlokProjectie.class,
			q -> q.projections((cb, r) -> List.of(
					r.get(AbstractHibernateObject_.id).alias("id"),
					r.get(MammaCapaciteitBlok_.vanaf).alias("vanaf"),
					r.get(MammaCapaciteitBlok_.tot).alias("tot"),
					r.get(MammaCapaciteitBlok_.aantalOnderzoeken).alias("aantalOnderzoeken"),
					r.get(MammaCapaciteitBlok_.blokType).alias("blokType"),
					r.get(MammaCapaciteitBlok_.opmerkingen).alias("opmerkingen"),
					r.get(MammaCapaciteitBlok_.minderValideAfspraakMogelijk).alias("minderValideAfspraakMogelijk")))
				.all());

		return capaciteitblokken.stream()
			.map(blokProjectie -> mapNaarPlanningBlok(screeningsEenheid, blokProjectie))
			.collect(Collectors.toSet());
	}

	private PlanningBlok mapNaarPlanningBlok(PlanningScreeningsEenheid screeningsEenheid, PlanningCapaciteitBlokProjectie blokProjectie)
	{
		var vanaf = toLocalDateTime(blokProjectie.getVanaf());

		var blok = new PlanningBlok(blokProjectie.getId(), vanaf.toLocalTime(), toLocalTime(blokProjectie.getTot()),
			blokProjectie.getAantalOnderzoeken(), blokProjectie.getBlokType(), blokProjectie.getOpmerkingen(), blokProjectie.isMinderValideAfspraakMogelijk());

		var dag = screeningsEenheid.getDagNavigableMap().get(vanaf.toLocalDate());
		blok.setDag(dag);
		blok.setScreeningsEenheid(screeningsEenheid);
		return blok;
	}
}
