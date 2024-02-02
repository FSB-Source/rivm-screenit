package nl.rivm.screenit.mamma.se.repository;

/*-
 * ========================LICENSE_START=================================
 * screenit-se-rest-bk
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

import nl.rivm.screenit.mamma.se.dto.DagStatistiekAfspraakStatussen;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.enums.MammaAfspraakStatus;
import nl.rivm.screenit.repository.BaseJpaRepository;

import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

public interface MammaAfspraakRepository extends BaseJpaRepository<MammaAfspraak>
{
	@Query("select "
		+ "coalesce(sum(case when (a.status = 'GEPLAND') then 1 else 0 end), 0) as aantalVerwacht, "
		+ "coalesce(sum(case when (a.status = 'BEEINDIGD' and o.status = 'AFGEROND') then 1 else 0 end), 0) as aantalAfgerond, "
		+ "coalesce(sum(case when (a.status = 'BEEINDIGD' and o.status = 'ONDERBROKEN') then 1 else 0 end), 0) as aantalOnderbroken, "
		+ "coalesce(sum(case when (a.status = 'BEEINDIGD' and o.status = 'ONVOLLEDIG') then 1 else 0 end), 0) as aantalOnvolledig "
		+ "from MammaAfspraak a "
		+ "left join a.onderzoek o "
		+ "join a.uitnodiging u "
		+ "join a.standplaatsPeriode sp "
		+ "join sp.screeningsEenheid se "
		+ "join u.screeningRonde sr "
		+ "where "
		+ "se.code = :seCode  "
		+ "and u.laatsteAfspraak = a "
		+ "and a.vanaf between :beginMoment and :eindMoment "
		+ "and a.status in :afspraakStatussen "
		+ "and sr.status = nl.rivm.screenit.model.ScreeningRondeStatus.LOPEND"
	)
	DagStatistiekAfspraakStatussen getAantalAfsprakenPerStatus(@Param("seCode") String seCode, @Param("beginMoment") Date beginMoment, @Param("eindMoment") Date eindMoment,
		@Param("afspraakStatussen") MammaAfspraakStatus[] afspraakStatussen);
}
