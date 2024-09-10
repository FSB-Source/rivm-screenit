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

import java.util.Date;
import java.util.List;

import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaStandplaatsRonde;
import nl.rivm.screenit.repository.BaseJpaRepository;

import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

public interface MammaBaseAfspraakRepository extends BaseJpaRepository<MammaAfspraak>
{
	@Query(nativeQuery = true, value = "select a.id from mamma.afspraak a" +
		" join mamma.uitnodiging u on a.id = u.laatste_afspraak" +
		" join mamma.screening_ronde s on u.id = s.laatste_uitnodiging" +
		" join mamma.dossier d on s.id = d.laatste_screening_ronde " +
		" where a.status = 'GEPLAND' and a.sms_status = 'TE_VERSTUREN' AND s.status = 'LOPEND' AND d.status = 'ACTIEF' and a.vanaf between :vanafMoment and :totMoment and u.laatste_afspraak = a.id"
		+ " order by a.id desc "
		+ "limit 250")
	List<Long> findTop250AfsprakenOmSmsTeVersturen(@Param("vanafMoment") Date vanafMoment, @Param("totMoment") Date totMoment);

	boolean existsByStandplaatsPeriode_StandplaatsRonde(MammaStandplaatsRonde standplaatsRonde);

}
