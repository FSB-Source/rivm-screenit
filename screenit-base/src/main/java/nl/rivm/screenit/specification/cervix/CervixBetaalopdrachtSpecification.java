package nl.rivm.screenit.specification.cervix;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import nl.rivm.screenit.model.cervix.facturatie.CervixBetaalopdracht;
import nl.rivm.screenit.model.cervix.facturatie.CervixBetaalopdracht_;
import nl.rivm.screenit.model.cervix.facturatie.CervixBoekRegel;
import nl.rivm.screenit.model.enums.BestandStatus;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject_;

import org.springframework.data.jpa.domain.Specification;

import static nl.rivm.screenit.specification.SpecificationUtil.skipWhenEmpty;
import static nl.rivm.screenit.specification.cervix.CervixBoekRegelSpecification.betaalopdrachtJoin;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class CervixBetaalopdrachtSpecification
{
	public static Specification<CervixBoekRegel> heeftOpdrachtID(Long id)
	{
		return (r, q, cb) -> cb.equal(betaalopdrachtJoin(r).get(AbstractHibernateObject_.id), id);
	}

	public static Specification<CervixBoekRegel> filterOpBetalingskenmerkContaining(String betalingskenmerk)
	{
		return skipWhenEmpty(betalingskenmerk,
			(r, q, cb) -> cb.like(betaalopdrachtJoin(r).get(CervixBetaalopdracht_.betalingskenmerk), "%" + betalingskenmerk + "%"));
	}

	public static Specification<CervixBetaalopdracht> heeftNietStatus(BestandStatus bestandStatus)
	{
		return (r, q, cb) -> cb.notEqual(r.get(CervixBetaalopdracht_.status), bestandStatus);
	}

	public static Specification<CervixBetaalopdracht> heeftNietStatusIn(List<BestandStatus> bestandStatuses)
	{
		return (r, q, cb) -> r.get(CervixBetaalopdracht_.status).in(bestandStatuses);
	}

	public static Specification<CervixBetaalopdracht> heeftStatusDatumOpOfVoor(LocalDate peilMoment)
	{
		return (r, q, cb) -> cb.lessThanOrEqualTo(r.get(CervixBetaalopdracht_.statusDatum), DateUtil.toUtilDate(peilMoment));
	}

	public static Specification<CervixBetaalopdracht> heeftSepaSpecificatiePdf()
	{
		return (r, q, cb) -> cb.isNotNull(r.get(CervixBetaalopdracht_.sepaSpecificatiePdf));
	}

	public static Specification<CervixBetaalopdracht> heeftSepaDocument()
	{
		return (r, q, cb) -> cb.isNotNull(r.get(CervixBetaalopdracht_.sepaDocument));
	}
}
