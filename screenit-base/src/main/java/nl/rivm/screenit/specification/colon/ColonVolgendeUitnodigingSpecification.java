package nl.rivm.screenit.specification.colon;

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
import java.time.temporal.ChronoUnit;

import javax.persistence.criteria.From;
import javax.persistence.criteria.Join;
import javax.persistence.criteria.JoinType;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;

import nl.rivm.screenit.model.colon.ColonUitnodigingsinterval;
import nl.rivm.screenit.model.colon.ColonUitnodigingsinterval_;
import nl.rivm.screenit.model.colon.ColonVolgendeUitnodiging;
import nl.rivm.screenit.model.colon.ColonVolgendeUitnodiging_;
import nl.rivm.screenit.specification.ExtendedSpecification;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject_;

import static nl.rivm.screenit.specification.DateSpecification.intervalInDagen;
import static nl.rivm.screenit.specification.SpecificationUtil.join;

@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class ColonVolgendeUitnodigingSpecification
{
	public static ExtendedSpecification<ColonVolgendeUitnodiging> heeftGeenDatumVolgendeRonde()
	{
		return (r, q, cb) -> cb.isNull(r.get(ColonVolgendeUitnodiging_.datumVolgendeRonde));
	}

	public static ExtendedSpecification<ColonVolgendeUitnodiging> heeftPeildatumVanafReferentieDatum(JoinType intervalJoinType)
	{
		return (r, q, cb) -> cb.and(
			cb.isNull(r.get(ColonVolgendeUitnodiging_.projectPeildatum)),
			cb.lessThanOrEqualTo(r.get(ColonVolgendeUitnodiging_.peildatum), intervalJoin(r, intervalJoinType).get(ColonUitnodigingsinterval_.berekendeReferentieDatum))
		);
	}

	public static ExtendedSpecification<ColonVolgendeUitnodiging> heeftPeildatumVanafIntervalReferentieDatum(long dagen, JoinType intervalJoinType)
	{
		return (r, q, cb) -> cb.and(
			cb.isNull(r.get(ColonVolgendeUitnodiging_.projectPeildatum)),
			cb.lessThanOrEqualTo(r.get(ColonVolgendeUitnodiging_.peildatum),
				intervalInDagen(cb, intervalJoin(r, intervalJoinType).get(ColonUitnodigingsinterval_.berekendeReferentieDatum), dagen))
		);
	}

	public static ExtendedSpecification<ColonVolgendeUitnodiging> heeftProjectPeildatumVanafReferentieDatum(JoinType intervalJoinType)
	{
		return (r, q, cb) -> cb.and(
			cb.isNotNull(r.get(ColonVolgendeUitnodiging_.projectPeildatum)),
			cb.lessThanOrEqualTo(r.get(ColonVolgendeUitnodiging_.projectPeildatum).as(LocalDate.class),
				intervalJoin(r, intervalJoinType).get(ColonUitnodigingsinterval_.berekendeReferentieDatum).as(LocalDate.class))
		);
	}

	public static ExtendedSpecification<ColonVolgendeUitnodiging> heeftProjectPeildatumVanafIntervalReferentieDatum(long dagen, JoinType intervalJoinType)
	{
		return (r, q, cb) -> cb.and(
			cb.isNotNull(r.get(ColonVolgendeUitnodiging_.projectPeildatum)),
			cb.lessThanOrEqualTo(r.get(ColonVolgendeUitnodiging_.projectPeildatum).as(LocalDate.class),
				intervalInDagen(cb, intervalJoin(r, intervalJoinType).get(ColonUitnodigingsinterval_.berekendeReferentieDatum), dagen).as(LocalDate.class))
		);
	}

	public static ExtendedSpecification<ColonVolgendeUitnodiging> heeftDatumVolgendeRondeVanaf(LocalDate datum)
	{
		return (r, q, cb) -> cb.and(
			cb.isNotNull(r.get(ColonVolgendeUitnodiging_.datumVolgendeRonde)),
			cb.lessThanOrEqualTo(r.get(ColonVolgendeUitnodiging_.datumVolgendeRonde), datum)
		);
	}

	public static ExtendedSpecification<ColonVolgendeUitnodiging> getReferentieSpecification(LocalDate peildatum, LocalDate vandaag, JoinType intervalJoinType)
	{
		var afwijking = ChronoUnit.DAYS.between(vandaag, peildatum);
		if (afwijking != 0)
		{
			return heeftDatumVolgendeRondeVanaf(vandaag.plusDays(afwijking))
				.or(heeftGeenDatumVolgendeRonde()
					.and(heeftPeildatumVanafIntervalReferentieDatum(afwijking, intervalJoinType)
						.or(heeftProjectPeildatumVanafIntervalReferentieDatum(afwijking, intervalJoinType))));
		}

		return heeftDatumVolgendeRondeVanaf(vandaag)
			.or(heeftGeenDatumVolgendeRonde()
				.and(heeftPeildatumVanafReferentieDatum(intervalJoinType)
					.or(heeftProjectPeildatumVanafReferentieDatum(intervalJoinType))));
	}

	private static Join<? extends ColonVolgendeUitnodiging, ColonUitnodigingsinterval> intervalJoin(From<?, ? extends ColonVolgendeUitnodiging> r, JoinType joinType)
	{
		return join(r, ColonVolgendeUitnodiging_.interval, joinType);
	}

	public static ExtendedSpecification<ColonVolgendeUitnodiging> heeftProjectPeildatum()
	{
		return (r, q, cb) -> cb.isNotNull(r.get(ColonVolgendeUitnodiging_.projectPeildatum));
	}

	public static ExtendedSpecification<ColonVolgendeUitnodiging> heeftGeenId()
	{
		return (r, q, cb) -> cb.isNull(r.get(AbstractHibernateObject_.id));
	}
}
