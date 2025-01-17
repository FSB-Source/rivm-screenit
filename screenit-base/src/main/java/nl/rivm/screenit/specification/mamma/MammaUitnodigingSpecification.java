package nl.rivm.screenit.specification.mamma;

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

import java.time.LocalDateTime;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import nl.rivm.screenit.model.Uitnodiging_;
import nl.rivm.screenit.model.mamma.MammaAfspraak_;
import nl.rivm.screenit.model.mamma.MammaUitnodiging;
import nl.rivm.screenit.model.mamma.MammaUitnodiging_;
import nl.rivm.screenit.model.mamma.enums.MammaAfspraakStatus;
import nl.rivm.screenit.specification.ExtendedSpecification;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject_;

import org.springframework.data.jpa.domain.Specification;

import static nl.rivm.screenit.specification.SpecificationUtil.join;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class MammaUitnodigingSpecification
{
	public static Specification<MammaUitnodiging> heeftLaatsteAfspraakMetStatus(MammaAfspraakStatus status)
	{
		return (r, q, cb) ->
		{
			var laatsteAfspraakJoin = join(r, MammaUitnodiging_.laatsteAfspraak);
			return cb.equal(laatsteAfspraakJoin.get(MammaAfspraak_.status), status);
		};
	}

	public static ExtendedSpecification<MammaUitnodiging> heeftLaatsteAfspraak()
	{
		return (r, q, cb) -> cb.isNotNull(r.get(MammaUitnodiging_.laatsteAfspraak));
	}

	public static Specification<MammaUitnodiging> heeftLaatsteAfspraakMetStandplaatsPeriode(Long standplaatsPeriodeId)
	{
		return (r, q, cb) ->
		{
			var laatsteAfspraakJoin = join(r, MammaUitnodiging_.laatsteAfspraak);
			var periodeJoin = join(laatsteAfspraakJoin, MammaAfspraak_.standplaatsPeriode);
			return cb.equal(periodeJoin.get(AbstractHibernateObject_.id), standplaatsPeriodeId);
		};
	}

	public static ExtendedSpecification<MammaUitnodiging> isHerinnerd(boolean isHerinnerd)
	{
		return (r, q, cb) -> cb.equal(r.get(MammaUitnodiging_.herinnered), isHerinnerd);
	}

	public static ExtendedSpecification<MammaUitnodiging> isGemaaktOpOfVoor(LocalDateTime peilMoment)
	{
		return (r, q, cb) -> cb.lessThanOrEqualTo(r.get(Uitnodiging_.creatieDatum), DateUtil.toUtilDate(peilMoment));
	}
}
