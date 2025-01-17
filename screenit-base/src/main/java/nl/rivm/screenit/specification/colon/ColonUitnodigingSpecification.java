package nl.rivm.screenit.specification.colon;

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
import java.util.Date;
import java.util.List;

import javax.persistence.criteria.JoinType;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import nl.rivm.screenit.model.Brief_;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.InpakbareUitnodiging_;
import nl.rivm.screenit.model.SingleTableHibernateObject_;
import nl.rivm.screenit.model.TablePerClassHibernateObject_;
import nl.rivm.screenit.model.Uitnodiging_;
import nl.rivm.screenit.model.colon.ColonBrief;
import nl.rivm.screenit.model.colon.ColonBrief_;
import nl.rivm.screenit.model.colon.ColonDossier_;
import nl.rivm.screenit.model.colon.ColonScreeningRonde_;
import nl.rivm.screenit.model.colon.ColonUitnodiging;
import nl.rivm.screenit.model.colon.ColonUitnodiging_;
import nl.rivm.screenit.model.colon.IFOBTTest_;
import nl.rivm.screenit.model.colon.enums.IFOBTTestStatus;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.specification.ExtendedSpecification;
import nl.rivm.screenit.specification.algemeen.ClientSpecification;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject_;

import org.jetbrains.annotations.NotNull;
import org.springframework.data.jpa.domain.Specification;

import com.google.common.collect.Range;

import static nl.rivm.screenit.specification.HibernateObjectSpecification.heeftGeenId;
import static nl.rivm.screenit.specification.RangeSpecification.bevat;
import static nl.rivm.screenit.specification.SpecificationUtil.join;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class ColonUitnodigingSpecification
{
	public static Specification<ColonUitnodiging> heeftActieveClient()
	{
		return ClientSpecification.heeftActieveClient().with(r ->
		{
			var ronde = join(r, ColonUitnodiging_.screeningRonde);
			var dossier = join(ronde, ColonScreeningRonde_.dossier);
			return join(dossier, ColonDossier_.client);
		});
	}

	public static ExtendedSpecification<ColonUitnodiging> isVerstuurd()
	{
		return (r, q, cb) -> cb.isTrue(r.get(InpakbareUitnodiging_.verstuurd));
	}

	public static ExtendedSpecification<ColonUitnodiging> heeftGeenUitnodiging()
	{
		return heeftGeenId();
	}

	public static Specification<ColonUitnodiging> heeftGeenVerstuurdDatum()
	{
		return (r, q, cb) -> cb.isNull(r.get(InpakbareUitnodiging_.verstuurdDatum));
	}

	public static Specification<ColonUitnodiging> heeftUitnodigingsDatumVoorDatum(Date datum)
	{
		return (r, q, cb) -> cb.lessThanOrEqualTo(r.get(Uitnodiging_.uitnodigingsDatum), datum);
	}

	public static Specification<ColonUitnodiging> heeftGeenBriefTypes(List<BriefType> briefTypes)
	{
		return (r, q, cb) ->
		{
			var subquery = q.subquery(Long.class);
			var subqueryRoot = subquery.from(ColonBrief.class);
			var briefScreeningRonde = subqueryRoot.get(ColonBrief_.screeningRonde);
			var uitnodigingScreeningRonde = r.get(ColonUitnodiging_.screeningRonde);

			subquery.select(subqueryRoot.get(TablePerClassHibernateObject_.id))
				.where(cb.and(cb.equal(briefScreeningRonde, uitnodigingScreeningRonde), subqueryRoot.get(Brief_.briefType).in(briefTypes)));

			return cb.not(cb.exists(subquery));
		};
	}

	public static Specification<ColonUitnodiging> heeftUitnodigingInRange(@NotNull Date begin, @NotNull Date eind)
	{
		var range = Range.open(begin, eind);
		return bevat(range, r -> r.get(Uitnodiging_.creatieDatum));
	}

	public static Specification<ColonUitnodiging> heeftClient(Client client)
	{
		return (r, q, cb) ->
		{
			var ronde = join(r, ColonUitnodiging_.screeningRonde);
			var dossier = join(ronde, ColonScreeningRonde_.dossier);
			var clientJoin = join(dossier, ColonDossier_.client);
			return cb.equal(clientJoin.get(SingleTableHibernateObject_.id), client.getId());
		};
	}

	public static ExtendedSpecification<ColonUitnodiging> heeftUitgesteldeUitslagDatumVoorOfOp(Date peildatum)
	{
		return (r, q, cb) ->
			cb.or(cb.isNull(r.get(ColonUitnodiging_.uitgesteldeUitslagDatum)), cb.lessThanOrEqualTo(r.get(ColonUitnodiging_.uitgesteldeUitslagDatum), peildatum));
	}

	public static ExtendedSpecification<ColonUitnodiging> heeftVerlopenFit(LocalDate peilDatum)
	{
		return (r, q, cb) ->
		{
			var fitJoin = join(r, ColonUitnodiging_.gekoppeldeTest, JoinType.LEFT);
			return cb.and(
				cb.equal(fitJoin.get(IFOBTTest_.status), IFOBTTestStatus.ACTIEF),
				cb.lessThan(r.get(Uitnodiging_.creatieDatum), DateUtil.toUtilDate(peilDatum))
			);
		};
	}

	public static ExtendedSpecification<ColonUitnodiging> heeftGeenFit()
	{
		return (r, q, cb) ->
		{
			var fitJoin = join(r, ColonUitnodiging_.gekoppeldeTest, JoinType.LEFT);
			return cb.isNull(fitJoin.get(AbstractHibernateObject_.id));
		};
	}

	public static ExtendedSpecification<ColonUitnodiging> heeftGeenRetourzendingReden()
	{
		return (r, q, cb) -> cb.isNull(r.get(InpakbareUitnodiging_.retourzendingReden));
	}

	public static ExtendedSpecification<ColonUitnodiging> heeftTrackTraceId(String trackTraceId)
	{
		return (r, q, cb) -> cb.equal(r.get(InpakbareUitnodiging_.trackTraceId), trackTraceId);
	}
}
