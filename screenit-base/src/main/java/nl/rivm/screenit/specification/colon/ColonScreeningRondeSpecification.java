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
import java.time.LocalDateTime;
import java.util.Date;
import java.util.List;

import javax.persistence.criteria.From;
import javax.persistence.criteria.Join;
import javax.persistence.criteria.JoinType;
import javax.persistence.criteria.Path;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import nl.rivm.screenit.model.ScreeningRonde_;
import nl.rivm.screenit.model.TablePerClassHibernateObject_;
import nl.rivm.screenit.model.berichten.enums.VerslagStatus;
import nl.rivm.screenit.model.colon.ColonAfmelding;
import nl.rivm.screenit.model.colon.ColonBrief;
import nl.rivm.screenit.model.colon.ColonBrief_;
import nl.rivm.screenit.model.colon.ColonConclusie;
import nl.rivm.screenit.model.colon.ColonDossier;
import nl.rivm.screenit.model.colon.ColonDossier_;
import nl.rivm.screenit.model.colon.ColonIntakeAfspraak;
import nl.rivm.screenit.model.colon.ColonIntakeAfspraak_;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.colon.ColonScreeningRonde_;
import nl.rivm.screenit.model.colon.ColonVerslag;
import nl.rivm.screenit.model.colon.ColonVerslag_;
import nl.rivm.screenit.model.colon.IFOBTTest_;
import nl.rivm.screenit.model.colon.OpenUitnodiging;
import nl.rivm.screenit.model.colon.OpenUitnodiging_;
import nl.rivm.screenit.model.colon.enums.ColonAfspraakStatus;
import nl.rivm.screenit.model.colon.enums.ColonConclusieType;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.specification.ExtendedSpecification;
import nl.rivm.screenit.specification.algemeen.AfmeldingSpecification;
import nl.rivm.screenit.specification.algemeen.BriefSpecification;
import nl.rivm.screenit.util.DateUtil;

import org.springframework.data.jpa.domain.Specification;

import static nl.rivm.screenit.specification.DateSpecification.truncate;
import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.colon.ColonFITSpecification.heeftOngunstigeReguliereOfStudieUitslag;
import static nl.rivm.screenit.specification.colon.ColonVerslagSpecification.heeftColonVerslagStatus;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class ColonScreeningRondeSpecification
{
	public static Specification<ColonScreeningRonde> heeftHuisarts()
	{
		return (r, q, cb) -> cb.isNotNull(r.get(ColonScreeningRonde_.colonHuisarts));
	}

	public static ExtendedSpecification<ColonScreeningRonde> heeftGeenAfsprakenZonderVervolg(LocalDate peilDatum)
	{
		return (r, q, cb) ->
		{
			var subquery = q.subquery(Long.class);
			var subRoot = subquery.from(ColonScreeningRonde.class);
			var testenJoin = join(subRoot, ColonScreeningRonde_.ifobtTesten);
			var afspraakJoin = join(subRoot, ColonScreeningRonde_.laatsteAfspraak);
			var conclusieJoin = join(afspraakJoin, ColonIntakeAfspraak_.conclusie, JoinType.LEFT);
			var afmeldingJoin = join(subRoot, ColonScreeningRonde_.laatsteAfmelding, JoinType.LEFT);

			subquery.select((subRoot.get(TablePerClassHibernateObject_.id)))
				.where(
					isEersteOngunstigeUitslagUitLaatsteRonde(testenJoin.get(IFOBTTest_.statusDatum), subRoot.get(TablePerClassHibernateObject_.id), peilDatum)
						.and(heefGeenVervolg(afspraakJoin, conclusieJoin, afmeldingJoin)
						).toPredicate(r, q, cb)
				);
			return cb.not(r.in(subquery));
		};
	}

	public static ExtendedSpecification<ColonScreeningRonde> isEersteOngunstigeUitslagUitLaatsteRonde(Path<Date> fitStatusDatum, Path<Long> screeningRondeId,
		LocalDate maxLengteRondeDatum)
	{
		return (r, q, cb) ->
		{
			var subquery = q.subquery(Integer.class);
			var subRoot = subquery.from(ColonDossier.class);
			var laatsteScreeningRondeJoin = join(subRoot, ColonDossier_.laatsteScreeningRonde);
			var testenJoin = join(laatsteScreeningRondeJoin, ColonScreeningRonde_.ifobtTesten);

			subquery.select(cb.literal(1));
			subquery.where(cb.and(
				heeftOngunstigeReguliereOfStudieUitslag().with(root -> testenJoin).toPredicate(r, q, cb),
				cb.equal(subRoot.get(ColonDossier_.laatsteScreeningRonde), screeningRondeId)));
			subquery.groupBy(laatsteScreeningRondeJoin.get(TablePerClassHibernateObject_.id));
			subquery.having(cb.equal(cb.least(testenJoin.get(IFOBTTest_.statusDatum)), fitStatusDatum));
			return cb.and(cb.exists(subquery), cb.lessThanOrEqualTo(fitStatusDatum, DateUtil.toUtilDate(maxLengteRondeDatum)));
		};
	}

	public static ExtendedSpecification<ColonScreeningRonde> heeftGeenBriefVanTypeIn(List<BriefType> briefTypes)
	{
		return (r, q, cb) ->
		{
			var subquery = q.subquery(Long.class);
			var subRoot = subquery.from(ColonBrief.class);
			var subqueryRoot = subRoot.get(ColonBrief_.screeningRonde);

			subquery.select(cb.literal(1L));
			subquery.where(cb.and(
				cb.equal(subqueryRoot, r.get(TablePerClassHibernateObject_.id)),
				BriefSpecification.heeftBriefTypeIn(briefTypes).toPredicate(subRoot, q, cb)
			));

			return cb.not(cb.exists(subquery));
		};
	}

	public static ExtendedSpecification<ColonScreeningRonde> heeftGeenLaatsteAfspraak()
	{
		return (r, q, cb) -> cb.isNull(r.get(ColonScreeningRonde_.laatsteAfspraak));
	}

	public static ExtendedSpecification<ColonScreeningRonde> heeftGeenLaatsteUitnodiging()
	{
		return (r, q, cb) -> cb.isNull(r.get(ColonScreeningRonde_.laatsteUitnodiging));
	}

	public static ExtendedSpecification<ColonScreeningRonde> heeftGeenFit()
	{
		return (r, q, cb) -> cb.isEmpty(r.get(ColonScreeningRonde_.IFOBT_TESTEN));
	}

	public static ExtendedSpecification<ColonScreeningRonde> heeftBriefZonderFit()
	{
		return heeftGeenFit().and(BriefSpecification.heeftBriefType(BriefType.COLON_UITNODIGING_ZONDER_FIT).with(ColonScreeningRonde_.laatsteBrief, JoinType.LEFT));
	}

	public static ExtendedSpecification<ColonScreeningRonde> heeftGeenAfgerondeVerslagen()
	{
		return (r, q, cb) ->
		{
			var subquery = q.subquery(ColonScreeningRonde.class);
			var subRoot = subquery.from(ColonVerslag.class);
			subquery.select(join(subRoot, ColonVerslag_.screeningRonde))
				.where(heeftColonVerslagStatus(VerslagStatus.AFGEROND).toPredicate(subRoot, q, cb))
				.distinct(true);
			return cb.not(r.in(subquery));
		};
	}

	public static ExtendedSpecification<ColonScreeningRonde> heefGeenOpenUitnodigingNa(LocalDateTime peilmoment)
	{
		return (r, q, cb) ->
		{
			var subquery = q.subquery(Long.class);
			var subRoot = subquery.from(OpenUitnodiging.class);
			var oudeAfspraakJoin = join(subRoot, OpenUitnodiging_.oudeAfspraak);
			var rondeJoin = join(subRoot, OpenUitnodiging_.ronde);
			var oudeAfspraakConclusieJoin = join(oudeAfspraakJoin, ColonIntakeAfspraak_.conclusie, JoinType.LEFT);
			var rondeVanOudeAfspraakJoin = join(oudeAfspraakJoin, ColonIntakeAfspraak_.colonScreeningRonde, JoinType.LEFT);
			var laatsteAfmeldingVanRondeVanOudeAfspraakJoin = join(rondeVanOudeAfspraakJoin, ColonScreeningRonde_.laatsteAfmelding, JoinType.LEFT);
			var laatsteAfspraakVanRondeJoin = join(rondeJoin, ColonScreeningRonde_.laatsteAfspraak, JoinType.LEFT);
			var laatsteAfmeldingVanRondeJoin = join(rondeJoin, ColonScreeningRonde_.laatsteAfmelding, JoinType.LEFT);
			var conclusieLaatsteAfspraakVanRondeJoin = join(laatsteAfspraakVanRondeJoin, ColonIntakeAfspraak_.conclusie, JoinType.LEFT);

			subquery.select(join(subRoot, OpenUitnodiging_.ronde).get(TablePerClassHibernateObject_.id))
				.where(
					heeftCreatieDatumVoorOfOp(peilmoment).with(subroot -> rondeJoin)
						.and(ColonIntakeAfspraakSpecification.heeftStatus(ColonAfspraakStatus.GEPLAND)
							.and(ColonIntakeAfspraakSpecification.heeftAfspraakXDagenVoorStartRonde(rondeJoin, 3)).with(subroot -> oudeAfspraakJoin)
							.or(heefGeenVervolg(oudeAfspraakJoin, oudeAfspraakConclusieJoin, laatsteAfmeldingVanRondeVanOudeAfspraakJoin).with(subroot -> rondeVanOudeAfspraakJoin))
						)
						.and(heeftGeenLaatsteAfspraak().with(subroot -> rondeJoin)
							.or(heefGeenVervolg(laatsteAfspraakVanRondeJoin, conclusieLaatsteAfspraakVanRondeJoin, laatsteAfmeldingVanRondeJoin).with(subroot -> rondeJoin))
						).toPredicate(subRoot, q, cb)
				);

			return cb.not(r.in(subquery));
		};
	}

	public static ExtendedSpecification<ColonScreeningRonde> heeftCreatieDatumVoorOfOp(LocalDateTime peilmoment)
	{
		return (r, q, cb) -> cb.lessThanOrEqualTo(r.get(ColonScreeningRonde_.creatieDatum), DateUtil.toUtilDate(peilmoment));
	}

	private static ExtendedSpecification<ColonScreeningRonde> heefGeenVervolg(From<?, ColonIntakeAfspraak> afspraakJoin,
		Join<ColonIntakeAfspraak, ColonConclusie> conclusieJoin, Join<ColonScreeningRonde, ColonAfmelding> afmeldingJoin)
	{
		return heeftGeannuleerdeAfspraakEnGeenConclusie(afspraakJoin, conclusieJoin)
			.or(heeftEenmaligAfgemeldGeenIntakeConclusieMaarWelFitUitslag(afmeldingJoin, conclusieJoin))
			.or(ColonConclusieSpecification.heeftTypeIn(List.of(ColonConclusieType.NO_SHOW, ColonConclusieType.CLIENT_WIL_ANDERE_INTAKELOKATIE))
				.with(subroot -> conclusieJoin));
	}

	private static ExtendedSpecification<ColonScreeningRonde> heeftGeannuleerdeAfspraakEnGeenConclusie(From<?, ColonIntakeAfspraak> afspraakJoin,
		Join<ColonIntakeAfspraak, ColonConclusie> conclusieJoin)
	{

		ExtendedSpecification<ColonScreeningRonde> heeftStatuses = ColonIntakeAfspraakSpecification.heeftStatusIn(ColonAfspraakStatus.GEANNULEERD).with(r -> afspraakJoin);
		return heeftStatuses.and(ColonConclusieSpecification.heeftGeenType().with(r -> conclusieJoin));
	}

	private static ExtendedSpecification<ColonScreeningRonde> heeftEenmaligAfgemeldGeenIntakeConclusieMaarWelFitUitslag(Join<ColonScreeningRonde, ColonAfmelding> afmeldingJoin,
		Join<ColonIntakeAfspraak, ColonConclusie> conclusieJoin)
	{
		ExtendedSpecification<ColonScreeningRonde> heeftGeenConclusieType = ColonConclusieSpecification.heeftGeenType().with(r -> conclusieJoin);
		return heeftGeenConclusieType.and(AfmeldingSpecification.isEenmaligAfgemeld().with(r -> afmeldingJoin));
	}

	public static ExtendedSpecification<ColonScreeningRonde> heeftCreatieDatum(Date datum)
	{
		return (r, q, cb) -> cb.equal(truncate("day", r.get(ScreeningRonde_.creatieDatum), cb), datum);
	}
}
