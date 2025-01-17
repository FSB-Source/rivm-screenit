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
import java.util.Date;
import java.util.List;
import java.util.function.Function;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Expression;
import javax.persistence.criteria.From;
import javax.persistence.criteria.JoinType;
import javax.persistence.criteria.Subquery;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import nl.rivm.screenit.model.Brief_;
import nl.rivm.screenit.model.Client_;
import nl.rivm.screenit.model.GbaPersoon_;
import nl.rivm.screenit.model.ScannedFormulier_;
import nl.rivm.screenit.model.ScreeningRondeStatus;
import nl.rivm.screenit.model.ScreeningRonde_;
import nl.rivm.screenit.model.TablePerClassHibernateObject_;
import nl.rivm.screenit.model.cervix.CervixBrief_;
import nl.rivm.screenit.model.cervix.CervixDossier_;
import nl.rivm.screenit.model.cervix.CervixLabformulier_;
import nl.rivm.screenit.model.cervix.CervixMonster;
import nl.rivm.screenit.model.cervix.CervixMonster_;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde_;
import nl.rivm.screenit.model.cervix.CervixUitnodiging_;
import nl.rivm.screenit.model.cervix.CervixUitstrijkje;
import nl.rivm.screenit.model.cervix.CervixUitstrijkje_;
import nl.rivm.screenit.model.cervix.enums.CervixLeeftijdcategorie;
import nl.rivm.screenit.model.cervix.enums.CervixMonsterType;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.specification.ExtendedSpecification;
import nl.rivm.screenit.specification.SpecificationUtil;

import org.springframework.data.jpa.domain.Specification;

import static nl.rivm.screenit.model.cervix.enums.CervixLabformulierStatus.GECONTROLEERD;
import static nl.rivm.screenit.model.cervix.enums.CervixLabformulierStatus.GECONTROLEERD_CYTOLOGIE;
import static nl.rivm.screenit.model.cervix.enums.CervixLabformulierStatus.HUISARTS_ONBEKEND;
import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.cervix.CervixLabformulierSpecification.heeftGeenScanDatum;
import static nl.rivm.screenit.specification.cervix.CervixMonsterSpecification.heeftGeenOntvangstDatum;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class CervixScreeningRondeSpecification
{
	public static Specification<CervixScreeningRonde> heeftPersoon(String bsn)
	{
		return (r, q, cb) ->
		{
			var dossier = SpecificationUtil.join(r, CervixScreeningRonde_.dossier);
			var client = SpecificationUtil.join(dossier, CervixDossier_.client);
			var persoon = SpecificationUtil.join(client, Client_.persoon);

			return cb.equal(persoon.get(GbaPersoon_.bsn), bsn);
		};
	}

	public static Specification<CervixScreeningRonde> heeftMonsterInDossier(CervixMonster monster)
	{
		return (r, q, cb) ->
		{
			var dossier = SpecificationUtil.join(r, CervixScreeningRonde_.dossier);
			var rondesVanDossier = dossier.join(CervixDossier_.screeningRondes);
			var uitnodiging = rondesVanDossier.join(CervixScreeningRonde_.uitnodigingen);
			var monsterJoin = SpecificationUtil.join(uitnodiging, CervixUitnodiging_.monster);
			return cb.equal(monsterJoin, monster);
		};
	}

	public static Specification<CervixScreeningRonde> heeftStatusDatumVoorOfOp(Date peilDatum)
	{
		return (r, q, cb) -> cb.lessThanOrEqualTo(r.get(ScreeningRonde_.statusDatum), peilDatum);
	}

	public static ExtendedSpecification<CervixScreeningRonde> heeftControleUitstrijkjeDatumVoorOfOp(LocalDate peilDatum)
	{
		return (r, q, cb) -> cb.lessThanOrEqualTo(r.get(CervixScreeningRonde_.controleUitstrijkjeDatum), peilDatum);
	}

	public static Specification<CervixScreeningRonde> getZASsenHandmatigAangevraagdSpecification(CervixScreeningRonde ronde, boolean aangevraagdDoorClient)
	{
		return ((r, cq, cb) ->
		{
			var uitnodigingJoin = r.join(CervixScreeningRonde_.uitnodigingen);
			var uitnodiging = uitnodigingJoin.on(
				cb.equal(uitnodigingJoin.get(CervixUitnodiging_.screeningRonde), r));

			var briefJoin = uitnodiging.join(CervixUitnodiging_.brief);
			var brief = briefJoin.on(cb.equal(briefJoin.get(CervixBrief_.uitnodiging), uitnodiging.get(CervixUitnodiging_.brief)));

			var juisteRonde = cb.equal(r, ronde);
			var monsterTypeZAS = cb.equal(uitnodiging.get(CervixUitnodiging_.monsterType), CervixMonsterType.ZAS);
			var zasAangevraagdDoorClient = cb.equal(uitnodiging.get(CervixUitnodiging_.zasAangevraagdDoorClient), aangevraagdDoorClient);
			var filterOpBriefType = cb.not(brief.get(Brief_.briefType).in(BriefType.getCervixZasUitnodigingNietDirectHerzendbaarBrieven()));

			return cb.and(juisteRonde, monsterTypeZAS, zasAangevraagdDoorClient, filterOpBriefType);
		});
	}

	public static Specification<CervixScreeningRonde> heeftNietLeeftijdCategorie(CervixLeeftijdcategorie leeftijdcategorie)
	{
		return (r, q, cb) -> cb.notEqual(r.get(CervixScreeningRonde_.leeftijdcategorie), leeftijdcategorie);
	}

	public static ExtendedSpecification<CervixScreeningRonde> heeftLeeftijdCategorie(CervixLeeftijdcategorie leeftijdcategorie)
	{
		return (r, q, cb) -> cb.equal(r.get(CervixScreeningRonde_.leeftijdcategorie), leeftijdcategorie);
	}

	public static ExtendedSpecification<CervixScreeningRonde> heeftStatus(ScreeningRondeStatus status)
	{
		return (r, q, cb) -> cb.equal(r.get(ScreeningRonde_.status), status);
	}

	public static ExtendedSpecification<CervixScreeningRonde> heeftGeenLaatsteUitnodiging()
	{
		return (r, q, cb) ->
			cb.isNull(r.get(CervixScreeningRonde_.laatsteUitnodiging));
	}

	public static ExtendedSpecification<CervixScreeningRonde> heeftGeenUitstel()
	{
		return (r, q, cb) -> cb.isNull(r.get(CervixScreeningRonde_.uitstel));
	}

	public static ExtendedSpecification<CervixScreeningRonde> isAangemeld(boolean aangemeld)
	{
		return (r, q, cb) -> cb.equal(r.get(ScreeningRonde_.aangemeld), aangemeld);
	}

	public static ExtendedSpecification<CervixScreeningRonde> heeftInVervolgonderzoekDatum()
	{
		return (r, q, cb) -> cb.isNotNull(r.get(CervixScreeningRonde_.inVervolgonderzoekDatum));
	}

	public static ExtendedSpecification<CervixScreeningRonde> heeftInVervolgonderzoekDatumVoor(Expression<Date> peilDatum)
	{
		return (r, q, cb) -> cb.lessThan(r.get(CervixScreeningRonde_.inVervolgonderzoekDatum), peilDatum);
	}

	public static ExtendedSpecification<CervixScreeningRonde> heeftGeenUitnodigingVervolgonderzoek()
	{
		return (r, q, cb) -> cb.isNull(r.get(CervixScreeningRonde_.uitnodigingVervolgonderzoek));
	}

	public static Subquery<Long> heeftInVervolgonderzoekDatumVoorOntvangstDatumOfScanDatum(CriteriaQuery<?> q, CriteriaBuilder cb)
	{
		var subquery = q.subquery(Long.class);
		var subRoot = subquery.from(CervixUitstrijkje.class);
		var screeningRondeJoin = screeningRondeJoin().apply(subRoot);
		var labformulierJoin = join(subRoot, CervixUitstrijkje_.labformulier, JoinType.LEFT)
			.on(subRoot.get(CervixUitstrijkje_.labformulier).get(CervixLabformulier_.status)
				.in(List.of(GECONTROLEERD, GECONTROLEERD_CYTOLOGIE, HUISARTS_ONBEKEND)));

		subquery.select(screeningRondeJoin.get(TablePerClassHibernateObject_.id)).distinct(true)
			.where(
				heeftInVervolgonderzoekDatum().with(screeningRondeJoin())
					.and(
						heeftInVervolgonderzoekDatumVoor(subRoot.get(CervixMonster_.ontvangstdatum))
							.with(screeningRondeJoin())
							.and(heeftGeenScanDatum().with(root -> labformulierJoin))
							.or(
								heeftInVervolgonderzoekDatumVoor(labformulierJoin.get(ScannedFormulier_.scanDatum))
									.with(screeningRondeJoin())
									.and(heeftGeenOntvangstDatum())
							)
							.or(
								heeftInVervolgonderzoekDatumVoor(subRoot.get(CervixMonster_.ontvangstdatum))
									.with(screeningRondeJoin())
									.and(heeftInVervolgonderzoekDatumVoor(labformulierJoin.get(ScannedFormulier_.scanDatum))
										.with(screeningRondeJoin()))
							)
					).toPredicate(subRoot, q, cb)
			);

		return subquery;
	}

	private static Function<From<?, ? extends CervixUitstrijkje>, From<?, ? extends CervixScreeningRonde>> screeningRondeJoin()
	{
		return q -> join(q, CervixMonster_.ontvangstScreeningRonde);
	}
}
