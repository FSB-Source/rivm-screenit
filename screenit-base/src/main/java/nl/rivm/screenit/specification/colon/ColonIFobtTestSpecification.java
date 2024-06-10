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
import java.util.List;

import javax.persistence.criteria.JoinType;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;

import nl.rivm.screenit.model.Brief_;
import nl.rivm.screenit.model.ClientBrief_;
import nl.rivm.screenit.model.Dossier_;
import nl.rivm.screenit.model.TablePerClassHibernateObject_;
import nl.rivm.screenit.model.Uitnodiging_;
import nl.rivm.screenit.model.colon.ColonBrief;
import nl.rivm.screenit.model.colon.ColonBrief_;
import nl.rivm.screenit.model.colon.ColonDossier;
import nl.rivm.screenit.model.colon.ColonDossier_;
import nl.rivm.screenit.model.colon.ColonScreeningRonde_;
import nl.rivm.screenit.model.colon.ColonUitnodiging;
import nl.rivm.screenit.model.colon.ColonUitnodiging_;
import nl.rivm.screenit.model.colon.IFOBTTest;
import nl.rivm.screenit.model.colon.IFOBTTest_;
import nl.rivm.screenit.model.colon.IFOBTType;
import nl.rivm.screenit.model.colon.enums.IFOBTTestStatus;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.specification.DateSpecification;
import nl.rivm.screenit.specification.SpecificationUtil;
import nl.rivm.screenit.specification.algemeen.ClientSpecification;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject_;

import org.springframework.data.jpa.domain.Specification;

import static nl.rivm.screenit.specification.SpecificationUtil.join;

@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class ColonIFobtTestSpecification
{
	public static Specification<IFOBTTest> heeftBarcode(String barcode)
	{
		return (r, q, cb) -> cb.equal(r.get(IFOBTTest_.barcode), barcode);
	}

	public static Specification<IFOBTTest> heeftDossier(ColonDossier dossier)
	{
		return (r, q, cb) -> cb.equal(SpecificationUtil.join(r, IFOBTTest_.colonScreeningRonde).get(ColonScreeningRonde_.dossier), dossier);
	}

	public static Specification<IFOBTTest> isStatusDatumVoorOfOp(LocalDate minimaleSignaleringsDatum)
	{
		return (r, q, cb) ->
			cb.lessThanOrEqualTo(r.get(IFOBTTest_.statusDatum), DateUtil.toUtilDate(minimaleSignaleringsDatum));
	}

	public static Specification<IFOBTTest> heeftFitType(IFOBTType type)
	{
		return (r, q, cb) ->
			cb.equal(r.get(IFOBTTest_.type), type);
	}

	public static Specification<IFOBTTest> isDatumLaatstGecontroleerdNa(LocalDate signalerenVanaf)
	{
		return (r, q, cb) ->
		{
			var screeningRondeJoin = SpecificationUtil.join(r, IFOBTTest_.colonScreeningRonde);
			var dossierJoin = SpecificationUtil.join(screeningRondeJoin, ColonScreeningRonde_.dossier);
			return cb.and(
				cb.greaterThan(dossierJoin.get(Dossier_.datumLaatstGecontroleerdeSignalering), DateUtil.toUtilDate(signalerenVanaf)),
				cb.greaterThan(DateSpecification.truncateToDay(r.get(IFOBTTest_.analyseDatum), cb),
					DateSpecification.truncateToDay(dossierJoin.get(Dossier_.datumLaatstGecontroleerdeSignalering), cb)));
		};
	}

	public static Specification<IFOBTTest> isFitAnalyseDatumNa(LocalDate signalerenVanaf)
	{
		return (r, q, cb) ->
			cb.greaterThan(r.get(IFOBTTest_.analyseDatum), DateUtil.toUtilDate(signalerenVanaf));
	}

	public static Specification<IFOBTTest> isGecontroleerdNaSignalering(LocalDate signalerenVanaf)
	{
		return (r, q, cb) ->
		{
			var screeningRondeJoin = SpecificationUtil.join(r, IFOBTTest_.colonScreeningRonde);
			var dossierJoin = SpecificationUtil.join(screeningRondeJoin, ColonScreeningRonde_.dossier);
			return cb.or(
				cb.lessThanOrEqualTo(DateSpecification.truncateToDay(dossierJoin.get(Dossier_.datumLaatstGecontroleerdeSignalering), cb),
					DateUtil.toUtilDate(signalerenVanaf)),
				cb.isNull(dossierJoin.get(Dossier_.datumLaatstGecontroleerdeSignalering))
			);
		};
	}

	public static Specification<IFOBTTest> heeftGeenUitslagBrief()
	{
		return (r, q, cb) ->
		{
			var subquery = q.subquery(ColonBrief.class);
			var briefRoot = subquery.from(ColonBrief.class);
			var projectBriefJoin = SpecificationUtil.join(briefRoot, ClientBrief_.projectBrief, JoinType.LEFT);

			subquery.select(briefRoot);
			subquery.where(
				cb.equal(briefRoot.get(ColonBrief_.ifobtTest), r.get(AbstractHibernateObject_.id)),
				cb.or(
					cb.and(
						cb.equal(briefRoot.get(ClientBrief_.vervangendeProjectBrief), false),
						briefRoot.get(Brief_.briefType).in(List.of(BriefType.COLON_UITSLAG_BRIEVEN)),
						cb.equal(briefRoot.get(Brief_.gegenereerd), true)
					),
					cb.and(
						cb.equal(briefRoot.get(ClientBrief_.vervangendeProjectBrief), true),
						projectBriefJoin.get(Brief_.briefType).in(List.of(BriefType.COLON_UITSLAG_BRIEVEN)),
						cb.equal(projectBriefJoin.get(Brief_.gegenereerd), true)
					)
				)
			);
			return cb.not(cb.exists(subquery));
		};

	}

	public static Specification<IFOBTTest> heeftGeenNieuweUitnodiging()
	{
		return (r, q, cb) ->
		{
			var subquery = q.subquery(ColonUitnodiging.class);
			var uitnodigingRoot = subquery.from(ColonUitnodiging.class);
			var screeningRondeJoin = SpecificationUtil.join(r, IFOBTTest_.colonScreeningRonde);
			var dossierJoin = SpecificationUtil.join(screeningRondeJoin, ColonScreeningRonde_.dossier);
			subquery.select(uitnodigingRoot);
			subquery.where(cb.equal(SpecificationUtil.join(uitnodigingRoot, ColonUitnodiging_.screeningRonde, JoinType.INNER).get(ColonScreeningRonde_.dossier),
					dossierJoin.get(TablePerClassHibernateObject_.id)),
				cb.equal(DateSpecification.truncateToDay(uitnodigingRoot.get(Uitnodiging_.creatieDatum), cb),
					DateSpecification.truncateToDay(r.get(IFOBTTest_.statusDatum), cb)));
			return cb.not(cb.exists(subquery));
		};
	}

	public static Specification<IFOBTTest> heeftNieuweUitnodigingZonderGekoppeldeFit()
	{
		return (r, q, cb) ->
		{
			var subquery = q.subquery(ColonUitnodiging.class);
			var uitnodigingZonderFitRoot = subquery.from(ColonUitnodiging.class);
			var screeningRondeJoin = SpecificationUtil.join(r, IFOBTTest_.colonScreeningRonde);
			var dossierJoin = SpecificationUtil.join(screeningRondeJoin, ColonScreeningRonde_.dossier);
			subquery.select(uitnodigingZonderFitRoot);
			subquery.where(
				cb.equal(uitnodigingZonderFitRoot.get(ColonUitnodiging_.screeningRonde).get(ColonScreeningRonde_.dossier), dossierJoin.get(TablePerClassHibernateObject_.id)),
				cb.equal(DateSpecification.truncateToDay(uitnodigingZonderFitRoot.get(Uitnodiging_.creatieDatum), cb),
					DateSpecification.truncateToDay(r.get(IFOBTTest_.statusDatum), cb)),
				cb.isNull(uitnodigingZonderFitRoot.get(ColonUitnodiging_.gekoppeldeTest)));
			return cb.exists(subquery);
		};
	}

	public static Specification<IFOBTTest> valideerFitUitslagStatus(LocalDate signalerenVanaf)
	{
		return (r, q, cb) ->
			cb.and(
				cb.or(
					cb.and(
						isGecontroleerdNaSignalering(signalerenVanaf).toPredicate(r, q, cb),
						isFitAnalyseDatumNa(signalerenVanaf).toPredicate(r, q, cb)
					),
					cb.and(
						isDatumLaatstGecontroleerdNa(signalerenVanaf).toPredicate(r, q, cb)
					)
				),
				cb.or(
					cb.and(
						heeftStatussen(List.of(IFOBTTestStatus.UITGEVOERD)).toPredicate(r, q, cb),
						heeftGeenUitslagBrief().toPredicate(r, q, cb)
					),
					cb.and(
						heeftStatussen(List.of(IFOBTTestStatus.VERVALDATUMVERLOPEN, IFOBTTestStatus.NIETTEBEOORDELEN)).toPredicate(r, q, cb),
						cb.or(
							cb.and(
								heeftGeenNieuweUitnodiging().toPredicate(r, q, cb),
								heeftGeenUitslagBrief().toPredicate(r, q, cb)
							),
							heeftNieuweUitnodigingZonderGekoppeldeFit().toPredicate(r, q, cb)
						)
					)
				)
			);
	}

	public static Specification<IFOBTTest> heeftActieveClient()
	{
		return ClientSpecification.heeftActieveClientPredicate()
			.toSpecification(r ->
			{
				var ronde = join(r, IFOBTTest_.colonScreeningRonde);
				var dossier = join(ronde, ColonScreeningRonde_.dossier);
				return join(dossier, ColonDossier_.client);
			});
	}

	public static Specification<IFOBTTest> heeftStatussen(List<IFOBTTestStatus> statussen)
	{
		return (r, q, cb) -> r.get(IFOBTTest_.status).in(statussen);
	}

}
