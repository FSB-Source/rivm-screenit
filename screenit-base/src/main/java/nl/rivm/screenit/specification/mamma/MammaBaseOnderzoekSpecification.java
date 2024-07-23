package nl.rivm.screenit.specification.mamma;

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
import java.util.Date;

import javax.persistence.criteria.Join;
import javax.persistence.criteria.JoinType;
import javax.persistence.criteria.Root;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;

import nl.rivm.screenit.model.ClientBrief_;
import nl.rivm.screenit.model.TablePerClassHibernateObject_;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.mamma.MammaAfspraak_;
import nl.rivm.screenit.model.mamma.MammaBrief;
import nl.rivm.screenit.model.mamma.MammaBrief_;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaDossier_;
import nl.rivm.screenit.model.mamma.MammaMammografie_;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;
import nl.rivm.screenit.model.mamma.MammaOnderzoek_;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde_;
import nl.rivm.screenit.model.mamma.MammaUitnodiging;
import nl.rivm.screenit.model.mamma.MammaUitnodiging_;
import nl.rivm.screenit.model.mamma.enums.MammaMammografieIlmStatus;
import nl.rivm.screenit.model.mamma.enums.MammaOnderzoekStatus;
import nl.rivm.screenit.specification.algemeen.ClientSpecification;
import nl.rivm.screenit.specification.cervix.ProjectBriefSpecification;
import nl.rivm.screenit.util.DateUtil;

import org.springframework.data.jpa.domain.Specification;

import static nl.rivm.screenit.specification.DateSpecification.truncate;
import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.mamma.MammaBriefSpecification.heeftBriefInBrieftypes;
import static nl.rivm.screenit.specification.mamma.MammaBriefSpecification.heeftVervangendeProjectBrief;
import static nl.rivm.screenit.specification.mamma.MammaBriefSpecification.isGegenereerd;

@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class MammaBaseOnderzoekSpecification
{

	public static Specification<MammaOnderzoek> heeftMissendeUitslag(LocalDate signalerenVanaf)
	{
		return (r, q, cb) ->
		{
			var rondeJoin = rondeJoin(r);
			var dossierJoin = join(rondeJoin, MammaScreeningRonde_.dossier);

			return cb.or(
				cb.and(
					cb.greaterThan(dossierJoin.get(MammaDossier_.datumLaatstGecontroleerdeSignalering), DateUtil.toUtilDate(signalerenVanaf)),
					cb.greaterThan(truncate("day", r.get(MammaOnderzoek_.creatieDatum), cb),
						truncate("day", dossierJoin.get(MammaDossier_.datumLaatstGecontroleerdeSignalering), cb))
				),
				cb.and(
					cb.or(
						cb.lessThanOrEqualTo(truncate("day", dossierJoin.get(MammaDossier_.datumLaatstGecontroleerdeSignalering), cb), DateUtil.toUtilDate(signalerenVanaf)),
						cb.isNull(dossierJoin.get(MammaDossier_.datumLaatstGecontroleerdeSignalering))
					),
					cb.greaterThan(r.get(MammaOnderzoek_.creatieDatum), DateUtil.toUtilDate(signalerenVanaf))
				)
			);
		};

	}

	private static Join<MammaUitnodiging, MammaScreeningRonde> rondeJoin(Root<MammaOnderzoek> r)
	{
		var afspraakJoin = join(r, MammaOnderzoek_.afspraak);
		var uitnodigingJoin = join(afspraakJoin, MammaAfspraak_.uitnodiging);
		return join(uitnodigingJoin, MammaUitnodiging_.screeningRonde);
	}

	public static Specification<MammaOnderzoek> heeftOnderzoekStatusNietOnderbroken(LocalDate minimaleSignaleringsDatum)
	{
		return (r, q, cb) -> cb.and(
			cb.notEqual(r.get(MammaOnderzoek_.status), MammaOnderzoekStatus.ONDERBROKEN),
			cb.lessThanOrEqualTo(truncate("day", r.get(MammaOnderzoek_.creatieDatum), cb), DateUtil.toUtilDate(minimaleSignaleringsDatum))
		);
	}

	public static Specification<MammaOnderzoek> heeftOnderzoekStatusOnderbroken(LocalDate minimaleSignaleringsDatumOnderbrokenOnderzoek)
	{
		return (r, q, cb) -> cb.and(
			cb.equal(r.get(MammaOnderzoek_.status), MammaOnderzoekStatus.ONDERBROKEN),
			cb.lessThanOrEqualTo(truncate("day", r.get(MammaOnderzoek_.creatieDatum), cb), DateUtil.toUtilDate(minimaleSignaleringsDatumOnderbrokenOnderzoek))
		);
	}

	public static Specification<MammaOnderzoek> heeftIlmStatusBeschikbaarOfGeweest()
	{
		return (r, q, cb) ->
		{
			var mammografieJoin = join(r, MammaOnderzoek_.mammografie);
			return mammografieJoin.get(MammaMammografie_.ilmStatus).in(MammaMammografieIlmStatus.BEELDEN_BESCHIKBAAR_OF_BESCHIKBAAR_GEWEEST);
		};
	}

	public static Specification<MammaOnderzoek> heeftDossierWatOvereenKomtMetRonde(MammaDossier dossier)
	{
		return (r, q, cb) ->
		{
			var rondeJoin = rondeJoin(r);
			return cb.equal(rondeJoin.get(MammaScreeningRonde_.dossier), dossier);
		};
	}

	public static Specification<MammaOnderzoek> heeftOnderzoekZonderUitslagBrieven()
	{
		return (r, q, cb) -> cb.or(
			cb.and(heeftGeenBrievenBijOnderzoek().toPredicate(r, q, cb)),
			heeftOnderzoekNaLaatsteUitslagBrief().toPredicate(r, q, cb)
		);
	}

	public static Specification<MammaOnderzoek> heeftActieveClient()
	{
		return ClientSpecification.heeftActieveClientPredicate().toSpecification(r ->
		{
			var rondeJoin = rondeJoin(r);
			var dossierJoin = join(rondeJoin, MammaScreeningRonde_.dossier);
			return join(dossierJoin, MammaDossier_.client);
		});
	}

	private static Specification<MammaOnderzoek> heeftGeenBrievenBijOnderzoek()
	{
		return (r, q, cb) ->
		{
			var rondeJoin = rondeJoin(r);

			var subquery = q.subquery(Long.class);
			var subRoot = subquery.from(MammaBrief.class);
			var projectBriefJoin = subRoot.join(ClientBrief_.projectBrief, JoinType.LEFT);
			subquery.select(subRoot.get(TablePerClassHibernateObject_.id)).where(cb.and(
				cb.equal(subRoot.get(MammaBrief_.screeningRonde), rondeJoin),
				cb.or(
					cb.and(
						heeftVervangendeProjectBrief(false).withPath(cb, subRoot),
						heeftBriefInBrieftypes(BriefType.getMammaUitslagBriefTypen()).withPath(cb, subRoot),
						isGegenereerd(true).withPath(cb, subRoot)
					),
					cb.and(
						heeftVervangendeProjectBrief(true).withPath(cb, subRoot),
						ProjectBriefSpecification.heeftBriefInBrieftypes(BriefType.getMammaUitslagBriefTypen()).withPath(cb, projectBriefJoin),
						ProjectBriefSpecification.isGegenereerd(true).withPath(cb, projectBriefJoin)
					))));
			return cb.not(cb.exists(subquery));
		};
	}

	private static Specification<MammaOnderzoek> heeftOnderzoekNaLaatsteUitslagBrief()
	{
		return (r, q, cb) ->
		{
			var rondeJoin = rondeJoin(r);

			var subquery = q.subquery(Date.class);
			var subRoot = subquery.from(MammaBrief.class);
			var projectBriefJoin = subRoot.join(ClientBrief_.projectBrief, JoinType.LEFT);
			subquery.select(cb.greatest(subRoot.get(MammaBrief_.creatieDatum))).where(cb.and(
				cb.equal(subRoot.get(MammaBrief_.screeningRonde), rondeJoin),
				cb.or(
					cb.and(
						heeftVervangendeProjectBrief(false).withPath(cb, subRoot),
						heeftBriefInBrieftypes(BriefType.getMammaUitslagBriefTypen()).withPath(cb, subRoot),
						isGegenereerd(true).withPath(cb, subRoot)
					),
					cb.and(
						heeftVervangendeProjectBrief(true).withPath(cb, subRoot),
						ProjectBriefSpecification.heeftBriefInBrieftypes(BriefType.getMammaUitslagBriefTypen()).withPath(cb, projectBriefJoin),
						ProjectBriefSpecification.isGegenereerd(true).withPath(cb, projectBriefJoin)
					))));

			return cb.greaterThan(r.get(MammaOnderzoek_.creatieDatum), subquery);
		};
	}

}
