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

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Collection;
import java.util.Date;
import java.util.List;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.From;
import javax.persistence.criteria.Join;
import javax.persistence.criteria.JoinType;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import nl.rivm.screenit.model.BeoordelingsEenheid;
import nl.rivm.screenit.model.Brief_;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Client_;
import nl.rivm.screenit.model.Dossier_;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.TablePerClassHibernateObject_;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.MammaOnderzoekType;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaAfspraak_;
import nl.rivm.screenit.model.mamma.MammaBeoordeling_;
import nl.rivm.screenit.model.mamma.MammaBrief;
import nl.rivm.screenit.model.mamma.MammaBrief_;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaDossier_;
import nl.rivm.screenit.model.mamma.MammaMammografie;
import nl.rivm.screenit.model.mamma.MammaMammografie_;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;
import nl.rivm.screenit.model.mamma.MammaOnderzoek_;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde_;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid_;
import nl.rivm.screenit.model.mamma.MammaUitnodiging;
import nl.rivm.screenit.model.mamma.MammaUitnodiging_;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus;
import nl.rivm.screenit.model.mamma.enums.MammaMammografieIlmStatus;
import nl.rivm.screenit.model.mamma.enums.MammaOnderzoekStatus;
import nl.rivm.screenit.model.mamma.enums.OnvolledigOnderzoekOption;
import nl.rivm.screenit.specification.ExtendedSpecification;
import nl.rivm.screenit.specification.algemeen.ClientSpecification;
import nl.rivm.screenit.specification.algemeen.PersoonSpecification;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject_;

import org.springframework.data.jpa.domain.Specification;

import com.google.common.collect.Range;

import static nl.rivm.screenit.specification.DateSpecification.bevatLocalDateToDate;
import static nl.rivm.screenit.specification.DateSpecification.truncate;
import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.SpecificationUtil.skipWhenEmptyExtended;
import static nl.rivm.screenit.specification.SpecificationUtil.skipWhenNullExtended;
import static nl.rivm.screenit.specification.algemeen.ClientBriefSpecification.heeftGegenereerdeBriefOfProjectBriefVanType;
import static nl.rivm.screenit.specification.algemeen.ScreeningRondeSpecification.isLopend;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class MammaOnderzoekSpecification
{
	public static ExtendedSpecification<MammaOnderzoek> isAangemaaktVanaf(LocalDate datum)
	{
		return (r, q, cb) -> cb.greaterThanOrEqualTo(r.get(MammaOnderzoek_.creatieDatum), DateUtil.toUtilDate(datum));
	}

	public static ExtendedSpecification<MammaOnderzoek> heeftStatus(MammaOnderzoekStatus status)
	{
		return (r, q, cb) -> cb.equal(r.get(MammaOnderzoek_.status), status);
	}

	public static ExtendedSpecification<MammaOnderzoek> isAangemaaktVoor(LocalDateTime peilMoment)
	{
		return (r, q, cb) -> cb.lessThan(r.get(MammaOnderzoek_.creatieDatum), DateUtil.toUtilDate(peilMoment));
	}

	public static Specification<MammaOnderzoek> heeftBeoordelingsEenheid(BeoordelingsEenheid beoordelingsEenheid)
	{
		return (r, q, cb) -> cb.equal(join(r, MammaOnderzoek_.screeningsEenheid).get(MammaScreeningsEenheid_.beoordelingsEenheid), beoordelingsEenheid);
	}

	public static ExtendedSpecification<MammaOnderzoek> isDoorgevoerd(boolean doorgevoerd)
	{
		return (r, q, cb) -> cb.equal(r.get(MammaOnderzoek_.isDoorgevoerd), doorgevoerd);
	}

	public static Specification<MammaOnderzoek> heeftGeenBeoordelingStatusIn(Collection<MammaBeoordelingStatus> statussen)
	{
		return (r, q, cb) -> cb.not(join(r, MammaOnderzoek_.beoordelingen).get(MammaBeoordeling_.status).in(statussen));
	}

	public static Specification<MammaOnderzoek> heeftMissendeUitslag(LocalDate signalerenVanaf)
	{
		return (r, q, cb) ->
		{
			var rondeJoin = screeningRondeJoin(r);
			var dossierJoin = join(rondeJoin, MammaScreeningRonde_.dossier);

			return cb.or(
				cb.and(
					cb.greaterThan(dossierJoin.get(Dossier_.datumLaatstGecontroleerdeSignalering), DateUtil.toUtilDate(signalerenVanaf)),
					cb.greaterThan(truncate("day", r.get(MammaOnderzoek_.creatieDatum), cb),
						truncate("day", dossierJoin.get(Dossier_.datumLaatstGecontroleerdeSignalering), cb))
				),
				cb.and(
					cb.or(
						cb.lessThanOrEqualTo(truncate("day", dossierJoin.get(Dossier_.datumLaatstGecontroleerdeSignalering), cb), DateUtil.toUtilDate(signalerenVanaf)),
						cb.isNull(dossierJoin.get(Dossier_.datumLaatstGecontroleerdeSignalering))
					),
					cb.greaterThan(r.get(MammaOnderzoek_.creatieDatum), DateUtil.toUtilDate(signalerenVanaf))
				)
			);
		};

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

	public static ExtendedSpecification<MammaOnderzoek> heeftOnvolledigOnderzoek(OnvolledigOnderzoekOption onvolledigOnderzoek)
	{
		return (r, q, cb) -> cb.equal(r.get(MammaOnderzoek_.onvolledigOnderzoek), onvolledigOnderzoek);
	}

	public static ExtendedSpecification<MammaOnderzoek> heeftStatusIn(Collection<MammaOnderzoekStatus> statussen)
	{
		return (r, q, cb) -> r.get(MammaOnderzoek_.status).in(statussen);
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
			var rondeJoin = screeningRondeJoin(r);
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
		return ClientSpecification.heeftActieveClient().withRoot(r ->
		{
			var rondeJoin = screeningRondeJoin(r);
			var dossierJoin = join(rondeJoin, MammaScreeningRonde_.dossier);
			return join(dossierJoin, MammaDossier_.client);
		});
	}

	public static Specification<MammaOnderzoek> filterBriefOnderbrokenOnderzoek(Boolean zoekenOpBriefMetOnderbrokenOnderzoek)
	{
		return (r, q, cb) ->
		{
			if (zoekenOpBriefMetOnderbrokenOnderzoek == null)
			{
				return null;
			}
			var rondeJoin = screeningRondeJoin(r);
			var briefJoin = join(rondeJoin, MammaScreeningRonde_.laatsteBrief, JoinType.LEFT);
			if (zoekenOpBriefMetOnderbrokenOnderzoek)
			{
				return cb.equal(briefJoin.get(Brief_.briefType), BriefType.MAMMA_OPROEP_OPNEMEN_CONTACT);
			}
			else
			{
				return cb.or(cb.isNull(briefJoin), cb.notEqual(briefJoin.get(Brief_.briefType), BriefType.MAMMA_OPROEP_OPNEMEN_CONTACT));
			}
		};
	}

	public static ExtendedSpecification<MammaOnderzoek> filterScreeningsEenheid(Collection<MammaScreeningsEenheid> screeningsEenheden)
	{
		return skipWhenEmptyExtended(screeningsEenheden, (r, q, cb) -> r.get(MammaOnderzoek_.screeningsEenheid).in(screeningsEenheden));
	}

	public static Specification<MammaOnderzoek> heeftScreeningsEenheid(MammaScreeningsEenheid screeningsEenheid)
	{
		return (r, q, cb) -> cb.equal(r.get(MammaOnderzoek_.screeningsEenheid), screeningsEenheid);
	}

	public static ExtendedSpecification<MammaOnderzoek> heeftMammografie()
	{
		return (r, q, cb) -> cb.isNotNull(r.get(MammaOnderzoek_.mammografie));
	}

	public static Specification<MammaOnderzoek> heeftLaatsteAfspraakVanLaatsteUitnodigingVanLaatsteRonde()
	{
		return (r, q, cb) ->
		{
			var afspraakJoin = join(r, MammaOnderzoek_.afspraak);
			var uitnodigingJoin = join(afspraakJoin, MammaAfspraak_.uitnodiging);
			var rondeJoin = join(uitnodigingJoin, MammaUitnodiging_.screeningRonde);
			var dossierJoin = join(rondeJoin, MammaScreeningRonde_.dossier);
			return cb.and(cb.equal(dossierJoin.get(MammaDossier_.laatsteScreeningRonde), rondeJoin),
				cb.equal(rondeJoin.get(MammaScreeningRonde_.laatsteUitnodiging), uitnodigingJoin),
				cb.equal(uitnodigingJoin.get(MammaUitnodiging_.laatsteAfspraak), afspraakJoin));
		};
	}

	public static Specification<MammaOnderzoek> heeftPersoonIsNietOverledenEnWoontInNederland()
	{
		return PersoonSpecification.isNietOverledenEnWoontInNederland().withRoot(MammaOnderzoekSpecification::persoonJoin);
	}

	public static Specification<MammaOnderzoek> heeftLopendeRonde()
	{
		return isLopend().withRoot(MammaOnderzoekSpecification::screeningRondeJoin);
	}

	private static Specification<MammaOnderzoek> heeftGeenBrievenBijOnderzoek()
	{
		return (r, q, cb) ->
		{
			var rondeJoin = screeningRondeJoin(r);

			var subquery = q.subquery(Long.class);
			var subRoot = subquery.from(MammaBrief.class);
			subquery.select(subRoot.get(TablePerClassHibernateObject_.id))
				.where(maakPredicateVoorBriefSubquery(q, cb, subRoot, rondeJoin));
			return cb.not(cb.exists(subquery));
		};
	}

	private static Specification<MammaOnderzoek> heeftOnderzoekNaLaatsteUitslagBrief()
	{
		return (r, q, cb) ->
		{
			var rondeJoin = screeningRondeJoin(r);

			var subquery = q.subquery(Date.class);
			var subRoot = subquery.from(MammaBrief.class);
			subquery.select(cb.greatest(subRoot.get(Brief_.creatieDatum)))
				.where(maakPredicateVoorBriefSubquery(q, cb, subRoot, rondeJoin));

			return cb.greaterThan(r.get(MammaOnderzoek_.creatieDatum), subquery);
		};
	}

	private static Predicate maakPredicateVoorBriefSubquery(CriteriaQuery<?> q, CriteriaBuilder cb, Root<MammaBrief> subRoot, Join<MammaUitnodiging, MammaScreeningRonde> rondeJoin)
	{
		return cb.and(
			cb.equal(subRoot.get(MammaBrief_.screeningRonde), rondeJoin),
			heeftGegenereerdeBriefOfProjectBriefVanType(BriefType.getMammaUitslagBriefTypen()).toPredicate(subRoot, q, cb)
		);
	}

	private static Join<Client, GbaPersoon> persoonJoin(From<?, MammaOnderzoek> r)
	{
		var rondeJoin = screeningRondeJoin(r);
		var dossierJoin = join(rondeJoin, MammaScreeningRonde_.dossier);
		var clientJoin = join(dossierJoin, MammaDossier_.client);
		return join(clientJoin, Client_.persoon);
	}

	public static Specification<MammaOnderzoek> filterOnderzoekType(MammaOnderzoekType onderzoekType)
	{
		return skipWhenNullExtended(onderzoekType, (r, q, cb) -> cb.equal(r.get(MammaOnderzoek_.onderzoekType), onderzoekType));
	}

	public static ExtendedSpecification<MammaOnderzoek> heeftBeeldenBeschikbaar()
	{
		return (r, q, cb) -> cb.equal(join(r, MammaOnderzoek_.mammografie).get(MammaMammografie_.ilmStatus), MammaMammografieIlmStatus.BESCHIKBAAR);
	}

	public static Specification<Client> heeftOnderzoekMetBeeldenGemaaktDoor(List<InstellingGebruiker> instellingGebruikers, Range<LocalDate> periode)
	{
		return (r, q, cb) ->
		{
			var subquery = q.subquery(MammaDossier.class);
			var subqueryRoot = subquery.from(MammaMammografie.class);
			var onderzoekJoin = join(subqueryRoot, MammaMammografie_.onderzoek);
			var screeningRondeJoin = screeningRondeJoin(onderzoekJoin);
			var dossierJoin = join(screeningRondeJoin, MammaScreeningRonde_.dossier);

			subquery.select(dossierJoin).where(
				cb.and(
					subqueryRoot.get(MammaMammografie_.afgerondDoor).in(instellingGebruikers),
					cb.equal(subqueryRoot.get(MammaMammografie_.ilmStatus), MammaMammografieIlmStatus.BESCHIKBAAR),
					valtInPeriode(periode).toPredicate(onderzoekJoin, q, cb)
				)
			);
			return cb.in(r.get(Client_.mammaDossier)).value(subquery);
		};
	}

	private static ExtendedSpecification<MammaOnderzoek> valtInPeriode(Range<LocalDate> periode)
	{
		return bevatLocalDateToDate(periode, o -> o.get(MammaOnderzoek_.creatieDatum));
	}

	public static Specification<MammaOnderzoek> isVanLaatsteAfspraakVanRonde()
	{
		return (r, q, cb) ->
		{
			var uitnodigingJoin = uitnodigingJoin(r);
			var screeningRondeJoin = join(uitnodigingJoin, MammaUitnodiging_.screeningRonde);
			return cb.and(
				cb.equal(screeningRondeJoin.get(MammaScreeningRonde_.laatsteUitnodiging), uitnodigingJoin),
				cb.equal(uitnodigingJoin.get(MammaUitnodiging_.laatsteAfspraak), r.get(MammaOnderzoek_.afspraak).get(AbstractHibernateObject_.id)));
		};
	}

	public static Join<MammaUitnodiging, MammaScreeningRonde> screeningRondeJoin(From<?, ? extends MammaOnderzoek> root)
	{
		return join(uitnodigingJoin(root), MammaUitnodiging_.screeningRonde);
	}

	private static Join<MammaAfspraak, MammaUitnodiging> uitnodigingJoin(From<?, ? extends MammaOnderzoek> root)
	{
		var afspraakJoin = join(root, MammaOnderzoek_.afspraak);
		return join(afspraakJoin, MammaAfspraak_.uitnodiging);
	}
}
