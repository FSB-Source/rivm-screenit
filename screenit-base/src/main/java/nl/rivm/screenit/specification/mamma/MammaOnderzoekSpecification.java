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
import java.util.Collection;
import java.util.Date;
import java.util.List;

import javax.persistence.criteria.From;
import javax.persistence.criteria.Join;
import javax.persistence.criteria.JoinType;
import javax.persistence.criteria.Root;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;

import nl.rivm.screenit.model.BeoordelingsEenheid;
import nl.rivm.screenit.model.Brief_;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientBrief_;
import nl.rivm.screenit.model.Client_;
import nl.rivm.screenit.model.Dossier_;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.ScreeningRondeStatus;
import nl.rivm.screenit.model.TablePerClassHibernateObject_;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.MammaOnderzoekType;
import nl.rivm.screenit.model.mamma.MammaAfspraak_;
import nl.rivm.screenit.model.mamma.MammaBeoordeling_;
import nl.rivm.screenit.model.mamma.MammaBrief;
import nl.rivm.screenit.model.mamma.MammaBrief_;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaDossier_;
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
import nl.rivm.screenit.specification.ExtendedSpecification;
import nl.rivm.screenit.specification.algemeen.ClientSpecification;
import nl.rivm.screenit.specification.algemeen.PersoonSpecification;
import nl.rivm.screenit.specification.cervix.ProjectBriefSpecification;
import nl.rivm.screenit.util.DateUtil;

import org.springframework.data.jpa.domain.Specification;

import static nl.rivm.screenit.specification.DateSpecification.truncate;
import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.SpecificationUtil.skipWhenEmpty;
import static nl.rivm.screenit.specification.SpecificationUtil.skipWhenNullExtended;
import static nl.rivm.screenit.specification.mamma.MammaBriefSpecification.heeftBriefInBrieftypesPredicate;
import static nl.rivm.screenit.specification.mamma.MammaBriefSpecification.heeftVervangendeProjectBriefPredicate;
import static nl.rivm.screenit.specification.mamma.MammaBriefSpecification.isGegenereerdPredicate;

@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class MammaOnderzoekSpecification
{
	public static Specification<Client> heeftClientLaatsteOnderzoekAangemaaktVanaf(LocalDate datum)
	{
		return (r, q, cb) -> cb.greaterThanOrEqualTo(onderzoekJoin(r).get(MammaOnderzoek_.creatieDatum), DateUtil.toUtilDate(datum));
	}

	public static Specification<Client> heeftClientLaatsteOnderzoekVolledig()
	{
		return (r, q, cb) -> cb.equal(onderzoekJoin(r).get(MammaOnderzoek_.status), MammaOnderzoekStatus.AFGEROND);
	}

	public static Specification<MammaOnderzoek> heeftBeoordelingsEenheid(BeoordelingsEenheid beoordelingsEenheid)
	{
		return (r, q, cb) -> cb.equal(join(r, MammaOnderzoek_.screeningsEenheid).get(MammaScreeningsEenheid_.beoordelingsEenheid), beoordelingsEenheid);
	}

	public static Specification<MammaOnderzoek> isDoorgevoerd(boolean doorgevoerd)
	{
		return (r, q, cb) -> cb.equal(r.get(MammaOnderzoek_.isDoorgevoerd), doorgevoerd);
	}

	public static Specification<MammaOnderzoek> heeftGeenBeoordelingStatusIn(Collection<MammaBeoordelingStatus> statussen)
	{
		return (r, q, cb) -> cb.not(join(r, MammaOnderzoek_.beoordelingen).get(MammaBeoordeling_.status).in(statussen));
	}

	private static From<MammaScreeningRonde, MammaOnderzoek> onderzoekJoin(Root<Client> clientRoot)
	{
		var dossierJoin = join(clientRoot, Client_.mammaDossier);
		var rondeJoin = join(dossierJoin, MammaDossier_.laatsteScreeningRonde);
		return join(rondeJoin, MammaScreeningRonde_.laatsteOnderzoek);
	}

	public static Specification<MammaOnderzoek> heeftMissendeUitslag(LocalDate signalerenVanaf)
	{
		return (r, q, cb) ->
		{
			var rondeJoin = rondeJoin(r);
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

	public static Specification<MammaOnderzoek> heeftOnderzoekStatus(MammaOnderzoekStatus status)
	{
		return (r, q, cb) -> cb.equal(r.get(MammaOnderzoek_.status), status);
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
		return ClientSpecification.heeftActieveClient().withRoot(r ->
		{
			var rondeJoin = rondeJoin(r);
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
			var rondeJoin = rondeJoin(r);
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

	public static Specification<MammaOnderzoek> filterScreeningsEenheid(List<MammaScreeningsEenheid> screeningsEenheden)
	{
		return skipWhenEmpty(screeningsEenheden, (r, q, cb) -> r.get(MammaOnderzoek_.screeningsEenheid).in(screeningsEenheden));
	}

	public static Specification<MammaOnderzoek> heeftScreeningsEenheid(MammaScreeningsEenheid screeningsEenheid)
	{
		return (r, q, cb) -> cb.equal(r.get(MammaOnderzoek_.screeningsEenheid), screeningsEenheid);
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
		return PersoonSpecification.isNietOverledenEnWoontInNederland().withRoot(MammaOnderzoekSpecification::getPersoonJoin);
	}

	private static Join<MammaUitnodiging, MammaScreeningRonde> rondeJoin(From<?, MammaOnderzoek> r)
	{
		var afspraakJoin = join(r, MammaOnderzoek_.afspraak);
		var uitnodigingJoin = join(afspraakJoin, MammaAfspraak_.uitnodiging);
		return join(uitnodigingJoin, MammaUitnodiging_.screeningRonde);
	}

	public static Specification<MammaOnderzoek> heeftLopendeRonde()
	{
		return MammaScreeningRondeSpecification.heeftRondeStatus(ScreeningRondeStatus.LOPEND).withRoot(MammaOnderzoekSpecification::rondeJoin);
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
						heeftVervangendeProjectBriefPredicate(false).withPath(cb, subRoot),
						heeftBriefInBrieftypesPredicate(BriefType.getMammaUitslagBriefTypen()).withPath(cb, subRoot),
						isGegenereerdPredicate(true).withPath(cb, subRoot)
					),
					cb.and(
						heeftVervangendeProjectBriefPredicate(true).withPath(cb, subRoot),
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
			subquery.select(cb.greatest(subRoot.get(Brief_.creatieDatum))).where(cb.and(
				cb.equal(subRoot.get(MammaBrief_.screeningRonde), rondeJoin),
				cb.or(
					cb.and(
						heeftVervangendeProjectBriefPredicate(false).withPath(cb, subRoot),
						heeftBriefInBrieftypesPredicate(BriefType.getMammaUitslagBriefTypen()).withPath(cb, subRoot),
						isGegenereerdPredicate(true).withPath(cb, subRoot)
					),
					cb.and(
						heeftVervangendeProjectBriefPredicate(true).withPath(cb, subRoot),
						ProjectBriefSpecification.heeftBriefInBrieftypes(BriefType.getMammaUitslagBriefTypen()).withPath(cb, projectBriefJoin),
						ProjectBriefSpecification.isGegenereerd(true).withPath(cb, projectBriefJoin)
					))));

			return cb.greaterThan(r.get(MammaOnderzoek_.creatieDatum), subquery);
		};
	}

	private static Join<Client, GbaPersoon> getPersoonJoin(From<?, MammaOnderzoek> r)
	{
		var rondeJoin = rondeJoin(r);
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
}
