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
import java.util.List;

import javax.persistence.criteria.JoinType;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import nl.rivm.screenit.model.BeoordelingsEenheid;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Client_;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.enums.Termijn;
import nl.rivm.screenit.model.mamma.MammaAfspraak_;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaBeoordeling_;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaDossier_;
import nl.rivm.screenit.model.mamma.MammaLezing;
import nl.rivm.screenit.model.mamma.MammaOnderzoek_;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde_;
import nl.rivm.screenit.model.mamma.MammaUitnodiging_;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingOpschortenReden;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus;
import nl.rivm.screenit.model.mamma.enums.MammaMammografieIlmStatus;
import nl.rivm.screenit.specification.ExtendedSpecification;
import nl.rivm.screenit.util.DateUtil;

import org.springframework.data.jpa.domain.Specification;

import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.SpecificationUtil.skipWhenEmptyExtended;
import static nl.rivm.screenit.specification.SpecificationUtil.skipWhenNullExtended;
import static nl.rivm.screenit.specification.mamma.MammaLezingSpecification.heeftNietBeoordeeldSindsSubquery;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class MammaBeoordelingSpecification
{
	public static Specification<Client> heeftClientLaatsteOnderzoekBeoordelingStatus(MammaBeoordelingStatus status)
	{
		return (r, q, cb) ->
		{
			var dossierJoin = join(r, Client_.mammaDossier);
			var rondeJoin = join(dossierJoin, MammaDossier_.laatsteScreeningRonde);
			var onderzoekJoin = join(rondeJoin, MammaScreeningRonde_.laatsteOnderzoek);
			var beoordelingJoin = join(onderzoekJoin, MammaOnderzoek_.laatsteBeoordeling);
			return cb.equal(beoordelingJoin.get(MammaBeoordeling_.status), status);
		};
	}

	public static ExtendedSpecification<MammaBeoordeling> filterStatusIn(List<MammaBeoordelingStatus> statussen)
	{
		return skipWhenEmptyExtended(statussen, (r, q, cb) -> r.get(MammaBeoordeling_.status).in(statussen));
	}

	public static ExtendedSpecification<MammaBeoordeling> heeftStatus(MammaBeoordelingStatus status)
	{
		return (r, q, cb) -> cb.equal(r.get(MammaBeoordeling_.status), status);
	}

	public static ExtendedSpecification<MammaBeoordeling> heeftOpschortReden(MammaBeoordelingOpschortenReden opschortenReden)
	{
		return (r, q, cb) -> cb.equal(r.get(MammaBeoordeling_.opschortReden), opschortenReden);
	}

	public static ExtendedSpecification<MammaBeoordeling> heeftTotDiscrepantieGeleid()
	{
		return heeftDiscrepantieLezing().or(heeftStatus(MammaBeoordelingStatus.DISCREPANTIE));
	}

	public static Specification<MammaBeoordeling> isVrijTeGeven(InstellingGebruiker ingelogdeGebruiker)
	{
		return (r, q, cb) -> cb.and(
			cb.equal(r.get(MammaBeoordeling_.reserveringhouder), ingelogdeGebruiker),
			cb.not(r.get(MammaBeoordeling_.status).in(MammaBeoordelingStatus.EERSTE_LEZING_OPGESLAGEN, MammaBeoordelingStatus.TWEEDE_LEZING_OPGESLAGEN)
			));
	}

	public static Specification<MammaBeoordeling> heeftLezing(MammaLezing lezing)
	{
		return (r, q, cb) -> cb.or(
			cb.equal(r.get(MammaBeoordeling_.eersteLezing), lezing),
			cb.equal(r.get(MammaBeoordeling_.tweedeLezing), lezing),
			cb.equal(r.get(MammaBeoordeling_.discrepantieLezing), lezing),
			cb.equal(r.get(MammaBeoordeling_.arbitrageLezing), lezing),
			cb.equal(r.get(MammaBeoordeling_.verslagLezing), lezing)
		);
	}

	public static ExtendedSpecification<MammaBeoordeling> heeftUitslagStatus()
	{
		return (r, q, cb) -> r.get(MammaBeoordeling_.status).in(MammaBeoordelingStatus.uitslagStatussen());
	}

	public static Specification<MammaBeoordeling> heeftDossier(MammaDossier dossier)
	{
		return (r, q, cb) ->
		{
			var onderzoekJoin = join(r, MammaBeoordeling_.onderzoek);
			var afspraakJoin = join(onderzoekJoin, MammaOnderzoek_.afspraak);
			var uitnodigingJoin = join(afspraakJoin, MammaAfspraak_.uitnodiging);
			var screeningRondeJoin = join(uitnodigingJoin, MammaUitnodiging_.screeningRonde);
			return cb.equal(screeningRondeJoin.get(MammaScreeningRonde_.dossier), dossier);
		};
	}

	public static Specification<MammaBeoordeling> heeftMammografieIlmStatus(MammaMammografieIlmStatus ilmStatus)
	{
		return MammaMammografieBaseSpecification.heeftIlmStatus(ilmStatus).with(r ->
		{
			var onderzoekJoin = join(r, MammaBeoordeling_.onderzoek);
			return join(onderzoekJoin, MammaOnderzoek_.mammografie);
		});
	}

	public static ExtendedSpecification<MammaBeoordeling> filterBeoordelingsEenheid(BeoordelingsEenheid beoordelingsEenheid)
	{
		return skipWhenNullExtended(beoordelingsEenheid, (r, q, cb) -> cb.equal(r.get(MammaBeoordeling_.beoordelingsEenheid), beoordelingsEenheid));
	}

	public static ExtendedSpecification<MammaBeoordeling> filterBeoordelingsEenheid(List<BeoordelingsEenheid> beoordelingsEenheden)
	{
		return skipWhenNullExtended(beoordelingsEenheden, (r, q, cb) -> r.get(MammaBeoordeling_.beoordelingsEenheid).in(beoordelingsEenheden));
	}

	public static ExtendedSpecification<MammaBeoordeling> isNietToegewezenAanSpecifiekeRadioloog()
	{
		return (r, q, cb) -> r.get(MammaBeoordeling_.toegewezenGebruiker).isNull();
	}

	public static ExtendedSpecification<MammaBeoordeling> isToegewezenAan(InstellingGebruiker radioloog)
	{
		return (r, q, cb) -> cb.equal(r.get(MammaBeoordeling_.toegewezenGebruiker), radioloog);
	}

	public static ExtendedSpecification<MammaBeoordeling> heeftDiscrepantieLezing()
	{
		return (r, q, cb) -> r.get(MammaBeoordeling_.discrepantieLezing).isNotNull();
	}

	public static ExtendedSpecification<MammaBeoordeling> heeftArbitrageLezing()
	{
		return (r, q, cb) -> r.get(MammaBeoordeling_.arbitrageLezing).isNotNull();
	}

	public static ExtendedSpecification<MammaBeoordeling> heeftGeenArbitrageLezing()
	{
		return (r, q, cb) -> r.get(MammaBeoordeling_.arbitrageLezing).isNull();
	}

	public static ExtendedSpecification<MammaBeoordeling> heeftStatusDatumVoor(LocalDate voorDatum)
	{
		return (r, q, cb) -> cb.lessThan(r.get(MammaBeoordeling_.statusDatum), DateUtil.toUtilDate(voorDatum));
	}

	public static ExtendedSpecification<MammaBeoordeling> heeftStatusDatumOpOfVoor(LocalDate voorDatum)
	{
		return (r, q, cb) -> cb.lessThanOrEqualTo(r.get(MammaBeoordeling_.statusDatum), DateUtil.toUtilDate(voorDatum));
	}

	public static ExtendedSpecification<MammaBeoordeling> heeftStatusDatumOpOfVoor(LocalDateTime peilMoment)
	{
		return (r, q, cb) -> cb.lessThanOrEqualTo(r.get(MammaBeoordeling_.statusDatum), DateUtil.toUtilDate(peilMoment));
	}

	public static ExtendedSpecification<MammaBeoordeling> heeftStatusDatumVanaf(LocalDate vanafDatum)
	{
		return (r, q, cb) -> cb.greaterThanOrEqualTo(r.get(MammaBeoordeling_.statusDatum), DateUtil.toUtilDate(vanafDatum));
	}

	public static Specification<MammaBeoordeling> heeftEersteOfTweedeLezingGedaanBinnenTermijn(InstellingGebruiker radioloog, LocalDate peilDatum, Termijn termijn)
	{
		return (r, q, cb) ->
		{
			var subquery = q.subquery(MammaLezing.class);
			var subqueryRoot = subquery.from(MammaLezing.class);
			subquery.select(subqueryRoot).where(MammaLezingSpecification.isGedaanBinnenTermijnDoor(radioloog, peilDatum, termijn).toPredicate(subqueryRoot, q, cb));
			return cb.or(
				r.get(MammaBeoordeling_.eersteLezing).in(subquery),
				r.get(MammaBeoordeling_.tweedeLezing).in(subquery));
		};
	}

	public static Specification<MammaBeoordeling> beoordelaarsZijnInactiefSinds(LocalDateTime peilMoment)
	{
		return heeftNietBeoordeeldSindsSubquery(peilMoment).with(MammaBeoordeling_.eersteLezing, JoinType.LEFT)
			.and(heeftNietBeoordeeldSindsSubquery(peilMoment).with(MammaBeoordeling_.tweedeLezing, JoinType.LEFT));
	}
}
