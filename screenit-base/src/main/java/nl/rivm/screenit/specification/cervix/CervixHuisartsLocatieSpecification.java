package nl.rivm.screenit.specification.cervix;

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

import lombok.AccessLevel;
import lombok.AllArgsConstructor;

import nl.rivm.screenit.huisartsenportaal.dto.LocatieDto;
import nl.rivm.screenit.huisartsenportaal.enums.CervixLocatieStatus;
import nl.rivm.screenit.model.Gebruiker_;
import nl.rivm.screenit.model.Gemeente;
import nl.rivm.screenit.model.InstellingGebruiker_;
import nl.rivm.screenit.model.Instelling_;
import nl.rivm.screenit.model.Woonplaats_;
import nl.rivm.screenit.model.cervix.CervixHuisarts;
import nl.rivm.screenit.model.cervix.CervixHuisartsAdres_;
import nl.rivm.screenit.model.cervix.CervixHuisartsLocatie;
import nl.rivm.screenit.model.cervix.CervixHuisartsLocatie_;
import nl.rivm.screenit.model.cervix.enums.CervixHuisartsLocatieMutatieSoort;
import nl.rivm.screenit.specification.ExtendedSpecification;
import nl.rivm.screenit.specification.SpecificationUtil;
import nl.rivm.screenit.util.RangeUtil;

import org.apache.commons.lang.StringUtils;
import org.springframework.data.jpa.domain.Specification;

import com.google.common.collect.BoundType;

import static nl.rivm.screenit.specification.DateSpecification.bevatLocalDateToDate;
import static nl.rivm.screenit.specification.SpecificationUtil.containsCaseInsensitive;
import static nl.rivm.screenit.specification.SpecificationUtil.exactCaseInsensitive;

@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class CervixHuisartsLocatieSpecification
{
	public static ExtendedSpecification<CervixHuisartsLocatie> isVolledig()
	{
		return (r, q, cb) -> cb.and(
			cb.notEqual(r.get(CervixHuisartsLocatie_.naam), ""),
			cb.notEqual(r.get(CervixHuisartsLocatie_.iban), ""),
			cb.notEqual(r.get(CervixHuisartsLocatie_.ibanTenaamstelling), ""),
			cb.notEqual(r.get(CervixHuisartsLocatie_.zorgmailklantnummer), ""),
			cb.notEqual(r.get(CervixHuisartsLocatie_.naam), LocatieDto.EMPTY_VALUE),
			cb.notEqual(r.get(CervixHuisartsLocatie_.iban), LocatieDto.EMPTY_VALUE),
			cb.notEqual(r.get(CervixHuisartsLocatie_.ibanTenaamstelling), LocatieDto.EMPTY_VALUE)
		);
	}

	public static Specification<CervixHuisartsLocatie> filterHuisartsMetAgbCodeContaining(String agbCode)
	{
		return CervixHuisartsSpecification.filterOpAgbCodeContainingPredicate(agbCode).toSpecification(CervixHuisartsLocatie_.huisarts);
	}

	public static Specification<CervixHuisartsLocatie> heeftGeregistreerdeHuisarts()
	{
		return CervixHuisartsSpecification.isGeregistreerd().toSpecification(CervixHuisartsLocatie_.huisarts);
	}

	public static Specification<CervixHuisartsLocatie> heeftActieveHuisarts()
	{
		return CervixHuisartsSpecification.isActiefPredicate().toSpecification(CervixHuisartsLocatie_.huisarts);
	}

	public static Specification<CervixHuisartsLocatie> isActief()
	{
		return (r, q, cb) -> cb.equal(r.get(CervixHuisartsLocatie_.status), CervixLocatieStatus.ACTIEF);
	}

	public static Specification<CervixHuisartsLocatie> isNietInactief()
	{
		return (r, q, cb) -> cb.notEqual(r.get(CervixHuisartsLocatie_.status), CervixLocatieStatus.INACTIEF);
	}

	public static Specification<CervixHuisartsLocatie> filterMutatieSoortIn(List<CervixHuisartsLocatieMutatieSoort> mutatieSoorten)
	{
		return SpecificationUtil.skipWhenEmpty(mutatieSoorten, (r, q, cb) -> r.get(CervixHuisartsLocatie_.mutatieSoort).in(mutatieSoorten));
	}

	public static Specification<CervixHuisartsLocatie> valtBinnenMutatieDatum(LocalDate vanaf, LocalDate totEnMet)
	{
		var range = RangeUtil.range(vanaf, BoundType.CLOSED, totEnMet, BoundType.CLOSED);
		return bevatLocalDateToDate(range, r -> r.get(CervixHuisartsLocatie_.mutatiedatum));
	}

	public static Specification<CervixHuisartsLocatie> valtBinnenGemeentes(List<Gemeente> gemeentes)
	{
		return (r, q, cb) -> gemeentes.isEmpty() ?
			cb.disjunction() :
			SpecificationUtil.join(r, CervixHuisartsLocatie_.locatieAdres).join(CervixHuisartsAdres_.woonplaats).join(Woonplaats_.gemeente)
				.in(gemeentes);
	}

	public static Specification<CervixHuisartsLocatie> filterOpAchternaamMedewerkerContaining(String achternaam)
	{
		return SpecificationUtil.skipWhenEmpty(achternaam, (r, q, cb) ->
		{
			var organisatieMedewerkerListJoin = r
				.join(CervixHuisartsLocatie_.huisarts)
				.join(Instelling_.organisatieMedewerkers)
				.join(InstellingGebruiker_.medewerker);

			return containsCaseInsensitive(cb, organisatieMedewerkerListJoin.get(Gebruiker_.achternaam), achternaam);
		});
	}

	public static Specification<CervixHuisartsLocatie> filterOpLocatieNaamContaining(String locatieNaam)
	{
		return SpecificationUtil.skipWhenEmpty(locatieNaam, (r, q, cb) -> containsCaseInsensitive(cb, r.get(CervixHuisartsLocatie_.naam), locatieNaam));
	}

	public static Specification<CervixHuisartsLocatie> filterOpPostcodeContaining(String postcode)
	{
		return SpecificationUtil.skipWhenEmpty(postcode, (r, q, cb) ->
			containsCaseInsensitive(cb, SpecificationUtil.join(r, CervixHuisartsLocatie_.locatieAdres).get(CervixHuisartsAdres_.postcode), StringUtils.deleteWhitespace(postcode))
		);
	}

	public static Specification<CervixHuisartsLocatie> filterOpPlaatsContaining(String plaats)
	{
		return SpecificationUtil.skipWhenEmpty(plaats,
			(r, q, cb) -> containsCaseInsensitive(cb, SpecificationUtil.join(r, CervixHuisartsLocatie_.locatieAdres).join(CervixHuisartsAdres_.woonplaats).get(Woonplaats_.naam),
				plaats));

	}

	public static Specification<CervixHuisartsLocatie> filterOpStraat(String straat)
	{
		return SpecificationUtil.skipWhenEmpty(straat,
			(r, q, cb) -> containsCaseInsensitive(cb, SpecificationUtil.join(r, CervixHuisartsLocatie_.locatieAdres).get(CervixHuisartsAdres_.straat), straat));
	}

	public static ExtendedSpecification<CervixHuisartsLocatie> heeftHuisarts(CervixHuisarts huisarts)
	{
		return (r, q, cb) -> cb.equal(r.get(CervixHuisartsLocatie_.huisarts), huisarts);
	}

	public static ExtendedSpecification<CervixHuisartsLocatie> heeftStatus(CervixLocatieStatus status)
	{
		return (r, q, cb) -> cb.equal(r.get(CervixHuisartsLocatie_.status), status);
	}

	public static ExtendedSpecification<CervixHuisartsLocatie> heeftNietStatus(CervixLocatieStatus status)
	{
		return (r, q, cb) -> cb.notEqual(r.get(CervixHuisartsLocatie_.status), status);
	}

	public static ExtendedSpecification<CervixHuisartsLocatie> heeftNaam(String naam)
	{
		return (r, q, cb) -> exactCaseInsensitive(cb, r.get(CervixHuisartsLocatie_.naam), naam);
	}
}
