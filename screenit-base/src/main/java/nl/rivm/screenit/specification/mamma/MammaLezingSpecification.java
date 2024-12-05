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
import java.time.LocalDateTime;

import javax.persistence.criteria.Path;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.enums.Termijn;
import nl.rivm.screenit.model.mamma.MammaLezing;
import nl.rivm.screenit.model.mamma.MammaLezing_;
import nl.rivm.screenit.model.mamma.enums.MammaBIRADSWaarde;
import nl.rivm.screenit.specification.DateSpecification;
import nl.rivm.screenit.specification.ExtendedSpecification;
import nl.rivm.screenit.util.DateUtil;

import com.google.common.collect.Range;

import static nl.rivm.screenit.specification.DateSpecification.bevatLocalDateToDate;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class MammaLezingSpecification
{

	public static ExtendedSpecification<MammaLezing> isAfwezigOfGedaanDoor(InstellingGebruiker radioloog)
	{
		return (r, q, cb) -> cb.or(
			r.isNull(),
			cb.equal(r.get(MammaLezing_.beoordelaar), radioloog));
	}

	public static ExtendedSpecification<MammaLezing> isGedaanDoor(InstellingGebruiker radioloog)
	{
		return (r, q, cb) -> cb.equal(r.get(MammaLezing_.beoordelaar), radioloog);
	}

	public static ExtendedSpecification<MammaLezing> isGedaanDoor(Path<InstellingGebruiker> radioloog)
	{
		return (r, q, cb) -> cb.equal(r.get(MammaLezing_.beoordelaar), radioloog);
	}

	public static ExtendedSpecification<MammaLezing> isNietGedaanDoor(InstellingGebruiker radioloog)
	{
		return (r, q, cb) -> cb.notEqual(r.get(MammaLezing_.beoordelaar), radioloog);
	}

	public static ExtendedSpecification<MammaLezing> isVerwezenDoor(InstellingGebruiker radioloog)
	{
		return isGedaanDoor(radioloog).and(isVerwezen());
	}

	public static ExtendedSpecification<MammaLezing> isVerwezen()
	{
		return (r, q, cb) -> cb.or(
			r.get(MammaLezing_.biradsLinks).in(MammaBIRADSWaarde.getVerwijzendBIRADSWaarden()),
			r.get(MammaLezing_.biradsRechts).in(MammaBIRADSWaarde.getVerwijzendBIRADSWaarden()));
	}

	public static ExtendedSpecification<MammaLezing> isGedaanInPeriode(Range<LocalDate> periode)
	{
		return bevatLocalDateToDate(periode, r -> r.get(MammaLezing_.beoordelingDatum));
	}

	public static ExtendedSpecification<MammaLezing> isGedaanBinnenTermijnDoor(InstellingGebruiker radioloog, LocalDate peildatum, Termijn termijn)
	{
		return isGedaanDoor(radioloog).and(isGedaanInPeriode(termijn.getPeriode(peildatum)));
	}

	public static ExtendedSpecification<MammaLezing> heeftBeoordelingDatumVanaf(LocalDateTime peilMoment)
	{
		return (r, q, cb) -> cb.greaterThanOrEqualTo(r.get(MammaLezing_.beoordelingDatum), DateUtil.toUtilDate(peilMoment));
	}

	public static ExtendedSpecification<MammaLezing> heeftBeoordelingDatumOpOfVoor(LocalDateTime peilMoment)
	{
		return (r, q, cb) -> cb.lessThanOrEqualTo(r.get(MammaLezing_.beoordelingDatum), DateUtil.toUtilDate(peilMoment));
	}

	public static ExtendedSpecification<MammaLezing> heeftNietBeoordeeldSindsSubquery(LocalDateTime peilMoment)
	{
		return (r, q, cb) ->
		{
			var subquery = q.subquery(MammaLezing.class);
			var subqueryRoot = subquery.from(MammaLezing.class);

			subquery.select(subqueryRoot).where(
				isGedaanDoor(r.get(MammaLezing_.beoordelaar)).and(heeftBeoordelingDatumVanaf(peilMoment))
					.toPredicate(subqueryRoot, q, cb));

			return cb.not(cb.exists(subquery));
		};
	}
}
