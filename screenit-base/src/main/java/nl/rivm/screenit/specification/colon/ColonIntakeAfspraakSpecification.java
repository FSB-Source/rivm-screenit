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

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import javax.persistence.criteria.Predicate;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Client_;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.GbaPersoon_;
import nl.rivm.screenit.model.berichten.enums.VerslagStatus;
import nl.rivm.screenit.model.berichten.enums.VerslagType;
import nl.rivm.screenit.model.colon.ColonConclusie_;
import nl.rivm.screenit.model.colon.ColonDossier_;
import nl.rivm.screenit.model.colon.ColonIntakeAfspraak;
import nl.rivm.screenit.model.colon.ColonIntakeAfspraak_;
import nl.rivm.screenit.model.colon.ColonIntakelocatie;
import nl.rivm.screenit.model.colon.ColonScreeningRonde_;
import nl.rivm.screenit.model.colon.ColonVerslag;
import nl.rivm.screenit.model.colon.ColonVerslag_;
import nl.rivm.screenit.model.colon.WerklijstIntakeFilter;
import nl.rivm.screenit.model.colon.enums.ColonAfspraakStatus;
import nl.rivm.screenit.model.colon.enums.ColonConclusieType;
import nl.rivm.screenit.model.colon.planning.ColonIntakekamer;
import nl.rivm.screenit.model.colon.planning.ColonIntakekamer_;
import nl.rivm.screenit.model.colon.planning.ColonTijdslot_;
import nl.rivm.screenit.specification.ExtendedSpecification;
import nl.rivm.screenit.specification.SpecificationUtil;

import org.apache.commons.lang.StringUtils;
import org.springframework.data.jpa.domain.Specification;

import static nl.rivm.screenit.specification.SpecificationUtil.join;

@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class ColonIntakeAfspraakSpecification
{
	public static Specification<ColonIntakeAfspraak> heeftGeenVerslagen()
	{
		return (r, q, cb) ->
		{
			var colonScreeningRondePath = r
				.get(ColonIntakeAfspraak_.colonScreeningRonde);

			var subquery = q.subquery(ColonVerslag.class);
			var subqueryRoot = subquery.from(ColonVerslag.class);

			subquery.select(subqueryRoot).where(
				cb.and(
					cb.equal(subqueryRoot.get(ColonVerslag_.type), VerslagType.MDL),
					cb.equal(subqueryRoot.get(ColonVerslag_.screeningRonde), colonScreeningRondePath),
					cb.equal(subqueryRoot.get(ColonVerslag_.status), VerslagStatus.AFGEROND)
				)
			);
			return cb.not(cb.exists(subquery));
		};
	}

	public static Specification<ColonIntakeAfspraak> heeftClientNietOverledenOfVerhuisdVoorColoscopie()
	{
		return (r, q, cb) ->
		{
			var subquery = q.subquery(Client.class);
			var subqueryRoot = subquery.from(Client.class);

			var gbaPersoonAlias = cb.treat(subqueryRoot.join(Client_.persoon), GbaPersoon.class);
			var datumColoscopiePath = r.join(ColonIntakeAfspraak_.conclusie).get(ColonConclusie_.datumColoscopie);
			var dossierPath = r.join(ColonIntakeAfspraak_.colonScreeningRonde).get(ColonScreeningRonde_.dossier);

			return cb.exists(subquery.select(subqueryRoot).where(
				cb.equal(dossierPath, subqueryRoot.get(Client_.colonDossier)),
				cb.and(
					cb.or(
						cb.isNull(gbaPersoonAlias.get(GbaPersoon_.overlijdensdatum)),
						cb.greaterThan(gbaPersoonAlias.get(GbaPersoon_.overlijdensdatum), datumColoscopiePath)
					),
					cb.or(
						cb.isNull(gbaPersoonAlias.get(GbaPersoon_.datumVertrokkenUitNederland)),
						cb.greaterThan(gbaPersoonAlias.get(GbaPersoon_.datumVertrokkenUitNederland),
							datumColoscopiePath)
					)
				))
			);

		};
	}

	public static Specification<ColonIntakeAfspraak> heeftConclusieType(ColonConclusieType conclusieType)
	{
		return (r, q, cb) ->
			cb.equal(r.get(ColonIntakeAfspraak_.conclusie).get(ColonConclusie_.type), conclusieType);
	}

	public static Specification<ColonIntakeAfspraak> heeftIntakelocatie(ColonIntakelocatie intakelocatie)
	{
		return (r, q, cb) ->
			cb.equal(SpecificationUtil.join(r, ColonTijdslot_.kamer).get(ColonIntakekamer_.intakelocatie), intakelocatie);
	}

	public static Specification<ColonIntakeAfspraak> heeftConclusieInVerleden(Date datum)
	{
		return (r, q, cb) -> cb.lessThan(SpecificationUtil.join(r, ColonIntakeAfspraak_.conclusie).get(ColonConclusie_.datumColoscopie), datum);
	}

	public static Specification<ColonIntakeAfspraak> metFilter(WerklijstIntakeFilter zoekObject)
	{
		return (r, q, cb) ->
		{
			var colonScreeningRondePath = r
				.get(ColonIntakeAfspraak_.colonScreeningRonde);

			var conclusiePath = r
				.get(ColonIntakeAfspraak_.conclusie);

			var persoonPath = colonScreeningRondePath
				.get(ColonScreeningRonde_.dossier)
				.get(ColonDossier_.client)
				.get(Client_.persoon);

			var predicates = new ArrayList<Predicate>();

			if (StringUtils.isNotBlank(zoekObject.getBsn()))
			{
				predicates.add(cb.equal(persoonPath.get(GbaPersoon_.bsn), zoekObject.getBsn()));
			}

			if (zoekObject.getVanaf() != null)
			{
				predicates.add(cb.greaterThanOrEqualTo(conclusiePath.get(ColonConclusie_.datumColoscopie), zoekObject.getVanaf()));
			}

			if (zoekObject.getTotEnMet() != null)
			{
				predicates.add(cb.lessThanOrEqualTo(conclusiePath.get(ColonConclusie_.datumColoscopie), zoekObject.getTotEnMet()));
			}

			return cb.and(predicates.toArray(Predicate[]::new));
		};
	}

	public static Specification<ColonIntakeAfspraak> heeftKamer(ColonIntakekamer kamer)
	{
		return (r, q, cb) -> cb.equal(r.get(ColonTijdslot_.kamer), kamer);
	}

	public static Specification<ColonIntakeAfspraak> heeftStatuses(List<ColonAfspraakStatus> statuses)
	{
		return (r, q, cb) -> r.get(ColonIntakeAfspraak_.status).in(statuses);
	}

	public static Specification<ColonIntakeAfspraak> heeftAfspraakslot(Long afspraakslotId)
	{
		return (r, q, cb) ->
		{
			var afspraakslot = join(r, ColonIntakeAfspraak_.afspraakslot);
			return cb.or(cb.equal(afspraakslot.get(ColonTijdslot_.id), afspraakslotId));
		};
	}

	public static ExtendedSpecification<ColonIntakeAfspraak> heeftLaatsteAfspraakMetBezwaar()
	{
		return (r, q, cb) -> cb.isTrue(r.get(ColonIntakeAfspraak_.bezwaar));
	}
}
