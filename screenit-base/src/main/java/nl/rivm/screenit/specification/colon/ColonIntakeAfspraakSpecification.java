package nl.rivm.screenit.specification.colon;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

import javax.persistence.criteria.Predicate;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;

import nl.rivm.screenit.model.berichten.enums.VerslagStatus;
import nl.rivm.screenit.model.berichten.enums.VerslagType;
import nl.rivm.screenit.model.colon.ColonConclusie_;
import nl.rivm.screenit.model.colon.ColonDossier_;
import nl.rivm.screenit.model.colon.ColonIntakeAfspraak;
import nl.rivm.screenit.model.colon.ColonIntakeAfspraak_;
import nl.rivm.screenit.model.colon.ColonScreeningRonde_;
import nl.rivm.screenit.model.colon.ColonVerslag;
import nl.rivm.screenit.model.colon.ColonVerslag_;
import nl.rivm.screenit.model.colon.ColoscopieCentrum;
import nl.rivm.screenit.model.colon.Kamer_;
import nl.rivm.screenit.model.colon.WerklijstIntakeFilter;
import nl.rivm.screenit.model.colon.enums.ColonConclusieType;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.specification.algemeen.PersoonSpecification;
import nl.topicuszorg.patientregistratie.persoonsgegevens.model.Patient_;
import nl.topicuszorg.patientregistratie.persoonsgegevens.model.Persoon_;
import nl.topicuszorg.wicket.planning.model.appointment.AbstractAppointment_;

import org.apache.commons.lang.StringUtils;
import org.springframework.data.jpa.domain.Specification;

@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class ColonIntakeAfspraakSpecification
{
	public static Specification<ColonIntakeAfspraak> zonderVerslagen()
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

	public static Specification<ColonIntakeAfspraak> clientNietOverledenVoorColoscopie()
	{
		return (r, q, cb) ->
		{
			var conclusiePath = r
				.get(ColonIntakeAfspraak_.conclusie);

			var persoonPath = r
				.get(ColonIntakeAfspraak_.colonScreeningRonde)
				.get(ColonScreeningRonde_.dossier)
				.get(ColonDossier_.client)
				.get(Patient_.persoon);

			return PersoonSpecification.nietOverledenVoor(conclusiePath.get(ColonConclusie_.datumColoscopie)).withPath(cb, persoonPath);
		};
	}

	public static Specification<ColonIntakeAfspraak> clientNietVerhuisdNaarBuitenlandVoorColoscopie()
	{
		return (r, q, cb) ->
		{
			var datumColoscopie = r
				.get(ColonIntakeAfspraak_.conclusie)
				.get(ColonConclusie_.datumColoscopie);

			var gbaPersoon = r
				.get(ColonIntakeAfspraak_.colonScreeningRonde)
				.get(ColonScreeningRonde_.dossier)
				.get(ColonDossier_.client)
				.get(Patient_.persoon);

			return PersoonSpecification.nietVetrokkenUitNederlandVoor(datumColoscopie).withPath(cb, gbaPersoon);
		};
	}

	public static Specification<ColonIntakeAfspraak> metConclusieType(ColonConclusieType conclusieType)
	{
		return (r, q, cb) ->
			cb.equal(r.get(ColonIntakeAfspraak_.conclusie).get(ColonConclusie_.type), conclusieType);
	}

	public static Specification<ColonIntakeAfspraak> metIntakelocatie(ColoscopieCentrum intakelocatie)
	{
		return (r, q, cb) ->
			cb.equal(r.get(AbstractAppointment_.location).get(Kamer_.coloscopieCentrum), intakelocatie);
	}

	public static Specification<ColonIntakeAfspraak> metConclusieInVerleden(ICurrentDateSupplier currentDateSupplier)
	{

		return (r, q, cb) ->
		{
			var datum = currentDateSupplier.getDateMidnight();
			return cb.lessThan(r.get(ColonIntakeAfspraak_.conclusie).get(ColonConclusie_.datumColoscopie), datum);
		};
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
				.get(Patient_.persoon);

			var predicates = new ArrayList<Predicate>();

			if (StringUtils.isNotBlank(zoekObject.getBsn()))
			{
				predicates.add(cb.equal(persoonPath.get(Persoon_.bsn), zoekObject.getBsn()));
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
}
