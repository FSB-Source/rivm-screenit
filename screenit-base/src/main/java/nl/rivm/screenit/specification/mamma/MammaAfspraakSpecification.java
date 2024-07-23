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
import java.util.List;

import javax.persistence.criteria.From;
import javax.persistence.criteria.Join;
import javax.persistence.criteria.JoinType;
import javax.persistence.criteria.Root;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;

import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaAfspraak_;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaOnderzoek_;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde_;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid_;
import nl.rivm.screenit.model.mamma.MammaStandplaats;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode_;
import nl.rivm.screenit.model.mamma.MammaStandplaatsRonde;
import nl.rivm.screenit.model.mamma.MammaStandplaatsRonde_;
import nl.rivm.screenit.model.mamma.MammaUitnodiging_;
import nl.rivm.screenit.model.mamma.enums.MammaAfspraakStatus;
import nl.rivm.screenit.model.mamma.enums.MammaDoelgroep;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject_;

import org.springframework.data.jpa.domain.Specification;

import com.google.common.collect.Range;

import static nl.rivm.screenit.specification.DateSpecification.tussenLocalDateTime;
import static nl.rivm.screenit.specification.RangeSpecification.tussen;
import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.SpecificationUtil.skipWhenEmpty;
import static nl.rivm.screenit.util.RangeUtil.closedOpen;

@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class MammaAfspraakSpecification
{
	public static Specification<MammaAfspraak> filterStatuses(List<MammaAfspraakStatus> statuses)
	{
		return skipWhenEmpty(statuses, heeftStatuses(statuses));
	}

	public static Specification<MammaAfspraak> heeftStatuses(List<MammaAfspraakStatus> statuses)
	{
		return (r, q, cb) -> r.get(MammaAfspraak_.status).in(statuses);
	}

	public static Specification<MammaAfspraak> begintTussen(LocalDateTime vanaf, LocalDateTime tot)
	{
		var range = closedOpen(vanaf, tot);
		return tussenLocalDateTime(range, r -> r.get(MammaAfspraak_.vanaf));
	}

	public static Specification<MammaAfspraak> heeftGeenCapaciteitBlok()
	{
		return (r, q, cb) -> cb.isNull(r.get(MammaAfspraak_.capaciteitBlok));
	}

	public static Specification<MammaAfspraak> heeftClientInTehuis()
	{
		return MammaDossierSpecification.woontInTehuisPredicate().toSpecification(MammaAfspraakSpecification::dossierJoin);
	}

	public static Specification<MammaAfspraak> heeftGeenClientInTehuis()
	{
		return MammaDossierSpecification.woontNietInTehuisPredicate().toSpecification(MammaAfspraakSpecification::dossierJoin);
	}

	public static Specification<MammaAfspraak> heeftDoelgroep(List<MammaDoelgroep> doelgroepen)
	{
		return MammaDossierSpecification.heeftDoelgroep(doelgroepen).toSpecification(MammaAfspraakSpecification::dossierJoin);
	}

	public static Specification<MammaAfspraak> heeftScreeningsEenheid(MammaScreeningsEenheid screeningsEenheid)
	{
		return (r, q, cb) -> cb.equal(standplaatsPeriodeJoin(r).get(MammaStandplaatsPeriode_.screeningsEenheid), screeningsEenheid);
	}

	public static Specification<MammaAfspraak> heeftScreeningsEenheid(String seCode)
	{
		return (r, q, cb) -> cb.equal(join(standplaatsPeriodeJoin(r), MammaStandplaatsPeriode_.screeningsEenheid).get(MammaScreeningsEenheid_.code), seCode);
	}

	public static Specification<MammaAfspraak> heeftStandplaats(MammaStandplaats standplaats)
	{
		return (r, q, cb) -> cb.equal(standplaatsRondeJoin(r).get(MammaStandplaatsRonde_.standplaats), standplaats);
	}

	private static Join<MammaStandplaatsPeriode, MammaStandplaatsRonde> standplaatsRondeJoin(Root<MammaAfspraak> r)
	{
		return join(standplaatsPeriodeJoin(r), MammaStandplaatsPeriode_.standplaatsRonde);
	}

	public static Specification<MammaAfspraak> heeftStandplaatsPeriode(long standplaatsPeriodeId)
	{
		return (r, q, cb) -> cb.equal(standplaatsPeriodeJoin(r).get(AbstractHibernateObject_.id), standplaatsPeriodeId);
	}

	public static Specification<MammaAfspraak> afsprakenWaarvanOnderzoekNietIsDoorgevoerdAfgelopen2Maanden(LocalDate vandaag, String seCode)
	{
		return (r, q, cb) ->
		{
			var standplaatsPeriodeJoin = join(r, MammaAfspraak_.standplaatsPeriode);
			var screeningsEenheidJoin = join(standplaatsPeriodeJoin, MammaStandplaatsPeriode_.screeningsEenheid);
			var onderzoekJoin = join(r, MammaAfspraak_.onderzoek, JoinType.LEFT);
			var range = Range.open(DateUtil.toUtilDate(vandaag.minusMonths(2)), DateUtil.toUtilDate(vandaag));
			return cb.and(
				cb.or(
					r.get(MammaAfspraak_.status).in(MammaAfspraakStatus.INGESCHREVEN, MammaAfspraakStatus.ONDERZOEK, MammaAfspraakStatus.SIGNALEREN),
					cb.equal(onderzoekJoin.get(MammaOnderzoek_.isDoorgevoerd), false)),
				cb.equal(screeningsEenheidJoin.get(MammaScreeningsEenheid_.code), seCode),
				tussen(range, r.get(MammaAfspraak_.vanaf)).withPath(cb, r));
		};
	}

	private static Join<MammaAfspraak, MammaStandplaatsPeriode> standplaatsPeriodeJoin(Root<MammaAfspraak> r)
	{
		return join(r, MammaAfspraak_.standplaatsPeriode);
	}

	private static From<MammaScreeningRonde, MammaDossier> dossierJoin(Root<MammaAfspraak> afspraakRoot)
	{
		var uitnodigingJoin = join(afspraakRoot, MammaAfspraak_.uitnodiging);
		var screeningRondeJoin = join(uitnodigingJoin, MammaUitnodiging_.screeningRonde);
		return join(screeningRondeJoin, MammaScreeningRonde_.dossier);
	}

}
