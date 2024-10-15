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
import java.util.Collection;
import java.util.Date;
import java.util.List;

import javax.persistence.criteria.From;
import javax.persistence.criteria.Join;
import javax.persistence.criteria.JoinType;
import javax.persistence.criteria.Root;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;

import nl.rivm.screenit.model.Instelling_;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaAfspraak_;
import nl.rivm.screenit.model.mamma.MammaBlokkade;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaMammografie_;
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
import nl.rivm.screenit.model.mamma.enums.MammaMammografieIlmStatus;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject_;

import org.springframework.data.jpa.domain.Specification;

import com.google.common.collect.Range;

import static nl.rivm.screenit.specification.DateSpecification.bevatLocalDateTime;
import static nl.rivm.screenit.specification.DateSpecification.bevatLocalDateToDate;
import static nl.rivm.screenit.specification.RangeSpecification.bevat;
import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.SpecificationUtil.skipWhenEmpty;
import static nl.rivm.screenit.util.RangeUtil.closedOpen;

@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class MammaAfspraakSpecification
{
	public static Specification<MammaAfspraak> isLaatsteAfspraakVanUitnodiging()
	{
		return (r, q, cb) ->
		{
			var uitnodigingJoin = join(r, MammaAfspraak_.uitnodiging);
			return cb.equal(uitnodigingJoin.get(MammaUitnodiging_.laatsteAfspraak), r);
		};
	}

	public static Specification<MammaAfspraak> valtOnderBlokkadeType(MammaBlokkade blokkade)
	{
		return (r, q, cb) ->
		{
			var standplaatsPeriodeJoin = standplaatsPeriodeJoin(r);
			Join<MammaStandplaatsPeriode, MammaScreeningsEenheid> screeningsEenheidJoin;
			switch (blokkade.getType())
			{
			case STANDPLAATS:
				var standplaatsRondeJoin = join(standplaatsPeriodeJoin, MammaStandplaatsPeriode_.standplaatsRonde);
				var standplaatsJoin = join(standplaatsRondeJoin, MammaStandplaatsRonde_.standplaats);
				return cb.equal(standplaatsJoin, blokkade.getStandplaats());
			case SCREENINGS_ORGANISATIE:
				screeningsEenheidJoin = join(standplaatsPeriodeJoin, MammaStandplaatsPeriode_.screeningsEenheid);
				var beoordelingsEenheidJoin = join(screeningsEenheidJoin, MammaScreeningsEenheid_.beoordelingsEenheid);
				var centraleEenheidJoin = join(beoordelingsEenheidJoin, Instelling_.parent);
				var regioJoin = join(centraleEenheidJoin, Instelling_.regio);
				return cb.equal(regioJoin, blokkade.getRegio());
			case SCREENINGS_EENHEID:
				screeningsEenheidJoin = join(standplaatsPeriodeJoin, MammaStandplaatsPeriode_.screeningsEenheid);
				return cb.equal(screeningsEenheidJoin, blokkade.getScreeningsEenheid());
			default:
				return null;
			}
		};
	}

	public static Specification<MammaAfspraak> filterStatuses(List<MammaAfspraakStatus> statuses)
	{
		return skipWhenEmpty(statuses, heeftStatuses(statuses));
	}

	public static Specification<MammaAfspraak> heeftStatuses(Collection<MammaAfspraakStatus> statuses)
	{
		return (r, q, cb) -> r.get(MammaAfspraak_.status).in(statuses);
	}

	public static Specification<MammaAfspraak> valtInPeriode(LocalDateTime vanaf, LocalDateTime tot)
	{
		var range = closedOpen(vanaf, tot);
		return bevatLocalDateTime(range, r -> r.get(MammaAfspraak_.vanaf));
	}

	public static Specification<MammaAfspraak> valtInPeriode(Range<LocalDate> periode)
	{
		return bevatLocalDateToDate(periode, r -> r.get(MammaAfspraak_.vanaf));
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
			var afgelopen2Maanden = Range.open(vandaag.minusMonths(2), vandaag);
			return cb.and(
				cb.or(
					r.get(MammaAfspraak_.status).in(MammaAfspraakStatus.INGESCHREVEN, MammaAfspraakStatus.ONDERZOEK, MammaAfspraakStatus.SIGNALEREN),
					cb.equal(onderzoekJoin.get(MammaOnderzoek_.isDoorgevoerd), false)),
				cb.equal(screeningsEenheidJoin.get(MammaScreeningsEenheid_.code), seCode),
				valtInPeriode(afgelopen2Maanden).toPredicate(r, q, cb));
		};
	}

	public static Specification<MammaAfspraak> heeftStandplaatsPeriodeDieAflooptOpOfNa(Date datum)
	{
		return (r, q, cb) ->
		{
			var standplaatsPeriodeJoin = join(r, MammaAfspraak_.standplaatsPeriode);
			return cb.greaterThanOrEqualTo(standplaatsPeriodeJoin.get(MammaStandplaatsPeriode_.totEnMet), datum);
		};
	}

	public static Specification<MammaAfspraak> heeftStandplaatsRonde(MammaStandplaats standplaats)
	{
		return (r, q, cb) ->
		{
			var standplaatsPeriodeJoin = join(r, MammaAfspraak_.standplaatsPeriode);
			var standplaatsRondeJoin = join(standplaatsPeriodeJoin, MammaStandplaatsPeriode_.standplaatsRonde);
			return cb.equal(standplaatsRondeJoin.get(MammaStandplaatsRonde_.standplaats), standplaats);
		};
	}

	public static Specification<MammaAfspraak> heeftAfgerondeMammografie()
	{
		return (r, q, cb) ->
		{
			var onderzoekJoin = join(r, MammaAfspraak_.onderzoek);
			var mammografieJoin = join(onderzoekJoin, MammaOnderzoek_.mammografie);
			return cb.isNotNull(mammografieJoin.get(MammaMammografie_.afgerondDoor));
		};
	}

	public static Specification<MammaAfspraak> heeftIlmStatus(MammaMammografieIlmStatus ilmStatus)
	{
		return (r, q, cb) ->
		{
			var onderzoekJoin = join(r, MammaAfspraak_.onderzoek);
			var mammografieJoin = join(onderzoekJoin, MammaOnderzoek_.mammografie);
			return cb.equal(mammografieJoin.get(MammaMammografie_.ilmStatus), ilmStatus);
		};
	}

	public static Specification<MammaAfspraak> heeftOnderzoekDoorgevoerd()
	{
		return (r, q, cb) ->
		{
			var onderzoekJoin = join(r, MammaAfspraak_.onderzoek);
			return cb.isTrue(onderzoekJoin.get(MammaOnderzoek_.isDoorgevoerd));
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
