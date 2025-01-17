package nl.rivm.screenit.batch.jobs.mamma.brieven.client.genererenstep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bk
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

import javax.persistence.criteria.From;
import javax.persistence.criteria.Join;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.batch.jobs.brieven.genereren.AbstractBrievenGenererenReader;
import nl.rivm.screenit.model.Brief_;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientBrief_;
import nl.rivm.screenit.model.Client_;
import nl.rivm.screenit.model.TijdelijkAdres_;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaAfspraak_;
import nl.rivm.screenit.model.mamma.MammaBrief;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaStandplaats;
import nl.rivm.screenit.model.mamma.MammaStandplaatsLocatie;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode_;
import nl.rivm.screenit.model.mamma.MammaStandplaatsRonde;
import nl.rivm.screenit.model.mamma.MammaStandplaatsRonde_;
import nl.rivm.screenit.model.mamma.MammaStandplaats_;
import nl.rivm.screenit.model.mamma.MammaUitnodiging;
import nl.rivm.screenit.model.mamma.MammaUitnodiging_;
import nl.rivm.screenit.specification.ExtendedSpecification;

import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import static javax.persistence.criteria.JoinType.LEFT;
import static nl.rivm.screenit.specification.ExtendedSpecification.not;
import static nl.rivm.screenit.specification.HibernateObjectSpecification.heeftId;
import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.algemeen.BriefSpecification.heeftBriefType;
import static nl.rivm.screenit.specification.algemeen.ClientSpecification.heeftIndicatie;
import static nl.rivm.screenit.specification.algemeen.MammaBriefSpecification.clientHeeftAfspraak;
import static nl.rivm.screenit.specification.algemeen.MammaBriefSpecification.laatsteUitnodigingJoin;
import static nl.rivm.screenit.specification.mamma.MammaStandplaatsLocatieSpecification.heeftBijlageOfApartPrinten;
import static nl.rivm.screenit.specification.mamma.MammaStandplaatsLocatieSpecification.heeftGeenBijlageOfApartPrinten;

@Component
@Slf4j
public class MammaBrievenGenererenReader extends AbstractBrievenGenererenReader<MammaBrief>
{

	@Override
	protected Long getScreeningOrganisatieId()
	{
		return getStepExecutionContext().getLong(MammaBrievenGenererenPartitioner.KEY_SCREENINGORGANISATIEID);
	}

	@Override
	protected Specification<MammaBrief> createSpecification()
	{
		ExtendedSpecification<MammaBrief> specification = heeftBriefType(briefType());

		var isVoorTijdelijkeLocaties = isVoorTijdelijkeLocaties();
		var standplaatsId = getStandplaatsId();

		if (isAparteBrieven())
		{
			if (isVoorTijdelijkeLocaties && standplaatsId != null)
			{
				specification = specification
					.and(clientHeeftAfspraak())
					.and(afspraakValtBinnenTijdelijkeLocatie())
					.and(heeftAfspraakStandplaatsId(standplaatsId))
					.and(heeftBijlageOfApartPrinten().with(r -> tijdelijkeLocatieJoin(laatsteAfspraakJoin(r))));
			}
			else if (!isVoorTijdelijkeLocaties && standplaatsId != null)
			{
				var normaleLocatieMetAfspraak = clientHeeftAfspraak()
					.and(not(afspraakValtBinnenTijdelijkeLocatie()))
					.and(heeftAfspraakStandplaatsId(standplaatsId))
					.and(heeftBijlageOfApartPrinten().with(r -> normaleLocatieJoin(laatsteAfspraakJoin(r))));

				var normaleLocatieZonderAfspraak = not(clientHeeftAfspraak())
					.and(heeftUitnodigingStandplaatsId(standplaatsId))
					.and(heeftBijlageOfApartPrinten().with(r -> uitnodigingslocatieJoin(r)));

				specification = specification.and(normaleLocatieMetAfspraak.or(normaleLocatieZonderAfspraak));
			}
			else if (isVoorTijdelijkeLocaties)
			{
				specification = specification
					.and(clientHeeftAfspraak())
					.and(afspraakValtBinnenTijdelijkeLocatie())
					.and(heeftGeenBijlageOfApartPrinten().with(r -> tijdelijkeLocatieJoin(laatsteAfspraakJoin(r))));
			}
			else
			{
				var normaleLocatieMetAfspraak = clientHeeftAfspraak()
					.and(not(afspraakValtBinnenTijdelijkeLocatie()))
					.and(heeftGeenBijlageOfApartPrinten().with(r -> normaleLocatieJoin(laatsteAfspraakJoin(r))));

				var normaleLocatieZonderAfspraak = not(clientHeeftAfspraak())
					.and(heeftGeenBijlageOfApartPrinten().with(r -> uitnodigingslocatieJoin(r)));

				specification = specification.and(normaleLocatieMetAfspraak.or(normaleLocatieZonderAfspraak));

			}
			specification = specification.and(eersteOfVervolgRondeSpecification());
		}

		return specification
			.and(heeftIndicatie().with(r -> clientJoin(r))) 
			.and(super.createSpecification());
	}

	private static ExtendedSpecification<MammaBrief> afspraakValtBinnenTijdelijkeLocatie()
	{
		return (r, q, cb) ->
		{
			var laatsteAfspraakJoin = laatsteAfspraakJoin(r);
			var tijdelijkeLocatieJoin = tijdelijkeLocatieJoin(laatsteAfspraakJoin);
			var laatsteAfspraakVanafDatum = laatsteAfspraakJoin.get(MammaAfspraak_.vanaf).as(LocalDate.class);
			return cb.and(
				cb.isNotNull(tijdelijkeLocatieJoin.get(TijdelijkAdres_.startDatum)), 
				cb.greaterThanOrEqualTo(laatsteAfspraakVanafDatum, tijdelijkeLocatieJoin.get(TijdelijkAdres_.startDatum).as(LocalDate.class)),
				cb.lessThanOrEqualTo(laatsteAfspraakVanafDatum, tijdelijkeLocatieJoin.get(TijdelijkAdres_.eindDatum).as(LocalDate.class))
			);
		};
	}

	private static ExtendedSpecification<MammaBrief> heeftAfspraakStandplaatsId(long standplaatsId)
	{
		return heeftId(standplaatsId).with(r -> afspraakStandplaatsJoin(laatsteAfspraakJoin(r)));
	}

	private static ExtendedSpecification<MammaBrief> heeftUitnodigingStandplaatsId(long standplaatsId)
	{
		return heeftId(standplaatsId).with(r -> uitnodigingStandplaatsJoin(laatsteUitnodigingJoin(r)));
	}

	private ExtendedSpecification<MammaBrief> eersteOfVervolgRondeSpecification()
	{
		var eersteRonde = isEersteRonde();

		if (eersteRonde == null)
		{
			return null;
		}

		return (r, q, cb) ->
		{
			var subquery = q.subquery(Long.class);
			var subRoot = subquery.from(MammaBrief.class);
			subquery.select(cb.count(cb.literal(1)));

			var clientJoin = join(subRoot, ClientBrief_.client);
			subquery.where(cb.and(
				cb.equal(clientJoin.get(Client_.mammaDossier), dossierJoin(r)),
				subRoot.get(Brief_.briefType).in(BriefType.getMammaEersteRondeBrieftype())
			));

			return eersteRonde ? cb.equal(subquery, 1L) : cb.greaterThan(subquery, 1L);
		};
	}

	private static Join<MammaStandplaatsRonde, MammaStandplaats> afspraakStandplaatsJoin(Join<?, MammaAfspraak> afspraakJoin)
	{
		var standplaatsPeriodeJoin = join(afspraakJoin, MammaAfspraak_.standplaatsPeriode, LEFT);
		var standplaatsRondeJoin = join(standplaatsPeriodeJoin, MammaStandplaatsPeriode_.standplaatsRonde, LEFT);
		return join(standplaatsRondeJoin, MammaStandplaatsRonde_.standplaats, LEFT);
	}

	private static Join<MammaStandplaats, MammaStandplaatsLocatie> tijdelijkeLocatieJoin(Join<?, MammaAfspraak> afspraakJoin)
	{
		var standplaatsJoin = afspraakStandplaatsJoin(afspraakJoin);
		return join(standplaatsJoin, MammaStandplaats_.tijdelijkeLocatie, LEFT);
	}

	private static Join<MammaStandplaats, MammaStandplaatsLocatie> normaleLocatieJoin(Join<?, MammaAfspraak> afspraakJoin)
	{
		return join(afspraakStandplaatsJoin(afspraakJoin), MammaStandplaats_.locatie, LEFT);
	}

	private static Join<MammaStandplaatsRonde, MammaStandplaats> uitnodigingStandplaatsJoin(Join<?, MammaUitnodiging> uitnodigingJoin)
	{
		var standplaatsRondeJoin = join(uitnodigingJoin, MammaUitnodiging_.standplaatsRonde);
		return join(standplaatsRondeJoin, MammaStandplaatsRonde_.standplaats);
	}

	private static Join<MammaStandplaats, MammaStandplaatsLocatie> uitnodigingslocatieJoin(From<?, ? extends MammaBrief> briefRoot)
	{
		return join(uitnodigingStandplaatsJoin(laatsteUitnodigingJoin(briefRoot)), MammaStandplaats_.locatie);
	}

	private static Join<MammaUitnodiging, MammaAfspraak> laatsteAfspraakJoin(From<?, ? extends MammaBrief> r)
	{
		var laatsteUitnodigingJoin = laatsteUitnodigingJoin(r);
		return join(laatsteUitnodigingJoin, MammaUitnodiging_.laatsteAfspraak, LEFT);
	}

	private Join<Client, MammaDossier> dossierJoin(From<?, ? extends MammaBrief> briefRoot)
	{
		return join(clientJoin(briefRoot), Client_.mammaDossier);
	}

	private BriefType briefType()
	{
		return BriefType.valueOf(getStepExecutionContext().getString(MammaBrievenGenererenPartitioner.KEY_BRIEFTYPE));
	}

	private boolean isAparteBrieven()
	{
		return Boolean.TRUE.equals(getStepExecutionContext().get(MammaBrievenGenererenPartitioner.KEY_BRIEFTYPEAPART));
	}

	private boolean isVoorTijdelijkeLocaties()
	{
		return Boolean.TRUE.equals(getStepExecutionContext().get(MammaBrievenGenererenPartitioner.KEY_TIJDELIJK));
	}

	private Long getStandplaatsId()
	{
		return (Long) getStepExecutionContext().get(MammaBrievenGenererenPartitioner.KEY_MAMMASTANDPLAATSID);
	}

	private Boolean isEersteRonde()
	{
		return (Boolean) getStepExecutionContext().get(MammaBrievenGenererenPartitioner.KEY_EERSTE_RONDE);
	}
}
