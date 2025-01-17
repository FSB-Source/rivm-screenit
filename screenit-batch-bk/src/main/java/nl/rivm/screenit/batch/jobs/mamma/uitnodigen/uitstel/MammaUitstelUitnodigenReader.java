package nl.rivm.screenit.batch.jobs.mamma.uitnodigen.uitstel;

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

import java.util.Date;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.From;
import javax.persistence.criteria.Join;
import javax.persistence.criteria.Root;
import javax.persistence.criteria.Subquery;

import lombok.AllArgsConstructor;

import nl.rivm.screenit.batch.jobs.helpers.BaseSpecificationScrollableResultReader;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Client_;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaDossier_;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde_;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid_;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode_;
import nl.rivm.screenit.model.mamma.MammaStandplaatsRonde_;
import nl.rivm.screenit.model.mamma.MammaStandplaats_;
import nl.rivm.screenit.model.mamma.MammaUitstel;
import nl.rivm.screenit.model.mamma.MammaUitstel_;
import nl.rivm.screenit.model.mamma.enums.MammaUitstelReden;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.specification.algemeen.ScreeningRondeSpecification;
import nl.rivm.screenit.util.DateUtil;

import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import static nl.rivm.screenit.specification.ExtendedSpecification.not;
import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.algemeen.ClientSpecification.voldoetAanMammaClientSelectieRestricties;
import static nl.rivm.screenit.specification.mamma.MammaUitstelSpecification.heeftUitnodiging;
import static nl.rivm.screenit.specification.mamma.MammaUitstelSpecification.heeftUitstelReden;
import static nl.rivm.screenit.specification.mamma.MammaUitstelSpecification.isGeannuleerd;

@Component
@AllArgsConstructor
public class MammaUitstelUitnodigenReader extends BaseSpecificationScrollableResultReader<Client>
{
	private final ICurrentDateSupplier currentDateSupplier;

	@Override
	protected Specification<Client> createSpecification()
	{
		return heeftLopendeRonde().and(heeftGeldigeUitstel()).and(streefDatumVoorEerstVolgendeStandplaatsPeriodeHeeftVrijgegevenTotEnMetOfMaximaleDatum())
			.and(voldoetAanMammaClientSelectieRestricties());
	}

	private Specification<Client> heeftGeldigeUitstel()
	{
		return not(isGeannuleerd()).and(not(heeftUitnodiging())).and(heeftUitstelReden(MammaUitstelReden.CLIENT_CONTACT)).with(r -> getLaatsteUitstelJoin(r));
	}

	private Specification<Client> heeftLopendeRonde()
	{
		return ScreeningRondeSpecification.isLopend().with(r -> screeningRondeJoin(r));
	}

	private Specification<Client> streefDatumVoorEerstVolgendeStandplaatsPeriodeHeeftVrijgegevenTotEnMetOfMaximaleDatum()
	{
		var maximaleStreefDatumIndienStandplaatsNietInEenRoute = DateUtil.toUtilDate(currentDateSupplier.getLocalDate().plusWeeks(8));
		var vandaag = currentDateSupplier.getDateMidnight();
		return (r, q, cb) ->
		{
			var subqueryVrijgevenTotEnMetVanStandplaatsInRoute = q.subquery(Date.class);
			var subroot = subqueryVrijgevenTotEnMetVanStandplaatsInRoute.from(MammaScreeningsEenheid.class);
			var subStandplaatsPeriodenJoin = join(subroot, MammaScreeningsEenheid_.standplaatsPerioden);
			var subStandplaatsRondeJoin = join(subStandplaatsPeriodenJoin, MammaStandplaatsPeriode_.standplaatsRonde);
			var subStandplaatsJoin = join(subStandplaatsRondeJoin, MammaStandplaatsRonde_.standplaats);

			var minimalePeriodeVanafVanStandplaats = minimalePeriodeVanafVanStandplaats(subqueryVrijgevenTotEnMetVanStandplaatsInRoute, r, cb, vandaag);

			subqueryVrijgevenTotEnMetVanStandplaatsInRoute.select(cb.least(subroot.get(MammaScreeningsEenheid_.vrijgegevenTotEnMet)))
				.where(
					cb.isTrue(subroot.get(MammaScreeningsEenheid_.actief)),
					cb.isTrue(subStandplaatsJoin.get(MammaStandplaats_.actief)),
					cb.greaterThanOrEqualTo(subStandplaatsPeriodenJoin.get(MammaStandplaatsPeriode_.totEnMet), vandaag),
					cb.equal(subStandplaatsRondeJoin.get(MammaStandplaatsRonde_.standplaats), getLaatsteUitstelJoin(r).get(MammaUitstel_.standplaats)),
					cb.equal(subStandplaatsPeriodenJoin.get(MammaStandplaatsPeriode_.vanaf), minimalePeriodeVanafVanStandplaats));

			var streefDatumLigtVoorVrijgevenTotEnMetVanStandplaatsInRoute = cb.lessThanOrEqualTo(getLaatsteUitstelJoin(r).get(MammaUitstel_.streefDatum),
				subqueryVrijgevenTotEnMetVanStandplaatsInRoute);

			var standplaatsNietInActieveRoute = cb.isNull(subqueryVrijgevenTotEnMetVanStandplaatsInRoute);
			var standplaatsNietInRouteEnStreefdatumLigtVoorMaximaleStreefDatum = cb.and(
				standplaatsNietInActieveRoute,
				cb.lessThanOrEqualTo(getLaatsteUitstelJoin(r).get(MammaUitstel_.streefDatum), maximaleStreefDatumIndienStandplaatsNietInEenRoute));

			return cb.or(streefDatumLigtVoorVrijgevenTotEnMetVanStandplaatsInRoute, standplaatsNietInRouteEnStreefdatumLigtVoorMaximaleStreefDatum);
		};
	}

	private Subquery<Date> minimalePeriodeVanafVanStandplaats(Subquery<Date> subqueryVrijgevenTotEnMetVanStandplaatsInRoute, Root<Client> clientRoot, CriteriaBuilder cb,
		Date vandaag)
	{
		var minimaleVanafSubquery = subqueryVrijgevenTotEnMetVanStandplaatsInRoute.subquery(Date.class);
		var minimaleVanafSubroot = minimaleVanafSubquery.from(MammaStandplaatsPeriode.class);

		var subMinimaleVanafStandplaats = join(join(minimaleVanafSubroot, MammaStandplaatsPeriode_.standplaatsRonde), MammaStandplaatsRonde_.standplaats);
		var subMinimaleVanafScreeningsEenheid = join(minimaleVanafSubroot, MammaStandplaatsPeriode_.screeningsEenheid);
		minimaleVanafSubquery.select(cb.least(minimaleVanafSubroot.get(MammaStandplaatsPeriode_.vanaf)))
			.where(
				cb.isTrue(subMinimaleVanafScreeningsEenheid.get(MammaScreeningsEenheid_.actief)),
				cb.isTrue(subMinimaleVanafStandplaats.get(MammaStandplaats_.actief)),
				cb.greaterThanOrEqualTo(minimaleVanafSubroot.get(MammaStandplaatsPeriode_.totEnMet), vandaag),
				cb.equal(subMinimaleVanafStandplaats, getLaatsteUitstelJoin(clientRoot).get(MammaUitstel_.standplaats))
			);
		return minimaleVanafSubquery;
	}

	private static Join<? extends Client, MammaDossier> dossierJoin(From<?, ? extends Client> r)
	{
		return join(r, Client_.mammaDossier);
	}

	private static Join<MammaDossier, MammaScreeningRonde> screeningRondeJoin(From<?, ? extends Client> r)
	{
		return join(dossierJoin(r), MammaDossier_.laatsteScreeningRonde);
	}

	private static Join<MammaScreeningRonde, MammaUitstel> getLaatsteUitstelJoin(From<?, ? extends Client> r)
	{
		return join(screeningRondeJoin(r), MammaScreeningRonde_.laatsteUitstel);
	}
}
