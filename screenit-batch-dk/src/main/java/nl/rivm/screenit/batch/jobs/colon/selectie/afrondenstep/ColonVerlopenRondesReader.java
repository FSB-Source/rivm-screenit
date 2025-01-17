package nl.rivm.screenit.batch.jobs.colon.selectie.afrondenstep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-dk
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

import javax.persistence.criteria.JoinType;

import lombok.AllArgsConstructor;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.batch.jobs.helpers.BaseSpecificationScrollableResultReader;
import nl.rivm.screenit.model.Client_;
import nl.rivm.screenit.model.TablePerClassHibernateObject_;
import nl.rivm.screenit.model.colon.ColonDossier_;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.colon.ColonScreeningRonde_;
import nl.rivm.screenit.model.colon.IFOBTTest_;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.specification.algemeen.PersoonSpecification;
import nl.rivm.screenit.specification.algemeen.ScreeningRondeSpecification;
import nl.rivm.screenit.specification.colon.ColonIntakeAfspraakSpecification;
import nl.rivm.screenit.specification.colon.ColonScreeningRondeSpecification;
import nl.rivm.screenit.specification.colon.ColonUitnodigingSpecification;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import static nl.rivm.screenit.specification.SpecificationUtil.join;

@Component
@AllArgsConstructor
public class ColonVerlopenRondesReader extends BaseSpecificationScrollableResultReader<ColonScreeningRonde>
{

	private static final int DAGEN_WACHTTIJD_CONCLUSIE = 5;

	private final ICurrentDateSupplier currentDateSupplier;

	private final SimplePreferenceService preferenceService;

	@Override
	protected Specification<ColonScreeningRonde> createSpecification()
	{
		var currentDate = currentDateSupplier.getLocalDate();

		var maxLeeftijdDatum = currentDate.minusYears(preferenceService.getInteger(PreferenceKey.MAXIMALE_LEEFTIJD_COLON.name()) + 1L);
		var maxLengteRondeDatum = currentDate.minusDays(preferenceService.getInteger(PreferenceKey.UITNODIGINGSINTERVAL.name()));
		var wachttijdNaAfspraakDatum = currentDate.minusDays(DAGEN_WACHTTIJD_CONCLUSIE).atStartOfDay();

		return (r, q, cb) ->
		{
			var dossierJoin = join(r, ColonScreeningRonde_.dossier);
			var clientJoin = join(dossierJoin, ColonDossier_.client);
			var persoonJoin = join(clientJoin, Client_.persoon);
			var uitnodigingJoin = join(r, ColonScreeningRonde_.laatsteUitnodiging, JoinType.LEFT);
			var afspraakJoin = join(r, ColonScreeningRonde_.laatsteAfspraak, JoinType.LEFT);
			var testenJoin = join(r, ColonScreeningRonde_.ifobtTesten, JoinType.LEFT);

			return ScreeningRondeSpecification.<ColonScreeningRonde> isLopend()
				.and(PersoonSpecification.isGeborenVoor(maxLeeftijdDatum).with(root -> persoonJoin))
				.and(ScreeningRondeSpecification.isAangemaaktVoor(maxLengteRondeDatum))
				.and(ColonScreeningRondeSpecification.heeftBriefZonderFit()
					.or(ColonScreeningRondeSpecification.heeftGeenLaatsteUitnodiging())
					.or(ColonUitnodigingSpecification.heeftVerlopenFit(maxLengteRondeDatum).with(root -> uitnodigingJoin))
					.or(ColonScreeningRondeSpecification.heeftGeenLaatsteAfspraak())
					.or(ColonIntakeAfspraakSpecification.conclusieNietBinnenWachtperiodeVerwerkt(wachttijdNaAfspraakDatum).with(root -> afspraakJoin))
					.or(ColonScreeningRondeSpecification.isEersteOngunstigeUitslagUitLaatsteRonde(testenJoin.get(IFOBTTest_.statusDatum), r.get(TablePerClassHibernateObject_.id),
						maxLengteRondeDatum))
				)
				.toPredicate(r, q, cb);
		};
	}

}
