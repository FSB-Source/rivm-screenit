package nl.rivm.screenit.batch.jobs.colon.selectie.vooraankondigingnavervolgonderzoek;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-dk
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

import java.util.List;

import javax.persistence.criteria.JoinType;

import lombok.AllArgsConstructor;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.batch.jobs.helpers.BaseSpecificationScrollableResultReader;
import nl.rivm.screenit.model.Client_;
import nl.rivm.screenit.model.colon.ColonDossier_;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.colon.ColonScreeningRonde_;
import nl.rivm.screenit.model.colon.ColonVolgendeUitnodiging_;
import nl.rivm.screenit.model.colon.enums.ColonUitnodigingsintervalType;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.specification.algemeen.ClientSpecification;
import nl.rivm.screenit.specification.algemeen.PersoonSpecification;
import nl.rivm.screenit.specification.colon.ColonScreeningRondeSpecification;
import nl.rivm.screenit.specification.colon.ColonUitnodigingBaseSpecification;
import nl.rivm.screenit.specification.colon.ColonUitnodigingsintervalSpecification;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import static nl.rivm.screenit.specification.SpecificationUtil.join;

@Component
@AllArgsConstructor
public class ColonVooraankondigingNaVervolgonderzoekReader extends BaseSpecificationScrollableResultReader<ColonScreeningRonde>
{

	private final ICurrentDateSupplier currentDateSupplier;

	private final SimplePreferenceService preferenceService;

	@Override
	protected Specification<ColonScreeningRonde> createSpecification()
	{
		var vandaag = currentDateSupplier.getLocalDate();
		var peildatum = vandaag.plusWeeks(preferenceService.getInteger(PreferenceKey.COLON_VOORAANKONDIGING_NA_VERVOLGONDERZOEK.name()));
		var minLeeftijd = preferenceService.getInteger(PreferenceKey.MINIMALE_LEEFTIJD_COLON.name());
		var maxLeeftijd = preferenceService.getInteger(PreferenceKey.MAXIMALE_LEEFTIJD_COLON.name());

		return (r, q, cb) ->
		{
			var dossierJoin = join(r, ColonScreeningRonde_.dossier);
			var volgendeUitnodigingJoin = join(dossierJoin, ColonDossier_.volgendeUitnodiging);
			var clientJoin = join(dossierJoin, ColonDossier_.client);
			var persoonJoin = join(clientJoin, Client_.persoon);
			var intervalJoin = join(volgendeUitnodigingJoin, ColonVolgendeUitnodiging_.interval);

			return
				ColonScreeningRondeSpecification.heeftGeenBriefVanTypeIn(List.of(BriefType.COLON_VOORAANKONDIGING_NA_VERVOLGONDERZOEK))
					.and(ColonUitnodigingBaseSpecification.u2Base(peildatum, vandaag, JoinType.INNER).with(ri -> join(ri, ColonScreeningRonde_.dossier)))
					.and(PersoonSpecification.valtBinnenLeeftijdGrensRestricties(minLeeftijd, maxLeeftijd, null, peildatum).with(root -> persoonJoin))
					.and(ClientSpecification.heeftActieveClient().with(root -> clientJoin))
					.and(ColonUitnodigingsintervalSpecification.heeftTypeIn(ColonUitnodigingsintervalType.intervalTypesMetVooraankondiging).with(root -> intervalJoin))
					.and(ColonUitnodigingsintervalSpecification.heeftAantal().with(root -> intervalJoin)).toPredicate(r, q, cb);
		};
	}
}
