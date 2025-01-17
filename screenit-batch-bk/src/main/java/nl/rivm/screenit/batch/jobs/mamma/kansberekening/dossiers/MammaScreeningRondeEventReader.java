package nl.rivm.screenit.batch.jobs.mamma.kansberekening.dossiers;

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

import javax.persistence.criteria.From;
import javax.persistence.criteria.Join;

import lombok.AllArgsConstructor;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.batch.jobs.helpers.BaseSpecificationScrollableResultReader;
import nl.rivm.screenit.model.Client_;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaDossier_;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import com.google.common.collect.Range;

import static java.time.temporal.TemporalAdjusters.firstDayOfYear;
import static java.time.temporal.TemporalAdjusters.lastDayOfYear;
import static nl.rivm.screenit.model.DossierStatus.ACTIEF;
import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.algemeen.ClientSpecification.heeftActieveClient;
import static nl.rivm.screenit.specification.algemeen.DossierSpecification.heeftStatus;
import static nl.rivm.screenit.specification.algemeen.PersoonSpecification.heeftGeboortedatumIn;
import static nl.rivm.screenit.specification.mamma.MammaBaseDossierSpecification.heeftGeenScreeningRondeEvent;

@Component
@AllArgsConstructor
public class MammaScreeningRondeEventReader extends BaseSpecificationScrollableResultReader<MammaDossier>
{
	public static final int MINIMALE_LEEFTIJD_OFFSET = -3;

	public static final int MAXIMALE_LEEFTIJD_OFFSET = 1;

	private final ICurrentDateSupplier dateSupplier;

	private final SimplePreferenceService preferenceService;

	@Override
	protected Specification<MammaDossier> createSpecification()
	{
		var vandaag = dateSupplier.getLocalDate();
		var minimaleLeeftijd = preferenceService.getInteger(PreferenceKey.MAMMA_MINIMALE_LEEFTIJD.name()) + MINIMALE_LEEFTIJD_OFFSET;
		var maximaleLeeftijd = preferenceService.getInteger(PreferenceKey.MAMMA_MAXIMALE_LEEFTIJD.name()) + MAXIMALE_LEEFTIJD_OFFSET;
		var geboortedatumBereik = Range.closed(
			vandaag.minusYears(maximaleLeeftijd).with(firstDayOfYear()),
			vandaag.minusYears(minimaleLeeftijd).with(lastDayOfYear()));

		return heeftGeenScreeningRondeEvent()
			.and(heeftStatus(ACTIEF))
			.and(heeftActieveClient().with(MammaDossier_.client))
			.and(heeftGeboortedatumIn(geboortedatumBereik).with(r -> getPersoonJoin(r)));
	}

	private static Join<?, GbaPersoon> getPersoonJoin(From<?, ? extends MammaDossier> r)
	{
		var clientJoin = join(r, MammaDossier_.client);
		return join(clientJoin, Client_.persoon);
	}
}
