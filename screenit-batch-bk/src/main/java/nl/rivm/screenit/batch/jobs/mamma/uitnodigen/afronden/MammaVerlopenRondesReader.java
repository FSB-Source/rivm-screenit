package nl.rivm.screenit.batch.jobs.mamma.uitnodigen.afronden;

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

import javax.persistence.criteria.Join;
import javax.persistence.criteria.JoinType;
import javax.persistence.criteria.Root;

import lombok.AllArgsConstructor;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.batch.jobs.helpers.BaseSpecificationScrollableResultReader;
import nl.rivm.screenit.model.Client_;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaDossier_;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde_;
import nl.rivm.screenit.model.mamma.MammaUitnodiging_;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.specification.HibernateObjectSpecification;
import nl.rivm.screenit.specification.algemeen.ScreeningRondeSpecification;
import nl.rivm.screenit.specification.mamma.MammaAfspraakSpecification;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import com.google.common.collect.Range;

import static nl.rivm.screenit.model.enums.Deelnamemodus.SELECTIEBLOKKADE;
import static nl.rivm.screenit.model.mamma.enums.MammaAfspraakStatus.GEPLAND;
import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.algemeen.DossierSpecification.heeftDeelnamemodus;
import static nl.rivm.screenit.specification.algemeen.PersoonSpecification.isGeborenVoorOfOp;
import static nl.rivm.screenit.specification.algemeen.ScreeningRondeSpecification.isAangemaaktVoor;
import static nl.rivm.screenit.specification.mamma.MammaScreeningRondeSpecification.heeftGeenLaatsteOnderzoek;

@Component
@AllArgsConstructor
public class MammaVerlopenRondesReader extends BaseSpecificationScrollableResultReader<MammaScreeningRonde>
{
	private static final int MAXIMALE_LEEFTIJD_OFFSET = 1;

	private final ICurrentDateSupplier currentDateSupplier;

	private final SimplePreferenceService preferenceService;

	@Override
	protected Specification<MammaScreeningRonde> createSpecification()
	{
		var vandaag = currentDateSupplier.getLocalDate();
		var leeftijdPeilmoment = vandaag.minusYears(preferenceService.getLong(PreferenceKey.MAMMA_MAXIMALE_LEEFTIJD.name()) + MAXIMALE_LEEFTIJD_OFFSET);
		var maxDuurRonde = vandaag.minusMonths(Constants.BK_GELDIGHEID_RONDE_MAANDEN);

		return ScreeningRondeSpecification.<MammaScreeningRonde> isLopend()
			.and(isGeborenVoorOfOp(leeftijdPeilmoment).withRoot(this::getPersoonJoin)
				.or(heeftDeelnamemodus(SELECTIEBLOKKADE).with(MammaScreeningRonde_.dossier)))
			.and(isAangemaaktVoor(maxDuurRonde))
			.and(heeftGeenLaatsteOnderzoek())
			.and(heeftGeenToekomstigGeplandeAfspraak(vandaag));
	}

	private Specification<MammaScreeningRonde> heeftGeenToekomstigGeplandeAfspraak(LocalDate vandaag)
	{
		var totVandaag = Range.lessThan(vandaag);

		var specs = HibernateObjectSpecification.<MammaAfspraak> heeftGeenId()
			.or(HibernateObjectSpecification.<MammaAfspraak> heeftId()
				.and(MammaAfspraakSpecification.heeftNietStatus(GEPLAND)
					.or(MammaAfspraakSpecification.valtInDatumPeriode(totVandaag))));

		return specs.withRoot(this::getLaatsteAfspraakJoin);
	}

	private Join<?, GbaPersoon> getPersoonJoin(Root<MammaScreeningRonde> r)
	{
		var dossierJoin = join(r, MammaScreeningRonde_.dossier);
		var clientJoin = join(dossierJoin, MammaDossier_.client);
		return join(clientJoin, Client_.persoon);
	}

	private Join<?, MammaAfspraak> getLaatsteAfspraakJoin(Root<MammaScreeningRonde> r)
	{
		var laatsteUitnodiging = join(r, MammaScreeningRonde_.laatsteUitnodiging, JoinType.LEFT);
		return join(laatsteUitnodiging, MammaUitnodiging_.laatsteAfspraak, JoinType.LEFT);
	}
}
