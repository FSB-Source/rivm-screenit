package nl.rivm.screenit.batch.jobs.mamma.herinneren.step;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bk
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

import java.time.LocalDateTime;

import javax.persistence.criteria.Join;
import javax.persistence.criteria.JoinType;
import javax.persistence.criteria.Root;

import lombok.AllArgsConstructor;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.batch.jobs.helpers.BaseSpecificationScrollableResultReader;
import nl.rivm.screenit.model.DossierStatus;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde_;
import nl.rivm.screenit.model.mamma.MammaUitnodiging;
import nl.rivm.screenit.model.mamma.MammaUitnodiging_;
import nl.rivm.screenit.model.mamma.enums.MammaAfspraakStatus;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.specification.algemeen.BriefSpecification;
import nl.rivm.screenit.specification.algemeen.DossierSpecification;
import nl.rivm.screenit.specification.mamma.MammaAfspraakSpecification;
import nl.rivm.screenit.specification.mamma.MammaBaseDossierSpecification;
import nl.rivm.screenit.specification.mamma.MammaUitnodigingSpecification;
import nl.rivm.screenit.specification.mamma.MammaUitstelSpecification;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import com.google.common.collect.Range;

import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.algemeen.ScreeningRondeSpecification.isLopend;
import static nl.rivm.screenit.specification.mamma.MammaScreeningRondeSpecification.heeftGeenUitstel;
import static nl.rivm.screenit.specification.mamma.MammaScreeningRondeSpecification.isMinderValideOnderzoekZiekenhuis;

@Component
@AllArgsConstructor
public class MammaHerinnerenReader extends BaseSpecificationScrollableResultReader<MammaScreeningRonde>
{

	private final ICurrentDateSupplier currentDateSupplier;

	private final SimplePreferenceService preferenceService;

	@Override
	protected Specification<MammaScreeningRonde> createSpecification()
	{
		var heeftGeplandeAfspraakVoorMaxNoShowPeriode = MammaAfspraakSpecification.valtInDatumTijdPeriode(Range.atMost(getMaxNoshowPeriode()))
			.and(MammaAfspraakSpecification.heeftStatus(MammaAfspraakStatus.GEPLAND)).withRoot(this::getLaatsteAfspraakJoin);

		var heeftActiefDossierZonderTehuis = DossierSpecification.heeftStatus(DossierStatus.ACTIEF).with(MammaScreeningRonde_.dossier)
			.and(MammaBaseDossierSpecification.woontNietInTehuis().with(MammaScreeningRonde_.dossier));

		var laatsteUitnodigingIsNietHerinnerd = MammaUitnodigingSpecification.isHerinnerd(false).withRoot(this::getLaatsteUitnodigingJoin);

		var uitnodigingCreatieDatumLigtVoorHerinneringsPeriodeAfspraak = MammaUitnodigingSpecification.isGemaaktOpOfVoor(getMaxGeenAfspraakPeriode())
			.withRoot(this::getLaatsteUitnodigingJoin);

		return isMinderValideOnderzoekZiekenhuis(false)
			.and(isLopend())
			.and(heeftActiefDossierZonderTehuis)
			.and(laatsteUitnodigingIsNietHerinnerd)
			.and(heeftGeenActiefUitstel())
			.and(heeftGeplandeAfspraakVoorMaxNoShowPeriode.or(
					heeftGeenLaatsteAfspraak()
						.and(heeftGeenSuspectBriefGekoppeldAanUitnodiging())
						.and(uitnodigingCreatieDatumLigtVoorHerinneringsPeriodeAfspraak)
				)
			);
	}

	private Specification<MammaScreeningRonde> heeftGeenLaatsteAfspraak()
	{
		return MammaUitnodigingSpecification.heeftGeenLaatsteAfspraak().withRoot(this::getLaatsteUitnodigingJoin);
	}

	private Specification<MammaScreeningRonde> heeftGeenSuspectBriefGekoppeldAanUitnodiging()
	{
		return BriefSpecification.heeftNietBriefType(BriefType.MAMMA_UITNODIGING_SUSPECT).withRoot(r -> join(getLaatsteUitnodigingJoin(r), MammaUitnodiging_.brief));
	}

	private Specification<MammaScreeningRonde> heeftGeenActiefUitstel()
	{
		return heeftGeenUitstel().or(
			MammaUitstelSpecification.heeftUitnodiging()
				.or(MammaUitstelSpecification.isGeannuleerd()).with(MammaScreeningRonde_.laatsteUitstel, JoinType.LEFT)
		);
	}

	private Join<MammaScreeningRonde, MammaUitnodiging> getLaatsteUitnodigingJoin(Root<MammaScreeningRonde> r)
	{
		return join(r, MammaScreeningRonde_.laatsteUitnodiging);
	}

	private Join<MammaUitnodiging, MammaAfspraak> getLaatsteAfspraakJoin(Root<MammaScreeningRonde> r)
	{
		var uitnodigingJoin = getLaatsteUitnodigingJoin(r);
		return join(uitnodigingJoin, MammaUitnodiging_.laatsteAfspraak, JoinType.LEFT);
	}

	private LocalDateTime getMaxGeenAfspraakPeriode()
	{
		var herinneringsPeriodeGeenAfspraak = preferenceService.getInteger(PreferenceKey.MAMMA_HERINNERINGS_PERIODE_GEEN_AFSPRAAK.name(), 4);
		return currentDateSupplier.getLocalDateTime().minusWeeks(herinneringsPeriodeGeenAfspraak);
	}

	private LocalDateTime getMaxNoshowPeriode()
	{
		var herinneringsPeriodeNoShow = preferenceService.getInteger(PreferenceKey.MAMMA_HERINNERINGS_PERIODE_NO_SHOW.name(), 2);
		return currentDateSupplier.getLocalDateTime().minusWeeks(herinneringsPeriodeNoShow);
	}
}
