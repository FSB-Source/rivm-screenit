package nl.rivm.screenit.batch.jobs.colon.vervolgintakeconclusie.briefandereintakelocatie;

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

import java.time.LocalDate;
import java.util.Date;
import java.util.List;

import javax.persistence.criteria.Join;
import javax.persistence.criteria.Root;

import lombok.AllArgsConstructor;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.batch.jobs.helpers.BaseSpecificationScrollableResultReader;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.DossierStatus;
import nl.rivm.screenit.model.colon.ColonConclusie;
import nl.rivm.screenit.model.colon.ColonDossier;
import nl.rivm.screenit.model.colon.ColonDossier_;
import nl.rivm.screenit.model.colon.ColonIntakeAfspraak;
import nl.rivm.screenit.model.colon.ColonIntakeAfspraak_;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.colon.ColonScreeningRonde_;
import nl.rivm.screenit.model.colon.enums.ColonConclusieType;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.specification.algemeen.ClientSpecification;
import nl.rivm.screenit.specification.algemeen.DossierSpecification;
import nl.rivm.screenit.specification.algemeen.ScreeningRondeSpecification;
import nl.rivm.screenit.specification.colon.ColonConclusieSpecification;
import nl.rivm.screenit.specification.colon.ColonScreeningRondeSpecification;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import static nl.rivm.screenit.specification.SpecificationUtil.join;

@Component
@AllArgsConstructor
public class HerinneringClientWilAnderIntakeLocatieBriefReader extends BaseSpecificationScrollableResultReader<ColonScreeningRonde>
{

	private final ICurrentDateSupplier currentDateSupplier;

	private final SimplePreferenceService preferenceService;

	@Override
	protected Specification<ColonScreeningRonde> createSpecification()
	{
		var nu = currentDateSupplier.getLocalDateTime();
		var uitnodigingsInterval = preferenceService.getInteger(PreferenceKey.UITNODIGINGSINTERVAL.name());
		if (uitnodigingsInterval == null)
		{
			throw new IllegalStateException("Spreidingsperiode op de parameterisatie pagina is niet gezet");
		}
		var conclusieMoetGegevenZijnOp = getHerinneringClientWilAnderIntakeAfspraakDate();
		var uitnodigingsIntervalVerlopen = nu.minusDays(uitnodigingsInterval).plusWeeks(2);

		return ClientSpecification.heeftActieveClient().withRoot(this::clientJoin)
			.and(ScreeningRondeSpecification.isLopend())
			.and(DossierSpecification.heeftStatus(DossierStatus.ACTIEF).withRoot(this::dossierJoin))
			.and(ColonConclusieSpecification.heeftType(ColonConclusieType.CLIENT_WIL_ANDERE_INTAKELOKATIE)
				.and(ColonConclusieSpecification.heeftDatumVoorOfOp(conclusieMoetGegevenZijnOp)).withRoot(this::conclusieJoin))
			.and(ColonScreeningRondeSpecification.heeftGeenBriefVanTypeIn(List.of(BriefType.COLON_HERINNERING_ANDERE_INTAKELOCATIE)))
			.and(ColonScreeningRondeSpecification.heeftGeenAfsprakenZonderVervolg(uitnodigingsIntervalVerlopen.toLocalDate()))
			.and(ColonScreeningRondeSpecification.heefGeenOpenUitnodigingNa(uitnodigingsIntervalVerlopen))
			.and(ColonScreeningRondeSpecification.heeftGeenAfgerondeVerslagen());
	}

	private Date getHerinneringClientWilAnderIntakeAfspraakDate()
	{
		LocalDate vandaag = currentDateSupplier.getLocalDate();
		return DateUtil.toUtilDate(vandaag.minusWeeks(6));
	}

	private Join<ColonIntakeAfspraak, ColonConclusie> conclusieJoin(Root<ColonScreeningRonde> r)
	{
		return join(join(r, ColonScreeningRonde_.laatsteAfspraak), ColonIntakeAfspraak_.conclusie);
	}

	private Join<ColonDossier, Client> clientJoin(Root<ColonScreeningRonde> r)
	{
		return join(dossierJoin(r), ColonDossier_.client);
	}

	private Join<ColonScreeningRonde, ColonDossier> dossierJoin(Root<ColonScreeningRonde> r)
	{
		return join(r, ColonScreeningRonde_.dossier);
	}
}
