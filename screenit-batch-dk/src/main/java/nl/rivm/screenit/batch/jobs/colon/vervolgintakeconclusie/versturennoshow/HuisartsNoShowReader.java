package nl.rivm.screenit.batch.jobs.colon.vervolgintakeconclusie.versturennoshow;

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

import java.util.Date;

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
public class HuisartsNoShowReader extends BaseSpecificationScrollableResultReader<ColonScreeningRonde>
{

	private final SimplePreferenceService preferenceService;

	private final ICurrentDateSupplier currentDateSupplier;

	@Override
	protected Specification<ColonScreeningRonde> createSpecification()
	{
		var conclusieMoetGegevenZijnOp = getNoShowDate();

		return ClientSpecification.heeftActieveClient().withRoot(this::clientJoin)
			.and(ScreeningRondeSpecification.isLopend())
			.and(DossierSpecification.heeftStatus(DossierStatus.ACTIEF).withRoot(this::dossierJoin))
			.and(ColonScreeningRondeSpecification.heeftGeenAfgerondeVerslagen())
			.and(ColonConclusieSpecification.heeftType(ColonConclusieType.NO_SHOW)
				.and(ColonConclusieSpecification.heeftDatumVoorOfOp(conclusieMoetGegevenZijnOp))
				.and(ColonConclusieSpecification.heeftGeenNoShowBericht()).withRoot(this::conclusieJoin));
	}

	private Date getNoShowDate()
	{
		try
		{
			var periode = preferenceService.getInteger(PreferenceKey.HUISARTS_NO_SHOW_PERIODE.name());
			var nu = currentDateSupplier.getLocalDateTime();
			return DateUtil.toUtilDate(nu.minusDays(periode));
		}
		catch (Exception e)
		{
			crashMelding("De huisarts no show periode is niet gezet.", e);
			throw e;
		}
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
