package nl.rivm.screenit.batch.jobs.colon.gunstigeuitslag.gunstigestep;

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

import java.util.List;

import javax.persistence.criteria.From;
import javax.persistence.criteria.Join;

import lombok.AllArgsConstructor;

import nl.rivm.screenit.batch.jobs.helpers.BaseSpecificationScrollableResultReader;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Client_;
import nl.rivm.screenit.model.DossierStatus;
import nl.rivm.screenit.model.colon.ColonDossier;
import nl.rivm.screenit.model.colon.ColonDossier_;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.colon.ColonScreeningRonde_;
import nl.rivm.screenit.model.colon.ColonUitnodiging;
import nl.rivm.screenit.model.colon.IFOBTTest;
import nl.rivm.screenit.model.colon.IFOBTTest_;
import nl.rivm.screenit.model.colon.IFOBTType;
import nl.rivm.screenit.model.colon.enums.IFOBTTestStatus;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.specification.algemeen.ClientSpecification;
import nl.rivm.screenit.specification.algemeen.DossierSpecification;
import nl.rivm.screenit.specification.colon.ColonFITSpecification;
import nl.rivm.screenit.specification.colon.ColonScreeningRondeSpecification;
import nl.rivm.screenit.specification.colon.ColonUitnodigingSpecification;

import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import static nl.rivm.screenit.specification.SpecificationUtil.join;

@Component
@AllArgsConstructor
public class GunstigeUitslagBriefReader extends BaseSpecificationScrollableResultReader<Client>
{
	private final ICurrentDateSupplier currentDateSupplier;

	@Override
	protected Specification<Client> createSpecification()
	{
		try
		{
			return ClientSpecification.heeftActieveClient()
				.and(DossierSpecification.heeftStatus(DossierStatus.ACTIEF).with(Client_.colonDossier))
				.and(ColonUitnodigingSpecification.heeftUitgesteldeUitslagDatumVoorOfOp(currentDateSupplier.getDate()).with(r -> uitnodigingJoin(r)))
				.and(ColonFITSpecification.heeftFitType(IFOBTType.GOLD).with(r -> fitJoin(r)))
				.and(ColonFITSpecification.heeftStatus(IFOBTTestStatus.UITGEVOERD).with(r -> fitJoin(r)))
				.and(ColonFITSpecification.heeftGunstigeUitslag().with(r -> fitJoin(r)))
				.and(ColonScreeningRondeSpecification.heeftGeenBriefVanTypeIn(List.of(BriefType.COLON_UITNODIGING_INTAKE, BriefType.COLON_GUNSTIGE_UITSLAG))
					.with(r -> laatsteScreeningRondeJoin(r)));
		}
		catch (Exception e)
		{
			crashMelding("Cliënten konden niet worden geselecteerd", e);
			throw e;
		}
	}

	private From<?, ? extends ColonUitnodiging> uitnodigingJoin(From<?, ? extends Client> r)
	{
		return join(fitJoin(r), IFOBTTest_.colonUitnodiging);
	}

	private From<?, ? extends IFOBTTest> fitJoin(From<?, ? extends Client> r)
	{
		return join(laatsteScreeningRondeJoin(r), ColonScreeningRonde_.ifobtTesten);
	}

	private static Join<ColonDossier, ColonScreeningRonde> laatsteScreeningRondeJoin(From<?, ? extends Client> r)
	{
		return join(join(r, Client_.colonDossier), ColonDossier_.laatsteScreeningRonde);
	}
}
