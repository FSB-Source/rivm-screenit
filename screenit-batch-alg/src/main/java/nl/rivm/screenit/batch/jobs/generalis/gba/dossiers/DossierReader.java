package nl.rivm.screenit.batch.jobs.generalis.gba.dossiers;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-alg
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

import nl.rivm.screenit.batch.jobs.helpers.BaseSpecificationScrollableResultReader;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Client_;
import nl.topicuszorg.patientregistratie.persoonsgegevens.model.Geslacht;

import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.algemeen.ClientSpecification.heeftCervixDossier;
import static nl.rivm.screenit.specification.algemeen.ClientSpecification.heeftColonDossier;
import static nl.rivm.screenit.specification.algemeen.ClientSpecification.heeftMammaDossier;
import static nl.rivm.screenit.specification.algemeen.PersoonSpecification.heeftGeslachtIn;
import static org.springframework.data.jpa.domain.Specification.not;

@Component
public class DossierReader extends BaseSpecificationScrollableResultReader<Client>
{
	@Override
	protected Specification<Client> createSpecification()
	{
		return not(heeftColonDossier())
			.or(isVrouwOfOnbekend().and(heeftGeenMammaOfCervixDossier()));
	}

	private Specification<Client> isVrouwOfOnbekend()
	{
		return heeftGeslachtIn(List.of(Geslacht.VROUW, Geslacht.ONBEKEND)).with(r ->
			join(r, Client_.persoon));
	}

	private Specification<Client> heeftGeenMammaOfCervixDossier()
	{
		return not(heeftMammaDossier()).or(not(heeftCervixDossier()));
	}
}
