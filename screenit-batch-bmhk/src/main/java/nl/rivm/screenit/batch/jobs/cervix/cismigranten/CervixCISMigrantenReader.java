package nl.rivm.screenit.batch.jobs.cervix.cismigranten;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
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

import java.util.function.Function;

import javax.persistence.criteria.From;
import javax.persistence.criteria.JoinType;

import lombok.AllArgsConstructor;

import nl.rivm.screenit.batch.jobs.helpers.BaseSpecificationScrollableResultReader;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Client_;
import nl.rivm.screenit.model.cervix.CervixDossier;
import nl.rivm.screenit.model.cervix.CervixDossier_;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde;
import nl.rivm.screenit.model.cervix.cis.CervixCISHistorie;
import nl.rivm.screenit.model.project.Project;
import nl.rivm.screenit.model.project.ProjectClient;
import nl.rivm.screenit.model.project.ProjectClient_;
import nl.rivm.screenit.service.ICurrentDateSupplier;

import org.jetbrains.annotations.NotNull;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.algemeen.ClientSpecification.heeftActieveClient;
import static nl.rivm.screenit.specification.algemeen.ProjectClientSpecification.heeftActieveProjectGroep;
import static nl.rivm.screenit.specification.algemeen.ProjectClientSpecification.isProjectClientActief;
import static nl.rivm.screenit.specification.algemeen.ProjectSpecification.heeftNaam;
import static nl.rivm.screenit.specification.algemeen.ProjectSpecification.heeftStartdatumVanaf;
import static nl.rivm.screenit.specification.cervix.CervixCISHistorieSpecification.heeftGeenScreeningRonde;
import static nl.rivm.screenit.specification.cervix.CervixCISHistorieSpecification.heeftGeenUitstel;
import static nl.rivm.screenit.specification.cervix.CervixDossierSpecification.heeftGeenScreeningRondeInCISHistorie;
import static nl.rivm.screenit.specification.cervix.CervixScreeningRondeSpecification.heeftGeenLaatsteUitnodiging;

@Component
@AllArgsConstructor
public class CervixCISMigrantenReader extends BaseSpecificationScrollableResultReader<Client>
{
	private final ICurrentDateSupplier dateSupplier;

	@Override
	protected Specification<Client> createSpecification()
	{
		return isProjectClientActief(true).with(projectClientJoin())
			.and(heeftActieveProjectGroep().with(projectClientJoin()))
			.and(heeftStartdatumVanaf(dateSupplier.getDate()).with(projectJoin()))
			.and(heeftNaam("CIS-Migranten").with(projectJoin()))
			.and(heeftActieveClient())
			.and(heeftGeenUitstel().with(cisHistorieJoin()))
			.and(heeftGeenScreeningRonde().with(
				cisHistorieJoin()).or(heeftGeenScreeningRondeInCISHistorie().with(cervixDossierJoin())))
			.and(heeftGeenLaatsteUitnodiging().with(screeningRondeJoin()));
	}

	@NotNull
	private static Function<From<?, ? extends Client>, From<?, ? extends CervixDossier>> cervixDossierJoin()
	{
		return q -> join(q, Client_.cervixDossier, JoinType.LEFT);
	}

	@NotNull
	private static Function<From<?, ? extends Client>, From<?, ? extends ProjectClient>> projectClientJoin()
	{
		return q -> join(q, Client_.projecten);
	}

	@NotNull
	private static Function<From<?, ? extends Client>, From<?, ? extends Project>> projectJoin()
	{
		return q ->
		{
			var projectClientJoin = projectClientJoin().apply(q);
			return join(projectClientJoin, ProjectClient_.project);
		};
	}

	@NotNull
	private static Function<From<?, ? extends Client>, From<?, ? extends CervixCISHistorie>> cisHistorieJoin()
	{
		return q ->
		{
			var dossierJoin = cervixDossierJoin().apply(q);
			return join(dossierJoin, CervixDossier_.cisHistorie, JoinType.LEFT);
		};
	}

	@NotNull
	private static Function<From<?, ? extends Client>, From<?, ? extends CervixScreeningRonde>> screeningRondeJoin()
	{
		return q ->
		{
			var screeningRondeJoin = cervixDossierJoin().apply(q);
			return join(screeningRondeJoin, CervixDossier_.laatsteScreeningRonde, JoinType.LEFT);
		};
	}
}
