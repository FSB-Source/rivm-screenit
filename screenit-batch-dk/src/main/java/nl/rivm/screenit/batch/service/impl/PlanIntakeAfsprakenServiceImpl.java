package nl.rivm.screenit.batch.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-dk
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.ArrayList;
import java.util.List;

import nl.rivm.screenit.batch.model.ClientAfspraak;
import nl.rivm.screenit.batch.model.IntakeSolution;
import nl.rivm.screenit.batch.service.PlanIntakeAfsprakenService;
import nl.rivm.screenit.model.colon.planning.VrijSlot;

import org.optaplanner.core.api.solver.Solver;
import org.optaplanner.core.api.solver.SolverFactory;
import org.optaplanner.core.config.solver.SolverConfig;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;

@Service
public class PlanIntakeAfsprakenServiceImpl implements PlanIntakeAfsprakenService
{
	private static final Logger LOGGER = LoggerFactory.getLogger(PlanIntakeAfsprakenServiceImpl.class);

	@Autowired
	@Qualifier("maximumSecondsSpend")
	private Long maximumSecondsSpend = 120L;

	@Override
	public List<ClientAfspraak> planIntakeAfspraken(List<ClientAfspraak> clienten, List<VrijSlot> vrijeSloten, StringBuilder planningResultaat)
	{
		return planIntakeAfspraken(clienten, vrijeSloten, planningResultaat, this.maximumSecondsSpend);
	}

	@Override
	public List<ClientAfspraak> planIntakeAfspraken(List<ClientAfspraak> clienten, List<VrijSlot> vrijeSloten, StringBuilder planningResultaat, Long maximumSecondsSpend)
	{
		SolverConfig solverConfig = SolverConfig.createFromXmlResource("screenit-planning-solver-config.xml");
		solverConfig.getTerminationConfig().setSecondsSpentLimit(maximumSecondsSpend);
		SolverFactory<IntakeSolution> solverFactory = SolverFactory.create(solverConfig);
		Solver<IntakeSolution> solver = solverFactory.buildSolver();

		IntakeSolution intakeSolution = new IntakeSolution();

		intakeSolution.setClientAfspraken(clienten);
		intakeSolution.setVrijeSloten(vrijeSloten);
		IntakeSolution bestSolution = solver.solve(intakeSolution);

		LOGGER.trace(bestSolution.toString());
		planningResultaat.append("planner score ").append(bestSolution.getScore());
		List<ClientAfspraak> clientAfspraken = new ArrayList<>();
		if (bestSolution.getScore().getHardScore() == 0)
		{
			clientAfspraken = bestSolution.getClientAfspraken();
		}
		return clientAfspraken;
	}
}
