package nl.rivm.screenit.batch.jobs.mamma.beoordeling.status.step;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bk
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

import java.util.HashSet;
import java.util.Set;

import nl.rivm.screenit.batch.jobs.helpers.BaseWriter;
import nl.rivm.screenit.batch.jobs.mamma.beoordeling.status.MammaBeoordelingenAccorderenListener;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus;
import nl.rivm.screenit.service.mamma.MammaBaseBeoordelingService;
import nl.rivm.screenit.util.NaamUtil;

import org.springframework.batch.core.StepExecution;
import org.springframework.beans.factory.annotation.Autowired;

public class MammaBeoordelingenAccorderenWriter extends BaseWriter<MammaBeoordeling>
{

	@Autowired
	private MammaBaseBeoordelingService baseBeoordelingService;

	@Override
	protected void write(MammaBeoordeling beoordeling) throws Exception
	{
		String naam = getRadioloogNaamVoorAccorderen(beoordeling);
		geefNaamDoor(naam);
		hoogAantalBeoordelingenOp();

		baseBeoordelingService.bevestigLezing(beoordeling);
	}

	private String getRadioloogNaamVoorAccorderen(MammaBeoordeling beoordeling)
	{
		InstellingGebruiker radioloog;
		if (MammaBeoordelingStatus.EERSTE_LEZING_OPGESLAGEN.equals(beoordeling.getStatus()))
		{
			radioloog = beoordeling.getEersteLezing().getBeoordelaar();
		}
		else 
		{
			radioloog = beoordeling.getTweedeLezing().getBeoordelaar();
		}

		return NaamUtil.getNaamGebruiker(radioloog.getMedewerker());
	}

	private void geefNaamDoor(String naam)
	{
		StepExecution stepExecution = getStepExecution();

		Set<String> radiologen = (Set<String>) stepExecution.getJobExecution().getExecutionContext()
			.get(MammaBeoordelingenAccorderenListener.MAMMA_RADIOLOGEN_BEOORDELINGEN_GEACCORDEERD);

		if (radiologen == null)
		{
			radiologen = new HashSet<>();
		}

		radiologen.add(naam);
		getExecutionContext().put(MammaBeoordelingenAccorderenListener.MAMMA_RADIOLOGEN_BEOORDELINGEN_GEACCORDEERD, radiologen);
	}

	private void hoogAantalBeoordelingenOp()
	{
		StepExecution stepExecution = getStepExecution();
		long aantalBeoordelingen = stepExecution.getJobExecution().getExecutionContext()
			.getLong(MammaBeoordelingenAccorderenListener.MAMMA_RADIOLOGEN_BEOORDELINGEN_GEACCORDEERD_AANTAL, 0L);

		getExecutionContext().put(MammaBeoordelingenAccorderenListener.MAMMA_RADIOLOGEN_BEOORDELINGEN_GEACCORDEERD_AANTAL, aantalBeoordelingen + 1);
	}
}
