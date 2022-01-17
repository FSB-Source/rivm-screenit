package nl.rivm.screenit.batch.jobs.mamma.beoordeling.discrepantie.step;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bk
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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
import nl.rivm.screenit.batch.jobs.mamma.beoordeling.discrepantie.MammaDiscrepantieNaarArbitrageListener;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaLezing;
import nl.rivm.screenit.model.mamma.enums.MammaLezingType;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.mamma.MammaBaseBeoordelingService;
import nl.rivm.screenit.util.NaamUtil;

import org.springframework.batch.core.StepExecution;
import org.springframework.beans.factory.annotation.Autowired;

public class MammaDiscrepantieNaarArbitrageWriter extends BaseWriter<MammaBeoordeling>
{
	@Autowired
	private MammaBaseBeoordelingService baseBeoordelingService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Override
	protected void write(MammaBeoordeling beoordeling) throws Exception
	{
		String namen = getRadioloogNamenVoorDoorzettenNaarArbitrage(beoordeling);
		geefNamenDoor(namen);
		hoogAantalBeoordelingenOp();

		zetAutomatischDoorNaarArbitrage(beoordeling);
	}

	private void zetAutomatischDoorNaarArbitrage(MammaBeoordeling beoordeling)
	{
		MammaLezing discrepantieLezing = new MammaLezing();
		discrepantieLezing.setBeoordeling(beoordeling);
		String defaultOpmerkingAutomatischDoorNaarArbitrage = "Discrepantie niet op tijd afgehandeld, daarom automatisch doorgezet naar arbitrage.";
		discrepantieLezing.setBiradsOpmerking(defaultOpmerkingAutomatischDoorNaarArbitrage);
		discrepantieLezing.setBeoordelingDatum(currentDateSupplier.getDate());
		discrepantieLezing.setLezingType(MammaLezingType.DISCREPANTIE_LEZING);
		baseBeoordelingService.discrepantieAfrondenEnNaarArbitrageZetten(beoordeling, discrepantieLezing);
	}

	private String getRadioloogNamenVoorDoorzettenNaarArbitrage(MammaBeoordeling beoordeling)
	{
		InstellingGebruiker radioloog1 = beoordeling.getEersteLezing().getBeoordelaar();
		InstellingGebruiker radioloog2 = beoordeling.getTweedeLezing().getBeoordelaar();

		return NaamUtil.getNaamGebruiker(radioloog1.getMedewerker()) + " & " + NaamUtil.getNaamGebruiker(radioloog2.getMedewerker());
	}

	private void geefNamenDoor(String namen)
	{
		StepExecution stepExecution = getStepExecution();

		Set<String> radiologen = (Set<String>) stepExecution.getJobExecution().getExecutionContext()
			.get(MammaDiscrepantieNaarArbitrageListener.MAMMA_RADIOLOGEN_BEOORDELINGEN_DOORGEZET_NAAR_ARBITRAGE);

		if (radiologen == null)
		{
			radiologen = new HashSet<>();
		}

		radiologen.add(namen);
		getExecutionContext().put(MammaDiscrepantieNaarArbitrageListener.MAMMA_RADIOLOGEN_BEOORDELINGEN_DOORGEZET_NAAR_ARBITRAGE, radiologen);
	}

	private void hoogAantalBeoordelingenOp()
	{
		StepExecution stepExecution = getStepExecution();
		long aantalBeoordelingen = stepExecution.getJobExecution().getExecutionContext()
			.getLong(MammaDiscrepantieNaarArbitrageListener.MAMMA_RADIOLOGEN_BEOORDELINGEN_DOORGEZET_NAAR_ARBITRAGE_AANTAL, 0L);

		getExecutionContext().put(MammaDiscrepantieNaarArbitrageListener.MAMMA_RADIOLOGEN_BEOORDELINGEN_DOORGEZET_NAAR_ARBITRAGE_AANTAL, aantalBeoordelingen + 1);
	}
}
