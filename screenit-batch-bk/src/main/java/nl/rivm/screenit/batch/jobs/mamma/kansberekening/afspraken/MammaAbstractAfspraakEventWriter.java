package nl.rivm.screenit.batch.jobs.mamma.kansberekening.afspraken;

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

import java.util.function.Consumer;

import javax.persistence.EntityGraph;

import nl.rivm.screenit.batch.jobs.mamma.kansberekening.MammaAbstractEventWriter;
import nl.rivm.screenit.model.Client_;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaAfspraak_;
import nl.rivm.screenit.model.mamma.MammaBrief_;
import nl.rivm.screenit.model.mamma.MammaDossier_;
import nl.rivm.screenit.model.mamma.MammaKansberekeningAfspraakEvent;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde_;
import nl.rivm.screenit.model.mamma.MammaUitnodiging_;
import nl.rivm.screenit.repository.mamma.MammaKansberekeningAfspraakEventRepository;
import nl.rivm.screenit.service.mamma.MammaBaseKansberekeningService;

import org.springframework.beans.factory.annotation.Autowired;

public abstract class MammaAbstractAfspraakEventWriter extends MammaAbstractEventWriter<MammaAfspraak>
{
	@Autowired
	private MammaBaseKansberekeningService baseKansberekeningService;

	@Autowired
	private MammaKansberekeningAfspraakEventRepository afspraakEventRepository;

	@Override
	protected void write(MammaAfspraak afspraak)
	{
		var afspraakEvent = afspraak.getAfspraakEvent();
		if (afspraakEvent == null)
		{
			afspraakEvent = new MammaKansberekeningAfspraakEvent();
			afspraakEvent.setAfspraak(afspraak);
			afspraak.setAfspraakEvent(afspraakEvent);
			afspraakEventRepository.save(afspraakEvent);
		}

		baseKansberekeningService.updateAfspraakEvent(afspraak, zetOpkomst());
		afspraakEventRepository.save(afspraakEvent);

		aantalContextOphogen(getContextKey());

		if (zetOpkomst())
		{
			baseKansberekeningService.dossierEventHerzien(afspraak.getUitnodiging().getScreeningRonde().getDossier());
		}
	}

	protected abstract boolean zetOpkomst();

	protected abstract String getContextKey();

	@Override
	protected Consumer<EntityGraph<MammaAfspraak>> getEntityGraphFunction()
	{
		return entityGraph ->
		{
			entityGraph.addSubgraph(MammaAfspraak_.afspraakEvent);
			entityGraph.addSubgraph(MammaAfspraak_.opkomstkans);

			var uitnodigingSubgraph = entityGraph.addSubgraph(MammaAfspraak_.uitnodiging);
			uitnodigingSubgraph.addSubgraph(MammaUitnodiging_.brief).addSubgraph(MammaBrief_.PROJECT_BRIEF);

			var screeningsRondeSubgraph = uitnodigingSubgraph.addSubgraph(MammaUitnodiging_.screeningRonde);
			screeningsRondeSubgraph.addSubgraph(MammaScreeningRonde_.laatsteAfmelding);

			var dossierSubgraph = screeningsRondeSubgraph.addSubgraph(MammaScreeningRonde_.dossier);
			dossierSubgraph.addSubgraph(MammaDossier_.deelnamekans);
			dossierSubgraph.addSubgraph(MammaDossier_.client).addSubgraph(Client_.persoon);
		};
	}
}
