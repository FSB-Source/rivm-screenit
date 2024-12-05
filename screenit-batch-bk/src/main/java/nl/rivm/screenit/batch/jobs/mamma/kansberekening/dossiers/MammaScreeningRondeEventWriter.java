package nl.rivm.screenit.batch.jobs.mamma.kansberekening.dossiers;

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

import java.util.function.Consumer;

import javax.persistence.EntityGraph;

import lombok.AllArgsConstructor;

import nl.rivm.screenit.batch.jobs.mamma.kansberekening.MammaAbstractEventWriter;
import nl.rivm.screenit.batch.jobs.mamma.kansberekening.MammaKansberekeningConstants;
import nl.rivm.screenit.model.Client_;
import nl.rivm.screenit.model.GbaPersoon_;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaDossier_;
import nl.rivm.screenit.model.mamma.MammaKansberekeningScreeningRondeEvent_;
import nl.rivm.screenit.service.mamma.MammaBaseKansberekeningService;

import org.springframework.stereotype.Component;

@Component
@AllArgsConstructor
public class MammaScreeningRondeEventWriter extends MammaAbstractEventWriter<MammaDossier>
{
	private final MammaBaseKansberekeningService baseKansberekeningService;

	@Override
	protected void write(MammaDossier dossier)
	{
		baseKansberekeningService.maakDossierEvent(dossier);
		aantalContextOphogen(MammaKansberekeningConstants.SCREENING_RONDE_EVENTS_KEY);
	}

	@Override
	protected Consumer<EntityGraph<MammaDossier>> getEntityGraphFunction()
	{
		return entityGraph ->
		{
			var clientSubgraph = entityGraph.addSubgraph(MammaDossier_.client);

			var persoonSubgraph = clientSubgraph.addSubgraph(Client_.persoon);
			persoonSubgraph.addSubgraph(GbaPersoon_.gbaAdres);
			persoonSubgraph.addSubgraph(GbaPersoon_.tijdelijkAdres);
			persoonSubgraph.addSubgraph(GbaPersoon_.tijdelijkGbaAdres);
			persoonSubgraph.addSubgraph(GbaPersoon_.gbaGeboorteLand);

			var screeningRondeEventSubgraph = entityGraph.addSubgraph(MammaDossier_.screeningRondeEvent);
			screeningRondeEventSubgraph.addSubgraph(MammaKansberekeningScreeningRondeEvent_.dossier);
			screeningRondeEventSubgraph.addSubgraph(MammaKansberekeningScreeningRondeEvent_.screeningRonde);
		};
	}
}
