package nl.rivm.screenit.batch.jobs.mamma.kansberekening.dossiers;

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

import lombok.AllArgsConstructor;

import nl.rivm.screenit.batch.jobs.mamma.kansberekening.MammaAbstractEventWriter;
import nl.rivm.screenit.batch.jobs.mamma.kansberekening.MammaKansberekeningConstants;
import nl.rivm.screenit.model.Client_;
import nl.rivm.screenit.model.GbaPersoon_;
import nl.rivm.screenit.model.MergedBrieven_;
import nl.rivm.screenit.model.mamma.MammaBrief;
import nl.rivm.screenit.model.mamma.MammaBrief_;
import nl.rivm.screenit.model.mamma.MammaDossier_;
import nl.rivm.screenit.model.mamma.MammaKansberekeningScreeningRondeEvent;
import nl.rivm.screenit.model.mamma.MammaKansberekeningScreeningRondeEvent_;
import nl.rivm.screenit.model.mamma.MammaMergedBrieven;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde_;
import nl.rivm.screenit.model.mamma.MammaUitnodiging_;
import nl.rivm.screenit.service.mamma.MammaBaseKansberekeningService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.stereotype.Component;

@Component
@AllArgsConstructor
public class MammaScreeningRondeSampleWriter extends MammaAbstractEventWriter<MammaScreeningRonde>
{
	private HibernateService hibernateService;

	private MammaBaseKansberekeningService baseKansberekeningService;

	@Override
	protected void write(MammaScreeningRonde screeningRonde)
	{
		var screeningRondeEvent = screeningRonde.getScreeningRondeEvent();
		if (screeningRondeEvent == null)
		{
			screeningRondeEvent = new MammaKansberekeningScreeningRondeEvent();
			screeningRondeEvent.setScreeningRonde(screeningRonde);
			screeningRonde.setScreeningRondeEvent(screeningRondeEvent);

			hibernateService.saveOrUpdate(screeningRonde);
		}

		baseKansberekeningService.updateScreeningRondeEvent(screeningRonde, true);

		hibernateService.saveOrUpdate(screeningRondeEvent);

		aantalContextOphogen(MammaKansberekeningConstants.SCREENING_RONDE_SAMPLES_KEY);

		baseKansberekeningService.dossierEventHerzien(screeningRonde.getDossier());
	}

	@Override
	protected Consumer<EntityGraph<MammaScreeningRonde>> getEntityGraphFunction()
	{
		return entityGraph ->
		{
			var uitnodigingenSubgraph = entityGraph.addSubgraph(MammaScreeningRonde_.uitnodigingen);
			var briefSubgraph = uitnodigingenSubgraph.addSubgraph(MammaUitnodiging_.BRIEF, MammaBrief.class);
			briefSubgraph.addSubgraph(MammaBrief_.PROJECT_BRIEF);
			briefSubgraph.addSubgraph(MammaBrief_.mergedBrieven, MammaMergedBrieven.class).addSubgraph(MergedBrieven_.MERGED_BRIEVEN);

			var screeningRondeEventSubgraph = entityGraph.addSubgraph(MammaScreeningRonde_.screeningRondeEvent);
			screeningRondeEventSubgraph.addSubgraph(MammaKansberekeningScreeningRondeEvent_.dossier);
			screeningRondeEventSubgraph.addSubgraph(MammaKansberekeningScreeningRondeEvent_.screeningRonde);

			var dossierSubgraph = entityGraph.addSubgraph(MammaScreeningRonde_.dossier);
			var clientSubgraph = dossierSubgraph.addSubgraph(MammaDossier_.client);
			var persoonSubgraph = clientSubgraph.addSubgraph(Client_.persoon);
			persoonSubgraph.addSubgraph(GbaPersoon_.gbaAdres);
			persoonSubgraph.addSubgraph(GbaPersoon_.tijdelijkAdres);
			persoonSubgraph.addSubgraph(GbaPersoon_.tijdelijkGbaAdres);
			persoonSubgraph.addSubgraph(GbaPersoon_.gbaGeboorteLand);
		};
	}
}
