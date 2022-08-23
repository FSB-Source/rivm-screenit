package nl.rivm.screenit.batch.jobs.mamma.kansberekening.dossiers;

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

import lombok.AllArgsConstructor;

import nl.rivm.screenit.batch.jobs.mamma.kansberekening.MammaAbstractEventWriter;
import nl.rivm.screenit.batch.jobs.mamma.kansberekening.MammaKansberekeningConstants;
import nl.rivm.screenit.model.mamma.MammaKansberekeningScreeningRondeEvent;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.service.mamma.MammaBaseKansberekeningService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.hibernate.Criteria;
import org.hibernate.FetchMode;
import org.hibernate.Session;
import org.hibernate.sql.JoinType;
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
	protected Criteria getCriteria(Session session)
	{
		var criteria = session.createCriteria(MammaScreeningRonde.class, "screeningRonde");
		criteria.createAlias("screeningRonde.uitnodigingen", "uitnodiging");
		criteria.createAlias("uitnodiging.brief", "brief");
		criteria.createAlias("screeningRonde.dossier", "dossier");
		criteria.createAlias("dossier.client", "client");
		criteria.createAlias("client.persoon", "persoon");
		criteria.createAlias("screeningRonde.screeningRondeEvent", "screeningRondeEvent", JoinType.LEFT_OUTER_JOIN);

		criteria.setFetchMode("uitnodiging", FetchMode.JOIN);
		criteria.setFetchMode("brief", FetchMode.JOIN);
		criteria.setFetchMode("dossier", FetchMode.JOIN);
		criteria.setFetchMode("client", FetchMode.JOIN);
		criteria.setFetchMode("persoon", FetchMode.JOIN);
		criteria.setFetchMode("screeningRondeEvent", FetchMode.JOIN);

		criteria.createAlias("brief.projectBrief", "projectBrief", JoinType.LEFT_OUTER_JOIN);
		criteria.createAlias("persoon.gbaAdres", "gbaAdres", JoinType.LEFT_OUTER_JOIN);
		criteria.createAlias("persoon.tijdelijkAdres", "tijdelijkAdres", JoinType.LEFT_OUTER_JOIN);
		criteria.createAlias("persoon.tijdelijkGbaAdres", "tijdelijkGbaAdres", JoinType.LEFT_OUTER_JOIN);
		criteria.createAlias("persoon.gbaGeboorteLand", "gbaGeboorteLand", JoinType.LEFT_OUTER_JOIN);
		criteria.createAlias("screeningRondeEvent.dossier", "sreDossier", JoinType.LEFT_OUTER_JOIN);
		criteria.createAlias("screeningRondeEvent.screeningRonde", "sreScreeningRonde", JoinType.LEFT_OUTER_JOIN);
		criteria.createAlias("brief.mergedBrieven", "mergedBrieven", JoinType.LEFT_OUTER_JOIN);
		criteria.createAlias("mergedBrieven.mergedBrieven", "uploadDocument", JoinType.LEFT_OUTER_JOIN);

		criteria.setFetchMode("projectBrief", FetchMode.SELECT);
		criteria.setFetchMode("gbaAdres", FetchMode.SELECT);
		criteria.setFetchMode("tijdelijkAdres", FetchMode.SELECT);
		criteria.setFetchMode("tijdelijkGbaAdres", FetchMode.SELECT);
		criteria.setFetchMode("gbaGeboorteLand", FetchMode.SELECT);
		criteria.setFetchMode("sreDossier", FetchMode.SELECT);
		criteria.setFetchMode("sreScreeningRonde", FetchMode.SELECT);
		criteria.setFetchMode("mergedBrieven", FetchMode.SELECT);
		criteria.setFetchMode("uploadDocument", FetchMode.SELECT);

		return criteria;
	}
}
