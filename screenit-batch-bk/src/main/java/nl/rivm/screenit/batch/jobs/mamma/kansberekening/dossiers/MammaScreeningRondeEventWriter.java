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

import nl.rivm.screenit.batch.jobs.mamma.kansberekening.MammaAbstractEventWriter;
import nl.rivm.screenit.batch.jobs.mamma.kansberekening.MammaKansberekeningConstants;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaKansberekeningScreeningRondeEvent;
import nl.rivm.screenit.service.mamma.MammaBaseKansberekeningService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.hibernate.Criteria;
import org.hibernate.FetchMode;
import org.hibernate.FlushMode;
import org.hibernate.Session;
import org.hibernate.sql.JoinType;
import org.springframework.beans.factory.annotation.Autowired;

public class MammaScreeningRondeEventWriter extends MammaAbstractEventWriter<MammaDossier>
{
	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private MammaBaseKansberekeningService baseKansberekeningService;

	@Override
	protected void write(MammaDossier dossier)
	{
		baseKansberekeningService.maakDossierEvent(dossier);
		aantalContextOphogen(MammaKansberekeningConstants.SCREENING_RONDE_EVENTS_KEY);
	}

	@Override
	protected Criteria getCriteria(Session session)
	{
		Criteria criteria = session.createCriteria(MammaDossier.class, "dossier");
		criteria.createAlias("dossier.client", "client");
		criteria.createAlias("client.persoon", "persoon");
		criteria.createAlias("dossier.screeningRondeEvent", "screeningRondeEvent", JoinType.LEFT_OUTER_JOIN);

		criteria.setFetchMode("client", FetchMode.JOIN);
		criteria.setFetchMode("persoon", FetchMode.JOIN);
		criteria.setFetchMode("screeningRondeEvent", FetchMode.JOIN);

		criteria.createAlias("persoon.gbaAdres", "gbaAdres", JoinType.LEFT_OUTER_JOIN);
		criteria.createAlias("persoon.tijdelijkAdres", "tijdelijkAdres", JoinType.LEFT_OUTER_JOIN);
		criteria.createAlias("persoon.tijdelijkGbaAdres", "tijdelijkGbaAdres", JoinType.LEFT_OUTER_JOIN);
		criteria.createAlias("persoon.gbaGeboorteLand", "gbaGeboorteLand", JoinType.LEFT_OUTER_JOIN);
		criteria.createAlias("screeningRondeEvent.dossier", "sreDossier", JoinType.LEFT_OUTER_JOIN);
		criteria.createAlias("screeningRondeEvent.screeningRonde", "sreScreeningRonde", JoinType.LEFT_OUTER_JOIN);

		criteria.setFetchMode("gbaAdres", FetchMode.SELECT);
		criteria.setFetchMode("tijdelijkAdres", FetchMode.SELECT);
		criteria.setFetchMode("tijdelijkGbaAdres", FetchMode.SELECT);
		criteria.setFetchMode("gbaGeboorteLand", FetchMode.SELECT);
		criteria.setFetchMode("sreDossier", FetchMode.SELECT);
		criteria.setFetchMode("sreScreeningRonde", FetchMode.SELECT);

		return criteria;
	}
}
