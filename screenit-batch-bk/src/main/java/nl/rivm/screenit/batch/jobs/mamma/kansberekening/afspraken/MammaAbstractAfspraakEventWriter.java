package nl.rivm.screenit.batch.jobs.mamma.kansberekening.afspraken;

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

import nl.rivm.screenit.batch.jobs.mamma.kansberekening.MammaAbstractEventWriter;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaKansberekeningAfspraakEvent;
import nl.rivm.screenit.service.mamma.MammaBaseKansberekeningService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.hibernate.Criteria;
import org.hibernate.FetchMode;
import org.hibernate.Session;
import org.hibernate.sql.JoinType;
import org.springframework.beans.factory.annotation.Autowired;

public abstract class MammaAbstractAfspraakEventWriter extends MammaAbstractEventWriter<MammaAfspraak>
{
	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private MammaBaseKansberekeningService baseKansberekeningService;

	@Override
	protected void write(MammaAfspraak afspraak)
	{
		var afspraakEvent = afspraak.getAfspraakEvent();
		if (afspraakEvent == null)
		{
			afspraakEvent = new MammaKansberekeningAfspraakEvent();
			afspraakEvent.setAfspraak(afspraak);
			afspraak.setAfspraakEvent(afspraakEvent);

			hibernateService.saveOrUpdate(afspraak);
		}

		baseKansberekeningService.updateAfspraakEvent(afspraak, zetOpkomst());

		hibernateService.saveOrUpdate(afspraakEvent);

		aantalContextOphogen(getContextKey());

		if (zetOpkomst())
		{
			baseKansberekeningService.dossierEventHerzien(afspraak.getUitnodiging().getScreeningRonde().getDossier());
		}
	}

	protected abstract boolean zetOpkomst();

	protected abstract String getContextKey();

	@Override
	protected Criteria getCriteria(Session session)
	{
		var criteria = session.createCriteria(MammaAfspraak.class, "afspraak");
		criteria.createAlias("afspraak.uitnodiging", "uitnodiging");
		criteria.createAlias("uitnodiging.brief", "brief");
		criteria.createAlias("uitnodiging.screeningRonde", "screeningRonde");
		criteria.createAlias("screeningRonde.dossier", "dossier");
		criteria.createAlias("dossier.client", "client");
		criteria.createAlias("client.persoon", "persoon");
		criteria.createAlias("afspraak.afspraakEvent", "afspraakEvent", JoinType.LEFT_OUTER_JOIN);
		criteria.createAlias("screeningRonde.laatsteAfmelding", "laatsteAfmelding", JoinType.LEFT_OUTER_JOIN);

		criteria.setFetchMode("uitnodiging", FetchMode.JOIN);
		criteria.setFetchMode("brief", FetchMode.JOIN);
		criteria.setFetchMode("screeningRonde", FetchMode.JOIN);
		criteria.setFetchMode("dossier", FetchMode.JOIN);
		criteria.setFetchMode("client", FetchMode.JOIN);
		criteria.setFetchMode("persoon", FetchMode.JOIN);
		criteria.setFetchMode("afspraakEvent", FetchMode.JOIN);
		criteria.setFetchMode("laatsteAfmelding", FetchMode.JOIN);

		criteria.createAlias("brief.projectBrief", "projectBrief", JoinType.LEFT_OUTER_JOIN);
		criteria.createAlias("persoon.gbaAdres", "gbaAdres", JoinType.LEFT_OUTER_JOIN);
		criteria.createAlias("persoon.tijdelijkAdres", "tijdelijkAdres", JoinType.LEFT_OUTER_JOIN);
		criteria.createAlias("persoon.tijdelijkGbaAdres", "tijdelijkGbaAdres", JoinType.LEFT_OUTER_JOIN);
		criteria.createAlias("persoon.gbaGeboorteLand", "gbaGeboorteLand", JoinType.LEFT_OUTER_JOIN);
		criteria.createAlias("afspraakEvent.afspraak", "aeAfspraak", JoinType.LEFT_OUTER_JOIN);
		criteria.createAlias("brief.mergedBrieven", "mergedBrieven", JoinType.LEFT_OUTER_JOIN);
		criteria.createAlias("mergedBrieven.mergedBrieven", "uploadDocument", JoinType.LEFT_OUTER_JOIN);

		criteria.setFetchMode("projectBrief", FetchMode.SELECT);
		criteria.setFetchMode("gbaAdres", FetchMode.SELECT);
		criteria.setFetchMode("tijdelijkAdres", FetchMode.SELECT);
		criteria.setFetchMode("tijdelijkGbaAdres", FetchMode.SELECT);
		criteria.setFetchMode("gbaGeboorteLand", FetchMode.SELECT);
		criteria.setFetchMode("aeAfspraak", FetchMode.SELECT);
		criteria.setFetchMode("mergedBrieven", FetchMode.SELECT);
		criteria.setFetchMode("uploadDocument", FetchMode.SELECT);

		return criteria;
	}

}
