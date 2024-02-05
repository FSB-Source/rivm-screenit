package nl.rivm.screenit.batch.jobs.colon.gunstigeuitslag.gunstigestep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-dk
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

import lombok.AllArgsConstructor;

import nl.rivm.screenit.batch.jobs.helpers.BaseScrollableResultReader;
import nl.rivm.screenit.dao.colon.impl.ColonRestrictions;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.DossierStatus;
import nl.rivm.screenit.model.colon.ColonBrief;
import nl.rivm.screenit.model.colon.IFOBTType;
import nl.rivm.screenit.model.colon.enums.IFOBTTestStatus;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.query.ScreenitRestrictions;

import org.hibernate.Criteria;
import org.hibernate.FetchMode;
import org.hibernate.HibernateException;
import org.hibernate.StatelessSession;
import org.hibernate.criterion.DetachedCriteria;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.hibernate.criterion.Subqueries;
import org.hibernate.sql.JoinType;
import org.springframework.stereotype.Component;

@Component
@AllArgsConstructor
public class GunstigeUitslagBriefReader extends BaseScrollableResultReader
{
	private final ICurrentDateSupplier currentDateSupplier;

	@Override
	public Criteria createCriteria(StatelessSession session) throws HibernateException
	{
		try
		{
			var criteria = session.createCriteria(Client.class);

			criteria.createAlias("colonDossier", "colonDossier");
			criteria.createAlias("colonDossier.laatsteScreeningRonde", "laatsteScreeningRonde");
			criteria.createAlias("laatsteScreeningRonde.ifobtTesten", "ifobten");
			criteria.createAlias("ifobten.colonUitnodiging", "uitnodiging");
			criteria.createAlias("persoon", "persoon", JoinType.INNER_JOIN);
			criteria.createAlias("persoon.gbaAdres", "adres", JoinType.INNER_JOIN);
			criteria.setFetchMode("persoon", FetchMode.JOIN);

			ScreenitRestrictions.addClientBaseRestrictions(criteria, "", "persoon");

			criteria.add(Restrictions.eq("colonDossier.status", DossierStatus.ACTIEF));

			criteria.add(Restrictions.eq("ifobten.status", IFOBTTestStatus.UITGEVOERD));
			criteria.add(Restrictions.eq("ifobten.type", IFOBTType.GOLD));
			criteria.add(
				Restrictions.or(
					Restrictions.isNull("uitnodiging.uitgesteldeUitslagDatum"),
					Restrictions.le("uitnodiging.uitgesteldeUitslagDatum", currentDateSupplier.getDate())));
			criteria.add(ColonRestrictions.critGunstig("ifobten."));

			var subquery = DetachedCriteria.forClass(ColonBrief.class, "brief");
			subquery.setProjection(Projections.id());
			subquery.add(Restrictions.eqProperty("brief.screeningRonde", "laatsteScreeningRonde.id"));
			subquery.add( 
				Restrictions.or( 
					Restrictions.eq("brief.briefType", BriefType.COLON_UITNODIGING_INTAKE), 
					Restrictions.eq("brief.briefType", BriefType.COLON_GUNSTIGE_UITSLAG) 
				) 
			);
			criteria.add(Subqueries.notExists(subquery));

			return criteria;
		}
		catch (Exception e)
		{
			crashMelding("Cliënten konden niet worden geselecteerd", e);
			throw e;
		}

	}

}
