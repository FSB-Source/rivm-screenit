package nl.rivm.screenit.batch.dao.impl;

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

import java.util.Date;
import java.util.List;

import nl.rivm.screenit.batch.dao.IntakeAfspraakDao;
import nl.rivm.screenit.dao.colon.impl.ColonRestrictions;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.DossierStatus;
import nl.rivm.screenit.model.ScreeningRondeStatus;
import nl.rivm.screenit.model.colon.enums.IFOBTTestStatus;
import nl.rivm.screenit.model.colon.planning.AfspraakStatus;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.GbaStatus;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.hibernate.Criteria;
import org.hibernate.criterion.Disjunction;
import org.hibernate.criterion.ProjectionList;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.hibernate.criterion.Subqueries;
import org.hibernate.sql.JoinType;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

@Repository
public class IntakeAfspraakDaoImpl extends AbstractAutowiredDao implements IntakeAfspraakDao
{

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Override
	public List<Object> getClientenVoorIntakeAfspraakMaken(Integer uitnodigingsInterval)
	{

		Date uitnodigingsIntervalVerlopen = DateUtil.toUtilDate(currentDateSupplier.getLocalDate().minusDays(uitnodigingsInterval));

		Criteria criteria = getSession().createCriteria(Client.class, "main");
		criteria.createAlias("persoon", "persoon");
		criteria.createAlias("persoon.gbaAdres", "gbaAdres");
		criteria.createAlias("gbaAdres.postcodeCoordinaten", "gbaPostcodeCoordinaten", JoinType.LEFT_OUTER_JOIN);
		criteria.createAlias("gbaAdres.gbaGemeente", "gemeente", JoinType.LEFT_OUTER_JOIN);
		criteria.createAlias("colonDossier", "dossier");
		criteria.createAlias("dossier.laatsteScreeningRonde", "laatsteScreeningRonde");
		criteria.createAlias("laatsteScreeningRonde.ifobtTesten", "testen");
		criteria.createAlias("laatsteScreeningRonde.laatsteAfspraak", "intake", JoinType.LEFT_OUTER_JOIN);
		criteria.createAlias("intake.conclusie", "conclusie", JoinType.LEFT_OUTER_JOIN);

		criteria.add(Restrictions.eq("dossier.status", DossierStatus.ACTIEF));
		criteria.add(Restrictions.isNull("persoon.overlijdensdatum"));
		criteria.add(Restrictions.ne("gbaStatus", GbaStatus.AFGEVOERD));
		criteria.add(Restrictions.eq("laatsteScreeningRonde.status", ScreeningRondeStatus.LOPEND));
		criteria.add(Restrictions.or(Restrictions.eq("testen.status", IFOBTTestStatus.UITGEVOERD), Restrictions.eq("testen.status", IFOBTTestStatus.DOETNIETMEE)));
		criteria.add(ColonRestrictions.critOngunstig("testen."));
		ColonRestrictions.addNogGeenUitslagbriefOntvangenCriteria(criteria, "laatsteScreeningRonde", BriefType.COLON_BRIEVEN_GEEN_INTAKE_NODIG);

		criteria.add(Subqueries.propertyNotIn("laatsteScreeningRonde.id", ColonRestrictions.critAfsprakenZonderVervolg(uitnodigingsIntervalVerlopen)));

		Disjunction disjunction = Restrictions.disjunction();

		disjunction.add(Restrictions.isNull("intake.status"));
		disjunction.add( 
			Restrictions.and( 
				Restrictions.ne("intake.status", AfspraakStatus.GEPLAND), 
				Restrictions.isNull("conclusie.type") 
			) 
		); 
		criteria.add(disjunction);

		ProjectionList projectionList = Projections.projectionList() 
			.add(Projections.property("id")) 
			.add(Projections.property("laatsteScreeningRonde.id")) 
			.add(Projections.property("testen.analyseDatum")) 
			.add(Projections.property("gbaPostcodeCoordinaten.latitude")) 
			.add(Projections.property("gbaPostcodeCoordinaten.longitude")) 
			.add(Projections.property("gemeente.latitude")) 
			.add(Projections.property("gemeente.longitude")) 
			.add(Projections.property("gemeente.naam")) 
			.add(Projections.property("gemeente.screeningOrganisatie.id")) 
			.add(Projections.property("intake.id")) 
			.add(Projections.property("persoon.bsn"))
			.add(Projections.property("persoon.geboortedatum"));

		criteria.setProjection(Projections.distinct(projectionList));

		return criteria.list();
	}
}
