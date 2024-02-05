package nl.rivm.screenit.batch.jobs.colon.vervolgintakeconclusie.briefandereintakelocatie;

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

import java.time.LocalDate;
import java.util.Date;

import lombok.AllArgsConstructor;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.batch.jobs.helpers.BaseScrollableResultReader;
import nl.rivm.screenit.dao.colon.impl.ColonRestrictions;
import nl.rivm.screenit.model.DossierStatus;
import nl.rivm.screenit.model.ScreeningRondeStatus;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.colon.enums.ColonConclusieType;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.query.ScreenitRestrictions;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.hibernate.Criteria;
import org.hibernate.HibernateException;
import org.hibernate.StatelessSession;
import org.hibernate.criterion.DetachedCriteria;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.hibernate.criterion.Subqueries;
import org.springframework.stereotype.Component;

@Component
@AllArgsConstructor
public class HerinneringClientWilAnderIntakeLocatieBriefReader extends BaseScrollableResultReader
{

	private final ICurrentDateSupplier currentDateSupplier;

	private final SimplePreferenceService preferenceService;

	@Override
	public Criteria createCriteria(StatelessSession session) throws HibernateException
	{
		var nu = currentDateSupplier.getLocalDateTime();
		Integer uitnodigingsInterval = preferenceService.getInteger(PreferenceKey.UITNODIGINGSINTERVAL.name());
		if (uitnodigingsInterval == null)
		{
			throw new IllegalStateException("Spreidingsperiode op de parameterisatie pagina is niet gezet");
		}
		var conclusieMoetGegevenZijnOp = getHerinneringClientWilAnderIntakeAfspraakDate();
		var uitnodigingsIntervalVerlopen = DateUtil.toUtilDate(nu.minusDays(uitnodigingsInterval).plusWeeks(2));

		var criteria = session.createCriteria(ColonScreeningRonde.class);
		criteria.createAlias("dossier", "dossier");
		criteria.createAlias("dossier.client", "client");
		criteria.createAlias("client.persoon", "persoon");

		ScreenitRestrictions.addClientBaseRestrictions(criteria, "client", "persoon");

		criteria.createAlias("laatsteAfspraak", "afspraak");
		criteria.createAlias("afspraak.conclusie", "conclusie");
		criteria.add(Restrictions.eq("status", ScreeningRondeStatus.LOPEND));
		criteria.add(Restrictions.eq("dossier.status", DossierStatus.ACTIEF));

		criteria.add(Restrictions.eq("conclusie.type", ColonConclusieType.CLIENT_WIL_ANDERE_INTAKELOKATIE));
		criteria.add(Restrictions.le("conclusie.datum", conclusieMoetGegevenZijnOp));

		var rondesMetHerinneringBrief = DetachedCriteria.forClass(ColonScreeningRonde.class);
		rondesMetHerinneringBrief.createAlias("brieven", "brief");
		rondesMetHerinneringBrief.add(Restrictions.eq("brief.briefType", BriefType.COLON_HERINNERING_ANDERE_INTAKELOCATIE));
		rondesMetHerinneringBrief.setProjection(Projections.id());

		criteria.add(Subqueries.propertyNotIn("id", rondesMetHerinneringBrief));
		criteria.add(Subqueries.propertyNotIn("id", ColonRestrictions.critAfsprakenZonderVervolg(uitnodigingsIntervalVerlopen)));
		criteria.add(Subqueries.propertyNotIn("id", ColonRestrictions.critOpenUitnodigingNa2jaar(uitnodigingsIntervalVerlopen)));
		ColonRestrictions.addHeeftGeenAfgerondeVerlagenRestrictions(criteria, "");

		return criteria;
	}

	private Date getHerinneringClientWilAnderIntakeAfspraakDate()
	{
		LocalDate vandaag = currentDateSupplier.getLocalDate();
		return DateUtil.toUtilDate(vandaag.minusWeeks(6));
	}

}
