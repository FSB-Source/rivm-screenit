package nl.rivm.screenit.batch.jobs.colon.selectie.vooraankondigingnavervolgonderzoek;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-dk
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.List;

import lombok.AllArgsConstructor;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.batch.jobs.helpers.BaseScrollableResultReader;
import nl.rivm.screenit.dao.colon.impl.ColonRestrictions;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.colon.enums.ColonUitnodigingsintervalType;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.query.ScreenitRestrictions;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.hibernate.Criteria;
import org.hibernate.HibernateException;
import org.hibernate.StatelessSession;
import org.hibernate.criterion.Restrictions;
import org.hibernate.sql.JoinType;
import org.springframework.stereotype.Component;

@Component
@AllArgsConstructor
public class ColonVooraankondigingNaVervolgonderzoekReader extends BaseScrollableResultReader
{

	private final ICurrentDateSupplier currentDateSupplier;

	private final SimplePreferenceService preferenceService;

	@Override
	public Criteria createCriteria(StatelessSession session) throws HibernateException
	{
		var vandaag = currentDateSupplier.getLocalDate();
		var peildatum = vandaag.plusWeeks(preferenceService.getInteger(PreferenceKey.COLON_VOORAANKONDIGING_NA_VERVOLGONDERZOEK.name()));
		var minLeeftijd = preferenceService.getInteger(PreferenceKey.MINIMALE_LEEFTIJD_COLON.name());
		var maxLeeftijd = preferenceService.getInteger(PreferenceKey.MAXIMALE_LEEFTIJD_COLON.name());

		var crit = session.createCriteria(ColonScreeningRonde.class, "ronde");
		crit.createCriteria("ronde.dossier", "dossier");
		crit.createCriteria("dossier.client", "client");
		crit.createCriteria("client.persoon", "persoon");
		crit.createAlias("dossier.laatsteScreeningRonde", "laatsteScreeningRonde");
		crit.createAlias("dossier.volgendeUitnodiging", "volgendeUitnodiging");
		crit.createAlias("volgendeUitnodiging.interval", "interval");
		crit.createAlias("laatsteScreeningRonde.laatsteAfspraak", "afspraak", JoinType.LEFT_OUTER_JOIN);

		crit.add(ColonRestrictions.getU2BaseCriteria(peildatum, vandaag));
		crit.add(ScreenitRestrictions.getLeeftijdsgrensRestrictions(minLeeftijd, maxLeeftijd, peildatum));
		ColonRestrictions.addNogGeenBriefOntvangenVanTypesCriteria(crit, "ronde", List.of(BriefType.COLON_VOORAANKONDIGING_NA_VERVOLGONDERZOEK));
		ScreenitRestrictions.addClientBaseRestrictions(crit, "client", "persoon");
		crit.add(Restrictions.in("interval.type",
			List.of(ColonUitnodigingsintervalType.INTAKE_COLOSCOPIE_GEPLAND,
				ColonUitnodigingsintervalType.INTAKE_CT_COLOGRAFIE,
				ColonUitnodigingsintervalType.INTAKE_GEEN_VERVOLGBELEID_3_JAAR_TERUG_NAAR_SCREENING,
				ColonUitnodigingsintervalType.INTAKE_GEEN_VERVOLGBELEID_4_JAAR_TERUG_NAAR_SCREENING,
				ColonUitnodigingsintervalType.INTAKE_GEEN_VERVOLGBELEID_5_JAAR_TERUG_NAAR_SCREENING,
				ColonUitnodigingsintervalType.INTAKE_GEEN_VERVOLGBELEID_6_JAAR_TERUG_NAAR_SCREENING,
				ColonUitnodigingsintervalType.INTAKE_GEEN_VERVOLGBELEID_7_JAAR_TERUG_NAAR_SCREENING,
				ColonUitnodigingsintervalType.INTAKE_GEEN_VERVOLGBELEID_8_JAAR_TERUG_NAAR_SCREENING,
				ColonUitnodigingsintervalType.INTAKE_GEEN_VERVOLGBELEID_9_JAAR_TERUG_NAAR_SCREENING,
				ColonUitnodigingsintervalType.INTAKE_GEEN_VERVOLGBELEID_10_JAAR_TERUG_NAAR_SCREENING,
				ColonUitnodigingsintervalType.ENDOSCOPIEVERSLAG_VERWIJZING_POLIKLINIEK,
				ColonUitnodigingsintervalType.ENDOSCOPIEVERSLAG_VERVOLGSCOPIE,
				ColonUitnodigingsintervalType.ENDOSCOPIEVERSLAG_SCOPIE_BEOORDELING_RADICALITEIT,
				ColonUitnodigingsintervalType.ENDOSCOPIEVERSLAG_NIEUWE_SCOPIE,
				ColonUitnodigingsintervalType.ENDOSCOPIEVERSLAG_SURVEILLANCE_1_JAAR,
				ColonUitnodigingsintervalType.ENDOSCOPIEVERSLAG_SURVEILLANCE_3_JAAR,
				ColonUitnodigingsintervalType.ENDOSCOPIEVERSLAG_SURVEILLANCE_5_JAAR,
				ColonUitnodigingsintervalType.ENDOSCOPIEVERSLAG_TERUG_BVO,
				ColonUitnodigingsintervalType.ENDOSCOPIEVERSLAG_CT_COLOGRAFIE,
				ColonUitnodigingsintervalType.ENDOSCOPIEVERSLAG_GEEN_SURVEILLANCE)));
		crit.add(Restrictions.isNotNull("interval.aantal"));
		return crit;
	}
}
