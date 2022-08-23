package nl.rivm.screenit.batch.jobs.mamma.beoordeling.ilm.step.beelden.gunstig;

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

import java.util.Arrays;

import nl.rivm.screenit.batch.jobs.helpers.BaseScrollableResultReader;
import nl.rivm.screenit.model.ScreeningRondeStatus;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus;
import nl.rivm.screenit.model.mamma.enums.MammaFollowUpConclusieStatus;
import nl.rivm.screenit.model.mamma.enums.MammaMammografieIlmStatus;

import org.hibernate.Criteria;
import org.hibernate.HibernateException;
import org.hibernate.StatelessSession;
import org.hibernate.criterion.DetachedCriteria;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Property;
import org.hibernate.criterion.Restrictions;
import org.hibernate.criterion.Subqueries;
import org.springframework.stereotype.Component;

import static nl.rivm.screenit.batch.jobs.mamma.beoordeling.ilm.step.beelden.gunstig.MammaBeeldenGunstigWriter.MINIMUM_AANTAL_GUNSTIGE_AFGESLOTEN_RONDES_MET_BEELDEN;

@Component
public class MammaBeeldenGunstigReader extends BaseScrollableResultReader
{
	private static final int MAX_AANTAL_DOSSIERS = 100000;

	@Override
	public Criteria createCriteria(StatelessSession session) throws HibernateException
	{
		var criteria = session.createCriteria(MammaDossier.class, "mammadossier");

		var afgeslotenRondesGunstigEnMetBeeldenSubquery = DetachedCriteria.forClass(MammaScreeningRonde.class, "rondes");
		afgeslotenRondesGunstigEnMetBeeldenSubquery.add(Property.forName("mammadossier.id").eqProperty("rondes.dossier"));
		afgeslotenRondesGunstigEnMetBeeldenSubquery.createAlias("laatsteOnderzoek", "onderzoek");
		afgeslotenRondesGunstigEnMetBeeldenSubquery.createAlias("onderzoek.laatsteBeoordeling", "beoordeling");
		afgeslotenRondesGunstigEnMetBeeldenSubquery.createAlias("onderzoek.mammografie", "mammografie");
		afgeslotenRondesGunstigEnMetBeeldenSubquery.add(
			Restrictions.and(
				Restrictions.in("status", ScreeningRondeStatus.AFGEROND),
				Restrictions.eq("beoordeling.status", MammaBeoordelingStatus.UITSLAG_GUNSTIG),
				Restrictions.eq("mammografie.ilmStatus", MammaMammografieIlmStatus.BESCHIKBAAR)));
		afgeslotenRondesGunstigEnMetBeeldenSubquery.setProjection(Projections.count("id"));

		criteria.add(Subqueries.lt(MINIMUM_AANTAL_GUNSTIGE_AFGESLOTEN_RONDES_MET_BEELDEN, afgeslotenRondesGunstigEnMetBeeldenSubquery));

		var dossierBevatOngustigeRondeSubquery = DetachedCriteria.forClass(MammaDossier.class, "dos");
		dossierBevatOngustigeRondeSubquery.createAlias("screeningRondes", "rondes");
		dossierBevatOngustigeRondeSubquery.createAlias("rondes.laatsteOnderzoek", "onderzoek");
		dossierBevatOngustigeRondeSubquery.createAlias("onderzoek.laatsteBeoordeling", "beoordeling");
		dossierBevatOngustigeRondeSubquery.add(
			Restrictions.or(
				Restrictions.in("beoordeling.status", Arrays.asList(MammaBeoordelingStatus.UITSLAG_ONGUNSTIG,
					MammaBeoordelingStatus.ONBEOORDEELBAAR)),
				Restrictions.and(
					Restrictions.eq("beoordeling.status", MammaBeoordelingStatus.UITSLAG_GUNSTIG),
					Restrictions.eq("rondes.followUpConclusieStatus", MammaFollowUpConclusieStatus.FALSE_NEGATIVE))));
		dossierBevatOngustigeRondeSubquery.setProjection(Projections.id());

		criteria.add(Subqueries.propertyNotIn("id", dossierBevatOngustigeRondeSubquery));
		criteria.setMaxResults(MAX_AANTAL_DOSSIERS);

		return criteria;
	}
}
