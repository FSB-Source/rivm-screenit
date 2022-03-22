package nl.rivm.screenit.batch.jobs.cervix.vervolgonderzoek.step;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
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

import nl.rivm.screenit.batch.jobs.helpers.BaseScrollableResultReader;
import nl.rivm.screenit.model.ScreeningRondeStatus;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.query.ScreenitRestrictions;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.hibernate.Criteria;
import org.hibernate.HibernateException;
import org.hibernate.StatelessSession;
import org.hibernate.criterion.Restrictions;
import org.hibernate.sql.JoinType;
import org.springframework.beans.factory.annotation.Autowired;

public class CervixVervolgonderzoekReader extends BaseScrollableResultReader
{

	@Autowired
	private ICurrentDateSupplier dateSupplier;

	@Autowired
	private SimplePreferenceService preferenceService;

	@Override
	public Criteria createCriteria(StatelessSession session) throws HibernateException
	{
		Criteria crit = session.createCriteria(CervixScreeningRonde.class, "ronde");
		crit.createAlias("ronde.dossier", "dossier");
		crit.createAlias("ronde.uitstel", "uitstel", JoinType.LEFT_OUTER_JOIN);
		crit.createAlias("dossier.client", "client");
		crit.createAlias("client.persoon", "persoon");

		ScreenitRestrictions.addClientBaseRestrictions(crit, "client", "persoon");

		crit.add(Restrictions.eq("ronde.status", ScreeningRondeStatus.LOPEND));

		crit.add(Restrictions.isNotNull("ronde.inVervolgonderzoekDatum"));
		crit.add(Restrictions.isNull("ronde.uitnodigingVervolgonderzoek"));
		crit.add(Restrictions.or(Restrictions.isNull("ronde.uitstel"), Restrictions.isNotNull("uitstel.geannuleerdDatum")));

		crit.add(Restrictions.le("ronde.controleUitstrijkjeDatum", dateSupplier.getLocalDate()));

		crit.add(Restrictions.sqlRestriction(
			"{alias}.id NOT IN (SELECT DISTINCT r.id FROM cervix.screening_ronde r JOIN cervix.monster m ON r.id = m.ontvangst_screening_ronde LEFT JOIN cervix.labformulier l ON m.labformulier = l.id AND (l.status = 'GECONTROLEERD' OR l.status = 'GECONTROLEERD_CYTOLOGIE' OR l.status = 'HUISARTS_ONBEKEND') WHERE r.in_vervolgonderzoek_datum NOTNULL AND (r.in_vervolgonderzoek_datum < m.ontvangstdatum AND l.scan_datum ISNULL OR r.in_vervolgonderzoek_datum < l.scan_datum AND m.ontvangstdatum ISNULL OR r.in_vervolgonderzoek_datum < m.ontvangstdatum AND r.in_vervolgonderzoek_datum < l.scan_datum))"));

		return crit;
	}
}
