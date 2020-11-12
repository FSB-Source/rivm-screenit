package nl.rivm.screenit.batch.jobs.colon.vervolgintakeconclusie.versturennoshow;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-dk
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.time.LocalDateTime;
import java.util.Date;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.batch.jobs.helpers.BaseScrollableResultReader;
import nl.rivm.screenit.model.DossierStatus;
import nl.rivm.screenit.model.ScreeningRondeStatus;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.colon.enums.ColonConclusieType;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.query.ScreenitRestrictions;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.hibernate.Criteria;
import org.hibernate.HibernateException;
import org.hibernate.StatelessSession;
import org.hibernate.criterion.Restrictions;
import org.springframework.beans.factory.annotation.Autowired;

public class HuisartsNoShowReader extends BaseScrollableResultReader
{

	@Autowired
	private SimplePreferenceService preferenceService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Override
	public Criteria createCriteria(StatelessSession session) throws HibernateException
	{
		Date conclusieMoetGegevenZijnOp = getNoShowDate();

		Criteria criteria = session.createCriteria(ColonScreeningRonde.class);
		criteria.createAlias("dossier", "colonDossier");
		criteria.createAlias("colonDossier.client", "client");
		criteria.createAlias("client.persoon", "persoon");
		criteria.createAlias("persoon.gbaAdres", "adres");

		ScreenitRestrictions.addClientBaseRestrictions(criteria, "client", "persoon");

		criteria.createAlias("laatsteAfspraak", "afspraak");
		criteria.createAlias("afspraak.conclusie", "conclusie");
		criteria.add(Restrictions.eq("status", ScreeningRondeStatus.LOPEND));
		criteria.add(Restrictions.eq("colonDossier.status", DossierStatus.ACTIEF));

		criteria.add(Restrictions.eq("conclusie.type", ColonConclusieType.NO_SHOW));
		criteria.add(Restrictions.le("conclusie.datum", conclusieMoetGegevenZijnOp));

		ScreenitRestrictions.addHeeftGeenAfgerondeColonVerlagenRestrictions(criteria, "");
		criteria.add(Restrictions.isNull("conclusie.noShowBericht"));

		return criteria;
	}

	private Date getNoShowDate()
	{
		try
		{
			Integer periode = preferenceService.getInteger(PreferenceKey.HUISARTS_NO_SHOW_PERIODE.name());
			LocalDateTime nu = currentDateSupplier.getLocalDateTime();
			return DateUtil.toUtilDate(nu.minusDays(periode));
		}
		catch (Exception e)
		{
			crashMelding("De huisarts no show periode is niet gezet.", e);
			throw e;
		}
	}

}
