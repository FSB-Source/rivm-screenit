package nl.rivm.screenit.batch.jobs.mamma.uitnodigen.interval;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bk
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

import java.time.LocalDate;
import java.util.Date;

import lombok.AllArgsConstructor;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.batch.jobs.helpers.BaseScrollableResultReader;
import nl.rivm.screenit.dao.mamma.MammaSelectieRestrictions;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.mamma.enums.MammaAfspraakStatus;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus;
import nl.rivm.screenit.model.mamma.enums.MammaOnderzoekStatus;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.mamma.MammaVolgendeUitnodigingService;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.hibernate.Criteria;
import org.hibernate.HibernateException;
import org.hibernate.StatelessSession;
import org.hibernate.criterion.Restrictions;
import org.hibernate.sql.JoinType;
import org.springframework.stereotype.Component;

@Component
@AllArgsConstructor
public class MammaIntervalUitnodigenReader extends BaseScrollableResultReader
{
	private final MammaVolgendeUitnodigingService volgendeUitnodigingService;

	private final SimplePreferenceService preferenceService;

	private final ICurrentDateSupplier currentDateSupplier;

	private final MammaSelectieRestrictions selectieRestricties;

	@Override
	public Criteria createCriteria(StatelessSession session) throws HibernateException
	{
		volgendeUitnodigingService.updateIntervalReferentieDatums();

		Criteria criteria = session.createCriteria(Client.class, "client");
		criteria.createAlias("client.persoon", "persoon");
		criteria.createAlias("persoon.gbaAdres", "adres");
		criteria.createAlias("persoon.tijdelijkGbaAdres", "tijdelijkGbaAdres", JoinType.LEFT_OUTER_JOIN);
		criteria.createAlias("client.mammaDossier", "dossier");
		criteria.createAlias("dossier.volgendeUitnodiging", "volgendeUitnodiging");
		criteria.createAlias("volgendeUitnodiging.interval", "uitnodigingsInterval");
		criteria.createAlias("dossier.laatsteScreeningRonde", "laatsteScreeningRonde", JoinType.LEFT_OUTER_JOIN);
		criteria.createAlias("laatsteScreeningRonde.laatsteUitnodiging", "laatsteUitnodiging", JoinType.LEFT_OUTER_JOIN);
		criteria.createAlias("laatsteUitnodiging.laatsteAfspraak", "laatsteAfspraak", JoinType.LEFT_OUTER_JOIN);
		criteria.createAlias("laatsteScreeningRonde.laatsteOnderzoek", "laatsteOnderzoek", JoinType.LEFT_OUTER_JOIN);
		criteria.createAlias("laatsteOnderzoek.laatsteBeoordeling", "laatsteBeoordeling", JoinType.LEFT_OUTER_JOIN);

		criteria.add(Restrictions.leProperty("volgendeUitnodiging.peildatum", "uitnodigingsInterval.berekendeReferentieDatum"));

		criteria.add(Restrictions.isNull("dossier.tehuis"));
		selectieRestricties.addStandaardSelectieRestricties(criteria);

		var vandaag = currentDateSupplier.getLocalDate();
		criteria.add(Restrictions.gt("persoon.geboortedatum", minimaleGeboorteDatum(vandaag)));
		criteria.add(Restrictions.le("persoon.geboortedatum", maximaleGeboorteDatum(vandaag)));

		criteria.add(Restrictions.or(
			Restrictions.isNull("dossier.laatsteMammografieAfgerond"),
			Restrictions.lt("dossier.laatsteMammografieAfgerond", maximaleMammografieDatum(vandaag))));

		addGeenToekomstigeAfspraakRestrictie(criteria, vandaag);
		addGeenLopendOnderzoekRestrictie(criteria);
		addGeenOpenstaandeBeoordelingRestrictie(criteria);

		return criteria;
	}

	private void addGeenToekomstigeAfspraakRestrictie(Criteria criteria, LocalDate vandaag)
	{
		criteria.add(Restrictions.or(
			Restrictions.isNull("laatsteAfspraak.id"),
			Restrictions.ne("laatsteAfspraak.status", MammaAfspraakStatus.GEPLAND),
			Restrictions.lt("laatsteAfspraak.vanaf", DateUtil.toUtilDate(vandaag))
		));
	}

	private void addGeenLopendOnderzoekRestrictie(Criteria criteria)
	{
		criteria.add(Restrictions.or(
			Restrictions.isNull("laatsteOnderzoek.id"),
			Restrictions.and(
				Restrictions.in("laatsteOnderzoek.status", MammaOnderzoekStatus.AFGEROND, MammaOnderzoekStatus.ONVOLLEDIG, MammaOnderzoekStatus.ONDERBROKEN_ZONDER_VERVOLG),
				Restrictions.eq("laatsteOnderzoek.isDoorgevoerd", true)
			)
		));
	}

	private void addGeenOpenstaandeBeoordelingRestrictie(Criteria criteria)
	{
		criteria.add(Restrictions.or(
			Restrictions.isNull("laatsteBeoordeling.id"),
			Restrictions.in("laatsteBeoordeling.status", MammaBeoordelingStatus.uitslagStatussen())
		));
	}

	private Date minimaleGeboorteDatum(LocalDate vandaag)
	{
		int totEnMetLeeftijd = preferenceService.getInteger(PreferenceKey.MAMMA_MAXIMALE_LEEFTIJD.name());
		return DateUtil.toUtilDate(vandaag.minusYears(totEnMetLeeftijd + 1L));
	}

	private Date maximaleGeboorteDatum(LocalDate vandaag)
	{
		int vanafLeeftijd = preferenceService.getInteger(PreferenceKey.MAMMA_MINIMALE_LEEFTIJD.name());
		return DateUtil.toUtilDate(vandaag.minusYears(vanafLeeftijd));
	}

	private Date maximaleMammografieDatum(LocalDate vandaag)
	{
		int minimaleIntervalMammografieOnderzoeken = preferenceService.getInteger(PreferenceKey.MAMMA_MINIMALE_INTERVAL_MAMMOGRAFIE_ONDERZOEKEN.name());
		return DateUtil.toUtilDate(vandaag.minusDays(minimaleIntervalMammografieOnderzoeken));
	}
}
