package nl.rivm.screenit.batch.jobs.colon.ifobtrappel.rappelstep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-dk
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

import java.util.Date;

import lombok.AllArgsConstructor;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.batch.jobs.colon.ifobtrappel.IfobtRappelJobConstants;
import nl.rivm.screenit.batch.jobs.helpers.BaseScrollableResultReader;
import nl.rivm.screenit.dao.colon.impl.ColonRestrictions;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ScreeningRondeStatus;
import nl.rivm.screenit.model.colon.IFOBTType;
import nl.rivm.screenit.model.colon.IFOBTVervaldatum;
import nl.rivm.screenit.model.colon.enums.IFOBTTestStatus;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.query.ScreenitRestrictions;
import nl.rivm.screenit.util.query.StringLengthRestrictions;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

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
public class IfobtRappelBriefReader extends BaseScrollableResultReader
{
	private final SimplePreferenceService preferenceService;

	private final ICurrentDateSupplier currentDateSupplier;

	@Override
	public Criteria createCriteria(StatelessSession session) throws HibernateException
	{

		var criteria = session.createCriteria(Client.class)
			.createAlias("colonDossier", "colonDossier")
			.createAlias("colonDossier.laatsteScreeningRonde", "laatsteScreeningRonde")
			.createAlias("laatsteScreeningRonde.laatsteIFOBTTest", "laatsteTest")
			.createAlias("persoon", "persoon", JoinType.INNER_JOIN)
			.createAlias("persoon.gbaAdres", "adres", JoinType.INNER_JOIN)

			.setFetchMode("persoon", FetchMode.JOIN)

			.add(Restrictions.eq("laatsteScreeningRonde.status", ScreeningRondeStatus.LOPEND))
			.add(Restrictions.eq("laatsteTest.status", IFOBTTestStatus.ACTIEF))
			.add(Restrictions.le("laatsteTest.statusDatum", getHerinneringsDatum()))
			.add(Restrictions.eq("laatsteTest.herinnering", Boolean.FALSE))
			.add(Restrictions.eq("laatsteTest.type", IFOBTType.GOLD))
			.add(Subqueries.exists(getDetachedIFOBTVervaldatumCriteria()))
			.setProjection(Projections.countDistinct("id"));

		ColonRestrictions.addNogGeenUitslagbriefOntvangenCriteria(criteria, "laatsteScreeningRonde");
		ScreenitRestrictions.addClientBaseRestrictions(criteria, "", "persoon");

		Integer minimumLeeftijd = preferenceService.getInteger(PreferenceKey.MINIMALE_LEEFTIJD_COLON.name());
		Integer maximumLeeftijd = preferenceService.getInteger(PreferenceKey.MAXIMALE_LEEFTIJD_COLON.name());
		Integer interval = preferenceService.getInteger(PreferenceKey.UITNODIGINGSINTERVAL.name());

		criteria.add(ScreenitRestrictions.getLeeftijdsgrensRestrictions(minimumLeeftijd, maximumLeeftijd, interval, currentDateSupplier.getLocalDate()));

		int uniqueResult = ((Number) criteria.uniqueResult()).intValue();
		getExecutionContext().putInt(IfobtRappelJobConstants.GESELECTEERD, uniqueResult);

		return criteria;
	}

	private DetachedCriteria getDetachedIFOBTVervaldatumCriteria()
	{
		return DetachedCriteria.forClass(IFOBTVervaldatum.class, "vervaldatum")
			.setProjection(Projections.id())
			.add(Restrictions.gt("vervaldatum.vervalDatum", DateUtil.toUtilDate(currentDateSupplier.getLocalDate())))
			.add(Restrictions.and(Restrictions.leProperty("vervaldatum.barcodeStart", "laatsteTest.barcode"),
				Restrictions.geProperty("vervaldatum.barcodeEnd", "laatsteTest.barcode")))
			.add(StringLengthRestrictions.eq("vervaldatum.lengthBarcode", "laatsteTest.barcode"));
	}

	private Date getHerinneringsDatum()
	{
		try
		{
			var herinneringsPeriode = preferenceService.getInteger(PreferenceKey.IFOBTRAPELPERIODE.name());
			var herinneringsDatum = currentDateSupplier.getLocalDateTime().minusDays(herinneringsPeriode);
			return DateUtil.toUtilDate(herinneringsDatum);
		}
		catch (Exception e)
		{
			crashMelding("De FIT herinnering-periode is niet gezet in de parameterisatie.", e);
			throw e;
		}
	}
}
