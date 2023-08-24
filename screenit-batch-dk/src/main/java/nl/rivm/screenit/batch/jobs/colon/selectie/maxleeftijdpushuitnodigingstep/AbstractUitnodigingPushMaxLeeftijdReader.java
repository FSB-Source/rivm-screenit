package nl.rivm.screenit.batch.jobs.colon.selectie.maxleeftijdpushuitnodigingstep;

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

import java.time.LocalDate;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.batch.jobs.colon.selectie.AbstractUitnodigingPushReader;
import nl.rivm.screenit.batch.jobs.colon.selectie.SelectieConstants;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.colon.enums.ColonUitnodigingCategorie;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.hibernate.Criteria;
import org.hibernate.HibernateException;
import org.hibernate.StatelessSession;
import org.hibernate.criterion.Projection;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.springframework.beans.factory.annotation.Autowired;

@Slf4j
public abstract class AbstractUitnodigingPushMaxLeeftijdReader extends AbstractUitnodigingPushReader
{

	@Autowired
	protected SimplePreferenceService preferenceService;

	protected AbstractUitnodigingPushMaxLeeftijdReader(ColonUitnodigingCategorie categorie)
	{
		super(categorie);
	}

	@Override
	protected Criteria createCriteria(StatelessSession session) throws HibernateException
	{
		var criteria = session.createCriteria(Client.class, "client");
		createBaseCriteria(criteria);
		var geboortedatum = currentDateSupplier.getLocalDate().minusYears(preferenceService.getInteger(PreferenceKey.MAXIMALE_LEEFTIJD_COLON.name()) + 1);

		geboortedatum = geboortedatum.minusDays(getExecutionContext().getInt(SelectieConstants.PUSH_MAX_LEEFTIJD_COUNT) - 1);
		LOG.info("Controle op geboortedatum {}", geboortedatum);
		criteria.add(Restrictions.eq("persoon.geboortedatum", DateUtil.toUtilDate(geboortedatum)));

		return criteria;
	}

	@Override
	protected LocalDate getPeildatum()
	{
		return super.getPeildatum().minusDays(getExecutionContext().getInt(SelectieConstants.PUSH_MAX_LEEFTIJD_COUNT));
	}

	@Override
	protected Projection getProjection()
	{
		return Projections.projectionList()
			.add(Projections.property("id"))
			.add(Projections.property("gemeente.id"))
			.add(Projections.property("screeningorganisatie.id"));
	}

	@Override
	protected ClientTePushenDto mapData(Object[] data)
	{
		var dto = new ClientTePushenDto();
		dto.setClientId(getNullSafeLongFromNumber(data[0]));
		dto.setGemeenteId(getNullSafeLongFromNumber(data[1]));
		dto.setScreeningorganisatieId(getNullSafeLongFromNumber(data[2]));
		return dto;
	}

}
