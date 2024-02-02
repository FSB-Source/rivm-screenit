package nl.rivm.screenit.batch.jobs.colon.controleuitslag.controlestep;

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
import nl.rivm.screenit.model.OrganisatieParameterKey;
import nl.rivm.screenit.model.colon.IFOBTTest;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.OrganisatieParameterService;

import org.hibernate.Criteria;
import org.hibernate.HibernateException;
import org.hibernate.StatelessSession;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.Projection;
import org.hibernate.criterion.Projections;
import org.springframework.stereotype.Component;

import static nl.rivm.screenit.Constants.MAX_AANTAL_DAGEN_TERUGKIJKEN_CONTROLE_MISSENDE_UITSLAGEN;

@Component
@AllArgsConstructor
public class ColonControleMissendeUitslagenReader extends BaseScrollableResultReader
{
	private final ICurrentDateSupplier currentDateSupplier;

	private final OrganisatieParameterService organisatieParameterService;

	@Override
	public Criteria createCriteria(StatelessSession session) throws HibernateException
	{
		try
		{
			var signaleringsTermijn = organisatieParameterService.getOrganisatieParameter(null, OrganisatieParameterKey.COLON_SIGNALERINGSTERMIJN_MISSENDE_UITSLAGEN, 30);

			var criteria = session.createCriteria(IFOBTTest.class, "ifobt");
			var vandaag = currentDateSupplier.getLocalDate();
			ColonRestrictions.addIfobtMissendeUitslagRestrictions(criteria, vandaag.minusDays(MAX_AANTAL_DAGEN_TERUGKIJKEN_CONTROLE_MISSENDE_UITSLAGEN),
				vandaag.minusDays(signaleringsTermijn));
			criteria.addOrder(Order.asc("dossier.id"));
			return criteria;
		}
		catch (Exception e)
		{
			crashMelding("Fout bij het bepalen van missende uitslagen DK", e);
			throw e;
		}
	}

	@Override
	protected Projection getProjection()
	{
		return Projections.distinct(Projections.property("dossier.id"));
	}
}
