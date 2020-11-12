
package nl.rivm.screenit.batch.jobs.colon.ifobtinlezen.cleanupstep;

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

import nl.rivm.screenit.batch.jobs.colon.ifobtinlezen.IfobtInlezenConstants;
import nl.rivm.screenit.batch.jobs.helpers.BaseScrollableResultReader;
import nl.rivm.screenit.model.colon.IFOBTBestand;
import nl.rivm.screenit.model.colon.enums.IFOBTBestandStatus;
import nl.rivm.screenit.model.enums.Level;
import nl.rivm.screenit.model.logging.IfobtVerwerkingBeeindigdLogEvent;

import org.apache.commons.lang.StringUtils;
import org.hibernate.Criteria;
import org.hibernate.HibernateException;
import org.hibernate.StatelessSession;
import org.hibernate.criterion.Restrictions;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class IFOBTBestandCleanupReader extends BaseScrollableResultReader
{

	private static final Logger LOG = LoggerFactory.getLogger(IFOBTBestandCleanupReader.class);

	@Override
	public Criteria createCriteria(StatelessSession session) throws HibernateException
	{
		IfobtVerwerkingBeeindigdLogEvent logEvent = (IfobtVerwerkingBeeindigdLogEvent) getStepExecution().getJobExecution().getExecutionContext()
			.get(IfobtInlezenConstants.RAPPORTAGEKEYINLEZEN);
		try
		{
			Criteria crit = session.createCriteria(IFOBTBestand.class);
			crit.add(Restrictions.or( 
				Restrictions.eq("status", IFOBTBestandStatus.KAN_ORIG_BESTAND_VERWIJDEREN), 
				Restrictions.eq("status", IFOBTBestandStatus.NIEUW)));

			return crit;
		}
		catch (Exception e)
		{
			LOG.error("Fout bij ophalen van bestanden om te verwijderen ", e);
			logEvent.setLevel(Level.ERROR);
			String melding = logEvent.getMelding();
			if (StringUtils.isBlank(melding))
			{
				melding = "";
			}
			melding += " Cleanup fout: FIT bestanden konden niet geselecteerd worden om opgeruimt te worden";
			logEvent.setMelding(melding);
			throw e;
		}
	}

}
