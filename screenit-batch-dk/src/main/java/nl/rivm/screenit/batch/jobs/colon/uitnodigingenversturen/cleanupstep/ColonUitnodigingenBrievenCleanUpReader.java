package nl.rivm.screenit.batch.jobs.colon.uitnodigingenversturen.cleanupstep;

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

import nl.rivm.screenit.batch.jobs.brieven.cleanup.AbstractBrievenCleanUpReader;
import nl.rivm.screenit.batch.service.UitnodigingenCleanUpService;
import nl.rivm.screenit.model.colon.ColonMergedBrieven;

import org.hibernate.Criteria;
import org.hibernate.HibernateException;
import org.hibernate.StatelessSession;
import org.springframework.beans.factory.annotation.Autowired;

public class ColonUitnodigingenBrievenCleanUpReader extends AbstractBrievenCleanUpReader<ColonMergedBrieven>
{

	@Autowired
	private UitnodigingenCleanUpService uitnodigingenCleanUpService;

	@Override
	public Criteria createCriteria(StatelessSession session) throws HibernateException
	{
		try
		{
			return uitnodigingenCleanUpService.cleanUpUitnodigingen(session, getMergedBrievenClass(), getMinimaleBestaanOpFilestore());
		}
		catch (Exception e)
		{
			crashMelding("Brieven konden niet geselecteerd worden om opgeruimte te worden", e);
			throw e;
		}
	}
}
