
package nl.rivm.screenit.batch.jobs.generalis.gba.verwerk105step;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-alg
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

import nl.rivm.screenit.model.gba.GbaVraag;

import org.hibernate.Criteria;
import org.hibernate.ScrollMode;
import org.hibernate.ScrollableResults;
import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.springframework.batch.item.ExecutionContext;
import org.springframework.batch.item.ItemReader;
import org.springframework.batch.item.ItemStream;
import org.springframework.batch.item.ItemStreamException;
import org.springframework.batch.item.NonTransientResourceException;
import org.springframework.batch.item.ParseException;
import org.springframework.batch.item.UnexpectedInputException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.orm.hibernate5.SessionFactoryUtils;

public class GbaVraagItemReader implements ItemReader<Long>, ItemStream
{
	
	private static final String ROWNUMBER = "key.rownumber";

	@Autowired
	private SessionFactory sessionFactory;

	private Session hibernateSession;

	private ScrollableResults scrollableResults;

	@Override
	public Long read() throws UnexpectedInputException, ParseException, NonTransientResourceException
	{
		if (scrollableResults.next())
		{
			return (Long) scrollableResults.get()[0];
		}
		return null;
	}

	@Override
	public void open(ExecutionContext executionContext) throws ItemStreamException
	{
		hibernateSession = sessionFactory.openSession();

		Criteria gbaVragen = hibernateSession.createCriteria(GbaVraag.class);
		gbaVragen.add(Restrictions.eq("verstuurd", false));
		gbaVragen.setProjection(Projections.id());

		scrollableResults = gbaVragen.scroll(ScrollMode.FORWARD_ONLY);

		if (executionContext.containsKey(ROWNUMBER))
		{
			scrollableResults.setRowNumber(executionContext.getInt(ROWNUMBER));
		}
	}

	@Override
	public void update(ExecutionContext executionContext) throws ItemStreamException
	{
		executionContext.putInt(ROWNUMBER, scrollableResults.getRowNumber());
	}

	@Override
	public void close() throws ItemStreamException
	{
		SessionFactoryUtils.closeSession(hibernateSession);
	}

}
