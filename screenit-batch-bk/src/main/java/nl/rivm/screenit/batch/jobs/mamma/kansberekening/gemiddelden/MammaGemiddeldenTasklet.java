package nl.rivm.screenit.batch.jobs.mamma.kansberekening.gemiddelden;

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

import java.io.IOException;
import java.math.BigInteger;
import java.util.List;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.batch.jobs.mamma.kansberekening.MammaAbstractKansberekeningTasklet;
import nl.rivm.screenit.batch.jobs.mamma.kansberekening.MammaKansberekeningConstants;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.commons.io.IOUtils;
import org.hibernate.SQLQuery;
import org.springframework.beans.factory.annotation.Autowired;

public class MammaGemiddeldenTasklet extends MammaAbstractKansberekeningTasklet
{
	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private ICurrentDateSupplier dateSupplier;

	@Override
	protected void execute()
	{
		try
		{
			String nu = dateSupplier.getLocalDateTime().toString();

			String sql = IOUtils.toString(MammaGemiddeldenTasklet.class.getResourceAsStream("/updateDeelnameGemiddelden.sql"));
			SQLQuery sqlQuery = hibernateService.getHibernateSession().createSQLQuery(sql);
			sqlQuery.setParameter("nu", nu);
			sqlQuery.setParameter("deelnamekansberekening_na_weken", Constants.DEELNAMEKANSBEREKENING_NA_WEKEN);

			List<Object[]> list = sqlQuery.list();
			for (Object[] result : list)
			{
				context.putLong(MammaKansberekeningConstants.REGIO_DEELNAME_GEMIDDELDEN_KEY, ((BigInteger) result[0]).longValue());
				context.putLong(MammaKansberekeningConstants.STANDPLAATS_RONDE_DEELNAME_GEMIDDELDEN_KEY, ((BigInteger) result[1]).longValue());
			}

			sql = IOUtils.toString(MammaGemiddeldenTasklet.class.getResourceAsStream("/updateOpkomstGemiddelden.sql"));
			sqlQuery = hibernateService.getHibernateSession().createSQLQuery(sql);
			sqlQuery.setParameter("nu", nu);

			list = sqlQuery.list();
			for (Object[] result : list)
			{
				context.putLong(MammaKansberekeningConstants.REGIO_OPKOMST_GEMIDDELDEN__KEY, ((BigInteger) result[0]).longValue());
				context.putLong(MammaKansberekeningConstants.STANDPLAATS_RONDE_OPKOMST_GEMIDDELDEN_KEY, ((BigInteger) result[1]).longValue());
			}

		}
		catch (IOException e)
		{
			throw new RuntimeException(e);
		}
	}
}
