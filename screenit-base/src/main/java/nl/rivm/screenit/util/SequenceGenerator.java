package nl.rivm.screenit.util;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

import org.hibernate.SessionFactory;
import org.hibernate.dialect.Dialect;
import org.hibernate.engine.spi.SessionFactoryImplementor;
import org.hibernate.jdbc.ReturningWork;

public class SequenceGenerator implements ReturningWork<Long>
{
	private final DatabaseSequence sequence;

	private final SessionFactory sessionFactory;

	public SequenceGenerator(DatabaseSequence sequence, SessionFactory sessionFactory)
	{
		this.sequence = sequence;
		this.sessionFactory = sessionFactory;
	}

	@Override
	public Long execute(Connection connection) throws SQLException
	{
		Dialect dialect = ((SessionFactoryImplementor) sessionFactory).getDialect();
		try (PreparedStatement preparedStatement = connection.prepareStatement(dialect.getSequenceNextValString(sequence.getDatabaseNaam()));
			ResultSet resultSet = preparedStatement.executeQuery())
		{
			resultSet.next();
			return resultSet.getLong(1);
		}
	}
}
