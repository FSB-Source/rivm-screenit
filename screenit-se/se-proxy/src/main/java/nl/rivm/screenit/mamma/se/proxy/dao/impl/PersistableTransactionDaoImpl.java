package nl.rivm.screenit.mamma.se.proxy.dao.impl;

/*-
 * ========================LICENSE_START=================================
 * se-proxy
 * %%
 * Copyright (C) 2017 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.sql.Statement;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

import nl.rivm.screenit.mamma.se.proxy.dao.PersistableTransactionDao;
import nl.rivm.screenit.mamma.se.proxy.model.PersistableTransaction;
import nl.rivm.screenit.mamma.se.proxy.util.DateUtil;
import nl.rivm.screenit.mamma.se.proxy.util.TransactionParser;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Repository;

@Repository
public class PersistableTransactionDaoImpl extends BaseDaoImpl implements PersistableTransactionDao
{
	private static final Logger LOG = LoggerFactory.getLogger(PersistableTransactionDaoImpl.class);

	private static final int WEKEN = 1;

	@Override
	public void putLast(PersistableTransaction transaction)
	{
		String sql = "INSERT INTO TRANSACTIE (datumTijd, transactie, clientId) VALUES (?, ?, ?);";
		try (Connection connection = getConnection();
			PreparedStatement putLast = connection.prepareStatement(sql))
		{
			putLast.setString(1, transaction.getDatumTijd().toString());
			putLast.setString(2, transaction.getTransactie());
			putLast.setString(3, transaction.getClientId() != null ? Long.toString(transaction.getClientId()) : null);
			putLast.execute();
		}
		catch (SQLException e)
		{
			LOG.error("Er ging iets fout bij het persisteren van transactie id: {}, client: {}, transactietijd: {}. Fout: {}", transaction.getTransactionId(),
				transaction.getClientId(), transaction.getDatumTijd(), e.getMessage());
			throw new IllegalStateException("Persisteren transactie ging fout.");
		}
	}

	@Override
	public PersistableTransaction takeFirst()
	{
		PersistableTransaction transaction = null;

		String query = "SELECT T.ID, T.datumTijd, T.transactie, T.clientId " +
			"FROM TRANSACTIE T " +
			"LEFT JOIN CLIENTEN_MET_GEBLOKKEERDE_TRANSACTIES FT ON FT.clientId = T.clientId " +
			"WHERE FT.clientId ISNULL " +
			"ORDER BY T.ID ASC LIMIT 1;";

		try (Connection dbConnection = getConnection();
			Statement statement = dbConnection.createStatement();
			ResultSet takeFirstResultSet = statement.executeQuery(query))
		{
			if (takeFirstResultSet.next())
			{
				transaction = getTransactionFromResultSet(takeFirstResultSet);
			}
		}
		catch (SQLException e)
		{
			LOG.error("Er is een probleem met het ophalen van de eerstvolgende te versturen transactie: " + e.getMessage());
		}
		return transaction;
	}

	@Override
	public List<PersistableTransaction> getAll()
	{
		String getAll = "SELECT ID, datumTijd, transactie, clientId FROM TRANSACTIE;";
		List<PersistableTransaction> list = new ArrayList<>();

		try (Connection dbConnection = getConnection();
			Statement statement = dbConnection.createStatement();
			ResultSet getAllResultSet = statement.executeQuery(getAll))
		{
			while (getAllResultSet.next())
			{
				list.add(getTransactionFromResultSet(getAllResultSet));
			}
		}
		catch (SQLException e)
		{
			LOG.error("Pending transacties konden niet uit de database gelezen worden: " + e.getMessage());
			throw new IllegalStateException("Er is iets fout gegaan bij het ophalen van transacties.");
		}
		return list;
	}

	@Override
	public void clearDb()
	{
		String getAll = "SELECT name FROM sqlite_master WHERE type ='table' AND name NOT LIKE 'sqlite_%' AND name NOT LIKE 'DATABASECHANGELOG%';";

		List<String> tablesToTruncate = new ArrayList<>();
		try (Connection dbConnection = getConnection();
			Statement statement = dbConnection.createStatement();
			ResultSet getAllResultSet = statement.executeQuery(getAll))
		{
			while (getAllResultSet.next())
			{
				tablesToTruncate.add(getAllResultSet.getString(1));
			}
		}
		catch (SQLException e)
		{
			LOG.warn("Tabelnamen konden niet uit de database gelezen worden: " + e.getMessage());
			throw new IllegalStateException("Er is iets fout gegaan bij het ophalen van lijst van tabel namen.");
		}
		for (String tabel : tablesToTruncate)
		{
			try (Connection connection = getConnection();
				PreparedStatement deleteStatement = connection.prepareStatement("DELETE FROM " + tabel))
			{
				deleteStatement.execute();
			}
			catch (SQLException e)
			{
				LOG.warn("Er is een probleem met het leegmaken van " + tabel + ": " + e.getMessage());
				throw new IllegalStateException("Er is iets fout gegaan bij het leeg maken van tabel " + tabel + ".");
			}
		}

	}

	@Override
	public void remove(Long id)
	{
		String sql = "DELETE FROM TRANSACTIE WHERE ID = ?;";
		try (Connection connection = getConnection();
			PreparedStatement removeTransactie = connection.prepareStatement(sql))
		{
			removeTransactie.setLong(1, id);
			removeTransactie.execute();
		}
		catch (SQLException e)
		{
			LOG.error("Er is een probleem met het verwijderen van transactie id {}: {}", id, e.getMessage());
		}
	}

	@Override
	public void addToVerstuurdeTransacties(PersistableTransaction transaction)
	{
		String sql = "INSERT INTO VERSTUURDE_TRANSACTIE(afspraakId, datumTijd, transactie, clientId) VALUES (?, ?, ?, ?);";

		try (Connection connection = getConnection();
			PreparedStatement addToVerstuurdeTransacties = connection.prepareStatement(sql))
		{
			addToVerstuurdeTransacties.setString(1, new TransactionParser(transaction.getTransactie()).getAfspraakId());
			addToVerstuurdeTransacties.setString(2, transaction.getDatumTijd().toString());
			addToVerstuurdeTransacties.setString(3, transaction.getTransactie());
			addToVerstuurdeTransacties.setString(4, Long.toString(transaction.getClientId()));
			addToVerstuurdeTransacties.execute();
		}
		catch (SQLException e)
		{
			LOG.warn("Er ging iets fout bij het opslaan van transactie {} in de verstuurde transacties tabel: {}", transaction.getTransactionId(), e.getMessage());
			throw new IllegalStateException("Persisteren transactie voor langere tijd ging fout.");
		}
	}

	@Override
	public void startOfDayCleanUp()
	{
		String verwijderDatum = DateUtil.getCurrentDateTime().minusWeeks(WEKEN).toString();
		LOG.info("Verwijder verstuurde transacties met datum t/m {}", verwijderDatum);

		String sql = "DELETE FROM VERSTUURDE_TRANSACTIE WHERE datumTijd <= ?;";

		try (Connection connection = getConnection();
			PreparedStatement removeVerstuurdeTransactie = connection.prepareStatement(sql))
		{
			removeVerstuurdeTransactie.setString(1, verwijderDatum);
			removeVerstuurdeTransactie.execute();
		}
		catch (SQLException e)
		{
			LOG.warn("Opschonen van verstuurde transacties ging fout: " + e.getMessage());
		}
	}

	@Override
	public void addToFouteTransactie(PersistableTransaction transaction)
	{
		String sql = "INSERT INTO CLIENTEN_MET_GEBLOKKEERDE_TRANSACTIES (clientId, transactieId) VALUES (?, ?);";
		try (Connection connection = getConnection();
			PreparedStatement putLast = connection.prepareStatement(sql))
		{
			putLast.setString(1, transaction.getClientId().toString());
			putLast.setString(2, Long.toString(transaction.getTransactionId()));
			putLast.execute();
		}
		catch (SQLException e)
		{
			LOG.error("Er ging iets fout bij blokkeren client door transactie {}: {}", transaction.getTransactionId(), e.getMessage());
			throw new IllegalStateException("Toevoegen foute transactie ging fout.");
		}
	}

	private PersistableTransaction getTransactionFromResultSet(ResultSet takeFirstResultSet) throws SQLException
	{
		return new PersistableTransaction(takeFirstResultSet.getLong(1), LocalDateTime.parse(takeFirstResultSet.getString(2)),
			takeFirstResultSet.getString(3), takeFirstResultSet.getLong(4));
	}
}
