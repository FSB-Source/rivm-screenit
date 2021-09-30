package nl.rivm.screenit.mamma.se.proxy.dao.impl;

/*-
 * ========================LICENSE_START=================================
 * se-proxy
 * %%
 * Copyright (C) 2017 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.time.LocalDate;

import nl.rivm.screenit.mamma.se.proxy.dao.AuthenticatieDao;
import nl.rivm.screenit.mamma.se.proxy.model.IngelogdeGebruikerDto;
import nl.rivm.screenit.mamma.se.proxy.model.LoginContext;
import nl.rivm.screenit.mamma.se.proxy.model.SeConfiguratieKey;
import nl.rivm.screenit.mamma.se.proxy.services.ConfiguratieService;
import nl.rivm.screenit.mamma.se.proxy.util.DateUtil;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

@Repository
public class AuthenticatieDaoImpl extends BaseDaoImpl implements AuthenticatieDao
{
	@Autowired
	private ConfiguratieService configuratieService;

	private static final Logger LOG = LoggerFactory.getLogger(AuthenticatieDaoImpl.class);

	@Override
	public IngelogdeGebruikerDto getIngelogdeGebruiker(LoginContext loginContext)
	{
		IngelogdeGebruikerDto ingelogdeGebruiker = null;

		String query = "SELECT IG.gebruikersnaam, IG.wachtwoord, IG.laatste_inlog, IG.yubikey_public, IG.login_response, IG.account_id " +
			"FROM INGELOGDE_GEBRUIKER IG " +
			"WHERE IG.gebruikersnaam=? " +
			"AND IG.wachtwoord=? " +
			"AND IG.yubikey_public=?;";

		try (Connection dbConnection = getConnection();
			PreparedStatement statement = dbConnection.prepareStatement(query))
		{
			statement.setString(1, loginContext.getGebruikersnaam());
			statement.setString(2, loginContext.getEncryptedWachtwoord());
			statement.setString(3, loginContext.getYubikeyIdentificatie());
			ResultSet resultSet = statement.executeQuery();
			if (resultSet.next())
			{
				return getIngelogdeGebruikerFromResultSet(resultSet);
			}
		}
		catch (SQLException e)
		{
			LOG.warn("Er is een probleem met het ophalen van een ingelogde gebruiker met gebruikersnaam " + loginContext.getGebruikersnaam() + ": " + e.getMessage());
			throw new IllegalStateException("Ophalen van ingelogde gebruiker ging fout.");
		}
		return ingelogdeGebruiker;
	}

	@Override
	public void insertOrUpdateIngelogdeGebruiker(IngelogdeGebruikerDto ingelogdeGebruikerDto)
	{
		String sql = "INSERT INTO INGELOGDE_GEBRUIKER(gebruikersnaam, wachtwoord, laatste_inlog, yubikey_public, login_response, account_id)" +
			" VALUES (?, ?, ?, ? ,?, ?)" +
			" ON CONFLICT(account_id)" +
			" DO UPDATE SET gebruikersnaam = ?, wachtwoord = ?, laatste_inlog = ?, yubikey_public = ?, login_response = ?, account_id = ?;";
		try (Connection connection = getConnection();
			PreparedStatement insertStatement = connection.prepareStatement(sql))
		{
			insertStatement.setString(1, ingelogdeGebruikerDto.getGebruikersnaam());
			insertStatement.setString(2, ingelogdeGebruikerDto.getWachtwoord());
			insertStatement.setString(3, ingelogdeGebruikerDto.getLaatsteInlog().toString());
			insertStatement.setString(4, ingelogdeGebruikerDto.getYubikeyIdentificatie());
			insertStatement.setString(5, ingelogdeGebruikerDto.getLoginResponse());
			insertStatement.setLong(6, ingelogdeGebruikerDto.getAccountId());

			insertStatement.setString(7, ingelogdeGebruikerDto.getGebruikersnaam());
			insertStatement.setString(8, ingelogdeGebruikerDto.getWachtwoord());
			insertStatement.setString(9, ingelogdeGebruikerDto.getLaatsteInlog().toString());
			insertStatement.setString(10, ingelogdeGebruikerDto.getYubikeyIdentificatie());
			insertStatement.setString(11, ingelogdeGebruikerDto.getLoginResponse());
			insertStatement.setLong(12, ingelogdeGebruikerDto.getAccountId());
			insertStatement.execute();
		}
		catch (SQLException e)
		{
			LOG.warn("Er ging iets fout bij toevoegen van een ingelogde gebruiker met gebruikersnaam " + ingelogdeGebruikerDto.getGebruikersnaam() + ": " + e.getMessage());
			throw new IllegalStateException("Toevoegen van ingelogde gebruiker ging fout.");
		}
	}

	@Override
	public void updateIngelogdeGebruiker(IngelogdeGebruikerDto ingelogdeGebruikerDto)
	{
		String sql = "UPDATE INGELOGDE_GEBRUIKER " +
			"SET gebruikersnaam = ?, wachtwoord = ?, laatste_inlog = ?, yubikey_public = ?, login_response = ?, account_id = ? " +
			"WHERE gebruikersnaam = ? ;";
		try (Connection connection = getConnection();
			PreparedStatement insertStatement = connection.prepareStatement(sql))
		{
			insertStatement.setString(1, ingelogdeGebruikerDto.getGebruikersnaam());
			insertStatement.setString(2, ingelogdeGebruikerDto.getWachtwoord());
			insertStatement.setString(3, ingelogdeGebruikerDto.getLaatsteInlog().toString());
			insertStatement.setString(4, ingelogdeGebruikerDto.getYubikeyIdentificatie());
			insertStatement.setString(5, ingelogdeGebruikerDto.getLoginResponse());
			insertStatement.setLong(6, ingelogdeGebruikerDto.getAccountId());
			insertStatement.setString(7, ingelogdeGebruikerDto.getGebruikersnaam());
			insertStatement.execute();
		}
		catch (SQLException e)
		{
			LOG.warn("Er ging iets fout bij updaten van een ingelogde gebruiker met gebruikersnaam " + ingelogdeGebruikerDto.getGebruikersnaam() + ": " + e.getMessage());
			throw new IllegalStateException("Updaten van ingelogde gebruiker ging fout.");
		}
	}

	@Override
	public Long getAccountIdFromUsername(String gebruikersnaam)
	{
		String query = "SELECT IG.account_id " +
			"FROM INGELOGDE_GEBRUIKER IG " +
			"WHERE IG.gebruikersnaam=?;";

		try (Connection dbConnection = getConnection();
			PreparedStatement statement = dbConnection.prepareStatement(query))
		{
			statement.setString(1, gebruikersnaam);
			ResultSet resultSet = statement.executeQuery();
			if (resultSet.next())
			{
				return resultSet.getLong(1);
			}
		}
		catch (SQLException e)
		{
			LOG.warn("Er is een probleem met het ophalen van een ingelogde gebruiker met gebruikersnaam " + gebruikersnaam + ": " + e.getMessage());
			throw new IllegalStateException("Ophalen van ingelogde gebruiker ging fout.");
		}
		return null;
	}

	@Override
	public void verwijderOudeIngelogdeGebruikers()
	{
		int termijn = configuratieService.getConfiguratieIntegerValue(SeConfiguratieKey.SE_MAX_OFFLINE_INLOG_PERIODE);

		String sql = "DELETE FROM INGELOGDE_GEBRUIKER WHERE laatste_inlog < ?;";
		try (Connection connection = getConnection();
			PreparedStatement removeTransactie = connection.prepareStatement(sql))
		{
			removeTransactie.setString(1, DateUtil.getCurrentDateTime().toLocalDate().minusDays(termijn).toString());
			removeTransactie.execute();
		}
		catch (SQLException e)
		{
			LOG.error("Er is een probleem met het verwijderen van de oude ingelogde gebruikers: " + e.getMessage());
		}
	}

	private IngelogdeGebruikerDto getIngelogdeGebruikerFromResultSet(ResultSet resultSet) throws SQLException
	{
		return new IngelogdeGebruikerDto(resultSet.getString(1), resultSet.getString(2), LocalDate.parse(resultSet.getString(3)),
			resultSet.getString(4), resultSet.getString(5), resultSet.getLong(6));
	}
}
