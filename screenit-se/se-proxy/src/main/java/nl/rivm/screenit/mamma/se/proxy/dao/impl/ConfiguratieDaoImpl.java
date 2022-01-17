package nl.rivm.screenit.mamma.se.proxy.dao.impl;

/*-
 * ========================LICENSE_START=================================
 * se-proxy
 * %%
 * Copyright (C) 2017 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.mamma.se.proxy.dao.ConfiguratieDao;
import nl.rivm.screenit.mamma.se.proxy.model.SeConfiguratieKey;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Repository;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

@Repository
public class ConfiguratieDaoImpl extends BaseDaoImpl implements ConfiguratieDao
{
    private static final Logger LOG = LoggerFactory.getLogger(ConfiguratieDaoImpl.class);

    @Override
    public String getConfiguratieValue(SeConfiguratieKey key)
    {
        String sql = "SELECT VALUE FROM CONFIGURATIE WHERE key = ?;";

        try (Connection dbConnection = getConnection();
             PreparedStatement statement = dbConnection.prepareStatement(sql))
        {
            statement.setString(1, key.name());
            ResultSet resultSet = statement.executeQuery();
            if (resultSet.next())
            {
                return resultSet.getString("value");
            }
        }
        catch (SQLException e)
        {
            LOG.warn("Er is een probleem met het ophalen van een configuratie met key " + key + ": " + e.getMessage());
        }
        return null;
    }

    @Override
    public Integer getConfiguratieIntegerValue(SeConfiguratieKey key)
    {
        try
        {
            return Integer.parseInt(getConfiguratieValue(key));
        }
        catch (NumberFormatException e)
        {
            LOG.warn("Configuratie met key " + key + " bestaat niet of is niet numeriek (maar hoort dit wel te zijn): " + e.getMessage());
            return 0;
        }
    }

    @Override
    public void insertOrUpdateConfiguratieValue(SeConfiguratieKey key, String value)
    {
        String sql = "INSERT INTO CONFIGURATIE(key, value) VALUES (?, ?)" +
                " ON CONFLICT(key)" +
                " DO UPDATE SET value = excluded.value " +
                " WHERE value != excluded.value;";
        try (Connection connection = getConnection();
             PreparedStatement insertStatement = connection.prepareStatement(sql))
        {
            insertStatement.setString(1, key.name());
            insertStatement.setString(2, value);
            insertStatement.execute();
        }
        catch (SQLException e)
        {
            LOG.warn("Er ging iets fout bij toevoegen van een configuratie met key " + key + ": " + e.getMessage());
            throw new IllegalStateException("Toevoegen van configuratie ging fout.");
        }
    }

    @Override
    public void updateConfiguratieValue(SeConfiguratieKey key, String value)
    {
        String sql = "UPDATE CONFIGURATIE SET value = ? WHERE key = ?;";
        try (Connection connection = getConnection();
             PreparedStatement updateStatement = connection.prepareStatement(sql))
        {
            updateStatement.setString(1, value);
            updateStatement.setString(2, key.name());
            updateStatement.execute();
        }
        catch (SQLException e)
        {
            LOG.warn("Er ging iets fout bij updaten van een configuratie met key " + key + ": " + e.getMessage());
            throw new IllegalStateException("Updaten van configuratie ging fout.");
        }
    }
}
