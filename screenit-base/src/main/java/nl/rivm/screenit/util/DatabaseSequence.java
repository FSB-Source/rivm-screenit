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

import org.hibernate.Session;

public enum DatabaseSequence
{
	MEEKIJKVERZOEK_ID("mamma.meekijkverzoek_id_seq"),

	HPTV_KWALITEITSBORGING_ID("cervix.hpv_kwaliteitsborging_id_seq"),

	MEDEWERKERCODE("gedeeld.medewerkercode_seq", 100001L),

	MONSTER_ID("cervix.monster_id_seq", 1000000000L),

	UITNODIGINGS_ID("gedeeld.uitnodigings_id_seq"),

	EXCHANGE_ID("gedeeld.exchange_id_seq");

	private final String databaseNaam;

	private final Long startAt;

	DatabaseSequence(String databaseNaam, Long startAt)
	{
		this.databaseNaam = databaseNaam;
		this.startAt = startAt;
	}

	DatabaseSequence(String databaseNaam)
	{
		this(databaseNaam, null);
	}

	public String getDatabaseNaam()
	{
		return databaseNaam;
	}

	public static void createAll(Session session)
	{
		for (DatabaseSequence sequence : values())
		{
			String queryString = "DROP SEQUENCE IF EXISTS " + sequence.databaseNaam + "; CREATE SEQUENCE " + sequence.databaseNaam + ";";
			if (sequence.startAt != null)
			{
				queryString += "ALTER SEQUENCE " + sequence.databaseNaam + " RESTART WITH " + sequence.startAt + ";";
			}
			session.createSQLQuery(queryString).executeUpdate();
		}
	}
}
