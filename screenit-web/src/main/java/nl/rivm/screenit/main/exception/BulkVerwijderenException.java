package nl.rivm.screenit.main.exception;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.colon.enums.ColonTijdslotType;

import com.fasterxml.jackson.databind.ObjectMapper;

@Getter
@Setter
public class BulkVerwijderenException extends Exception
{
	private final ObjectMapper objectMapper = new ObjectMapper();

	private final ColonTijdslotType typeTijdslot;

	private int aantalVerwijderen;

	private int aantalGebruiktVoorCapaciteit;

	private int aantalMetAfspraak;

	private int aantalNietGevonden;

	public BulkVerwijderenException(ColonTijdslotType typeTijdslot)
	{
		super();
		this.typeTijdslot = typeTijdslot;
	}

	public void aantalVerwijderenOphogen()
	{
		this.aantalVerwijderen += 1;
	}

	public void aantalGebruiktVoorCapaciteitOphogen()
	{
		this.aantalGebruiktVoorCapaciteit += 1;
	}

	public void aantalMetAfspraakOphogen()
	{
		this.aantalMetAfspraak += 1;
	}

	public void aantalNietGevondenOphogen()
	{
		this.aantalNietGevonden += 1;
	}

	public String toJson()
	{
		var node = objectMapper.createObjectNode();
		node.put("aantalVerwijderen", aantalVerwijderen);
		if (typeTijdslot == ColonTijdslotType.AFSPRAAKSLOT)
		{
			node.put("aantalGebruiktVoorCapaciteit", aantalGebruiktVoorCapaciteit);
			node.put("aantalMetAfspraak", aantalMetAfspraak);
		}
		node.put("aantalNietGevonden", aantalNietGevonden);
		return node.toString();
	}

	@Override
	public String getMessage()
	{
		if (typeTijdslot == ColonTijdslotType.AFSPRAAKSLOT)
		{
			return String.format("Bulk verwijderen resultaten: aantalVerwijderen: %d, aantalGebruiktVoorCapaciteit: %d, aantalMetAfspraak: %d, aantalNietGevonden: %d",
				aantalVerwijderen, aantalGebruiktVoorCapaciteit, aantalMetAfspraak, aantalNietGevonden);
		}
		else
		{
			return String.format("Bulk verwijderen resultaten: aantalVerwijderen: %d, aantalNietGevonden: %d",
				aantalVerwijderen, aantalNietGevonden);
		}
	}
}
