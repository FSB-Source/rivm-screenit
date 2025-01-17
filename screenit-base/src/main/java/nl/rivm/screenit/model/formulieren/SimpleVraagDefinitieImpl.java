package nl.rivm.screenit.model.formulieren;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;
import javax.persistence.UniqueConstraint;

import nl.topicuszorg.formulieren2.persistence.definitie.VraagDefinitieImpl;

@Entity

@Table(uniqueConstraints = @UniqueConstraint(name = "form_vraag_definite_id_domein_ukey", columnNames = { "identifier", "domein" }))
public class SimpleVraagDefinitieImpl<T> extends VraagDefinitieImpl<T> implements IdentifierElement
{
	@Column
	private String identifier;

	@Column
	private String domein;

	@Override
	public String getIdentifier()
	{
		return identifier;
	}

	@Override
	public void setIdentifier(String identifier)
	{
		this.identifier = identifier;
	}

	@Override
	public String getDomein()
	{
		return domein;
	}

	@Override
	public void setDomein(String domein)
	{
		this.domein = domein;
	}
}
