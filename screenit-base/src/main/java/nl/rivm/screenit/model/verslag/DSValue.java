
package nl.rivm.screenit.model.verslag;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.model.helper.HibernateMagicNumber;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import com.fasterxml.jackson.annotation.JsonValue;

@Entity(name = "verslag_ds_value")
@Table(schema = "gedeeld", uniqueConstraints = @UniqueConstraint(name = "verslag_ds_value_uc", columnNames = { "valueSetName", "code", "codeSystem" }))
public class DSValue extends AbstractHibernateObject
{

	private static final long serialVersionUID = 1L;

	@Column(nullable = false)
	private String valueSetName;

	@Column(nullable = false)
	private String code;

	@Column(nullable = false)
	private String codeSystem;

	@Column(length = HibernateMagicNumber.L512, nullable = false)
	private String displayName;

	@Column(length = HibernateMagicNumber.L512, nullable = false)
	private String displayNameNl;

	@Column(length = HibernateMagicNumber.L4096)
	private String extraInformation;

	public String getCode()
	{
		return code;
	}

	public void setCode(String code)
	{
		this.code = code;
	}

	public String getCodeSystem()
	{
		return codeSystem;
	}

	public void setCodeSystem(String codeSystem)
	{
		this.codeSystem = codeSystem;
	}

	public String getDisplayName()
	{
		return displayName;
	}

	public void setDisplayName(String displayName)
	{
		this.displayName = displayName;
	}

	public String getExtraInformation()
	{
		return extraInformation;
	}

	public void setExtraInformation(String extraInformation)
	{
		this.extraInformation = extraInformation;
	}

	@JsonValue
	public String getDisplayNameNl()
	{
		return displayNameNl;
	}

	public void setDisplayNameNl(String displayNameNl)
	{
		this.displayNameNl = displayNameNl;
	}

	public String getValueSetName()
	{
		return valueSetName;
	}

	public void setValueSetName(String valueSetName)
	{
		this.valueSetName = valueSetName;
	}

}
