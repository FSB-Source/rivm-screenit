package nl.rivm.screenit.main.model.formulieren;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.ArrayList;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.JoinColumn;
import javax.persistence.JoinTable;
import javax.persistence.ManyToMany;
import javax.persistence.Transient;

import nl.rivm.screenit.model.formulieren.IdentifierElement;
import nl.topicuszorg.formulieren2.beanantwoord.BeanAntwoordVraagDefintie;
import nl.topicuszorg.formulieren2.beanantwoord.PropertyPathLocation;
import nl.topicuszorg.formulieren2.persistence.definitie.VraagDefinitieImpl;

import org.apache.commons.lang3.NotImplementedException;
import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;

@Entity
public class BeanAntwoordVraagDefinitieImpl<T> extends VraagDefinitieImpl<T> implements BeanAntwoordVraagDefintie<T>, IdentifierElement
{

	private static final long serialVersionUID = 1L;

	private String propertyPath;

	private String code;

	@Enumerated(EnumType.STRING)
	private PropertyPathLocation propertyPathLocation;

	@ManyToMany(cascade = CascadeType.ALL)
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "verslag.cache")
	@JoinTable(schema = "algemeen", name = "form_vraag_definitie_unit_options", joinColumns = { @JoinColumn(name = "form_vraag_definitie") })
	private List<UnitOption> unitOptions = new ArrayList<>();

	@Override
	public PropertyPathLocation getPropertyPathLocation()
	{
		return propertyPathLocation;
	}

	public void setPropertyPathLocation(PropertyPathLocation propertyPathLocation)
	{
		this.propertyPathLocation = propertyPathLocation;
	}

	@Override
	public String getPropertyPath()
	{
		return propertyPath;
	}

	public void setPropertyPath(String propertyPath)
	{
		this.propertyPath = propertyPath;
	}

	public String getCode()
	{
		return code;
	}

	public void setCode(String code)
	{
		this.code = code;
	}

	public List<UnitOption> getUnitOptions()
	{
		return unitOptions;
	}

	public void setUnitOptions(List<UnitOption> unitOptions)
	{
		this.unitOptions = unitOptions;
	}

	@Override
	@Transient
	public String getIdentifier()
	{
		return code;
	}

	@Override
	@Transient
	public void setIdentifier(String identifier)
	{
		this.code = identifier;
	}

	@Override
	@Transient
	public String getDomein()
	{
		throw new NotImplementedException("");
	}

	@Override
	@Transient
	public void setDomein(String domein)
	{
		throw new NotImplementedException("");
	}
}
