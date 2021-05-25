
package nl.rivm.screenit.main.model.formulieren;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import javax.persistence.Entity;
import javax.persistence.Transient;

import nl.topicuszorg.formulieren2.beanantwoord.BeanAntwoordVraagDefintie;
import nl.topicuszorg.formulieren2.beanantwoord.BeanAntwoordVraagInstantie;
import nl.topicuszorg.formulieren2.persistence.instantie.VraagInstantieImpl;

@Entity
public class BeanAntwoordVraagInstantieImpl<T> extends VraagInstantieImpl<T> implements BeanAntwoordVraagInstantie<T>
{

	private static final long serialVersionUID = 1L;

	private String propertyPath;

	@Override
	@Transient
	public String getPropertyPath()
	{
		return propertyPath;
	}

	public void setPropertyPath(String propertyPath)
	{
		this.propertyPath = propertyPath;
	}

	@Override
	public BeanAntwoordVraagDefintie<T> getVraagDefinitie()
	{
		return (BeanAntwoordVraagDefintie<T>) super.getVraagDefinitie();
	}
}
