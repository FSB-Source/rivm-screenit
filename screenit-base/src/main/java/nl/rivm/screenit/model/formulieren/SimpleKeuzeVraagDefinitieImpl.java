
package nl.rivm.screenit.model.formulieren;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;

import nl.topicuszorg.formulieren2.api.definitie.AntwoordKeuzeVraagDefinitie;
import nl.topicuszorg.formulieren2.api.rendering.AntwoordRenderType;

@Entity
public class SimpleKeuzeVraagDefinitieImpl<T> extends SimpleVraagDefinitieImpl<T> implements AntwoordKeuzeVraagDefinitie<T>, IdentifierElement
{

	private static final long serialVersionUID = 1L;

	@Enumerated(EnumType.STRING)
	private AntwoordRenderType renderType;

	@Override
	public AntwoordRenderType getRenderType()
	{
		return renderType;
	}

	@Override
	public boolean isMeervoudig()
	{
		return false;
	}

	public void setRenderType(AntwoordRenderType renderType)
	{
		this.renderType = renderType;
	}

}
