package nl.rivm.screenit.main.model.formulieren;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import javax.persistence.Entity;

import nl.topicuszorg.formulieren2.api.definitie.AntwoordKeuzeVraagDefinitie;
import nl.topicuszorg.formulieren2.api.rendering.AntwoordRenderType;

@Entity
public class UnitBeanAntwoordVraagDefintieImpl extends BeanAntwoordVraagDefinitieImpl<String> implements AntwoordKeuzeVraagDefinitie<String>
{

	private static final long serialVersionUID = 1L;

	@Override
	public AntwoordRenderType getRenderType()
	{
		return AntwoordRenderType.DROPDOWN;
	}

	@Override
	public boolean isMeervoudig()
	{
		return false;
	}

}
