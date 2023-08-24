
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

import java.io.Serializable;

import nl.rivm.screenit.model.verslag.DSValue;
import nl.topicuszorg.formulieren2.api.definitie.AntwoordDefinitie;
import nl.topicuszorg.formulieren2.api.definitie.AntwoordKeuzeVraagDefinitie;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.model.IDetachable;
import org.apache.wicket.model.IModel;

public class DsValueAntwoordDefintie implements AntwoordDefinitie<DSValue>, Serializable, IDetachable
{

	private static final long serialVersionUID = 1L;

	private IModel<DSValue> dsValue;

	@Override
	public String getAntwoordString()
	{
		DSValue antwoordValue = getAntwoordValue();
		if (antwoordValue != null)
		{
			return antwoordValue.getDisplayNameNl();
		}

		return null;
	}

	@Override
	public DSValue getAntwoordValue()
	{
		return ModelUtil.nullSafeGet(dsValue);
	}

	@Override
	public Class<DSValue> getAntwoordValueClass()
	{
		return DSValue.class;
	}

	@Override
	public void setAntwoordValue(DSValue value)
	{
		this.dsValue = ModelUtil.sModel(value);
	}

	@Override
	public void setAntwoordString(String value)
	{

	}

	@Override
	public void setAntwoordKeuzeVraagDefinitie(AntwoordKeuzeVraagDefinitie<DSValue> vraagDefinitie)
	{

	}

	@Override
	public void detach()
	{
		ModelUtil.nullSafeDetach(dsValue);
	}

}
