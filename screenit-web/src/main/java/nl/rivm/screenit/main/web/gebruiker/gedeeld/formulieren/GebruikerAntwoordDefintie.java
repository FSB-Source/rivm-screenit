
package nl.rivm.screenit.main.web.gebruiker.gedeeld.formulieren;

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

import nl.rivm.screenit.model.Gebruiker;
import nl.topicuszorg.formulieren2.api.definitie.AntwoordDefinitie;
import nl.topicuszorg.formulieren2.api.definitie.AntwoordKeuzeVraagDefinitie;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.model.IDetachable;
import org.apache.wicket.model.IModel;

public class GebruikerAntwoordDefintie implements AntwoordDefinitie<Gebruiker>, IDetachable
{

	private static final long serialVersionUID = 1L;

	private final IModel<Gebruiker> medewerkerModel;

	public GebruikerAntwoordDefintie(Gebruiker medewerker)
	{
		this.medewerkerModel = ModelUtil.sModel(medewerker);
	}

	@Override
	public void detach()
	{
		ModelUtil.nullSafeDetach(medewerkerModel);
	}

	@Override
	public String getAntwoordString()
	{
		return medewerkerModel.getObject().getNaamVolledig();
	}

	@Override
	public Gebruiker getAntwoordValue()
	{
		return medewerkerModel.getObject();
	}

	@Override
	public Class<Gebruiker> getAntwoordValueClass()
	{
		return Gebruiker.class;
	}

	@Override
	public void setAntwoordValue(Gebruiker value)
	{
		medewerkerModel.setObject(value);
	}

	@Override
	public void setAntwoordString(String value)
	{

	}

	@Override
	public void setAntwoordKeuzeVraagDefinitie(AntwoordKeuzeVraagDefinitie<Gebruiker> vraagDefinitie)
	{

	}

}
