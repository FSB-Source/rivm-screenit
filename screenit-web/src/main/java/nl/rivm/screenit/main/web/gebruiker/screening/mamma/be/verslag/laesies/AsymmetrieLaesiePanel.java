package nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.verslag.laesies;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.util.List;

import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.dto.LaesieDto;
import nl.rivm.screenit.model.mamma.enums.MammaAsymmetrieSpecificatie;

import org.apache.wicket.model.IModel;

public class AsymmetrieLaesiePanel extends LaesiePanel
{
	public AsymmetrieLaesiePanel(String id, IModel<LaesieDto> model, List<LaesieDto> alleLaesies)
	{
		super(id, model, alleLaesies);
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();
		add(new LaesieTypePanel("laesieType", getModel(), alleLaesies));
		add(new LaesieLocatiePanel("locatiePanel", getModel()));
		add(createSpecificatieRadioChoice("asymmetrieSpecificatie", MammaAsymmetrieSpecificatie.class));
	}
}
