package nl.rivm.screenit.main.web.gebruiker.algemeen.technischbeheer;

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

import nl.rivm.screenit.main.model.Parameterisatie;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.gebruiker.algemeen.parameterisatie.BasePrimaireParametersPanel;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.enums.ToegangLevel;

import org.apache.wicket.model.IModel;

public abstract class BaseTechnischBeheerParametersPanel extends BasePrimaireParametersPanel
{

	public BaseTechnischBeheerParametersPanel(String id, IModel<Parameterisatie> model)
	{
		super(id, model);
	}

	@Override
	protected ToegangLevel getToegangsLevel()
	{
		return ScreenitSession.get().getToegangsLevel(Actie.AANPASSEN, Recht.TECHNISCH_BEHEER);
	}

	@Override
	protected boolean magAanpassen()
	{
		return ScreenitSession.get().checkPermission(Recht.TECHNISCH_BEHEER, Actie.AANPASSEN);
	}
}
