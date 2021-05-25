
package nl.rivm.screenit.main.web.gebruiker.screening.colon.houdbaarheid.ifobtbatch;

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

import java.util.List;

import nl.rivm.screenit.main.web.gebruiker.base.GebruikerHoofdMenuItem;
import nl.rivm.screenit.main.web.gebruiker.screening.gedeeld.houdbaarheid.HoudbaarheidEditPage;
import nl.rivm.screenit.main.web.gebruiker.screening.gedeeld.houdbaarheid.HoudbaarheidOverzichtPage;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.colon.IFOBTVervaldatum;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;

import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.INZIEN,
	checkScope = false,
	constraint = ShiroConstraint.HasPermission,
	recht = Recht.GEBRUIKER_SCREENING_IFOBT_BATCH,
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.COLON })
public class IFOBTBatchOverzichtPage extends HoudbaarheidOverzichtPage<IFOBTVervaldatum>
{

	private static final long serialVersionUID = 1L;

	@Override
	protected List<IColumn<IFOBTVervaldatum, String>> createColumns()
	{
		List<IColumn<IFOBTVervaldatum, String>> columns = super.createColumns();
		columns.add(new PropertyColumn<IFOBTVervaldatum, String>(new Model<String>("Type"), "type", "type"));
		return columns;
	}

	@Override
	protected HoudbaarheidEditPage<IFOBTVervaldatum> createEditPage(IModel<IFOBTVervaldatum> model)
	{
		if (model == null)
		{
			return new IFOBTBatchEditPage();
		}
		else
		{
			return new IFOBTBatchEditPage(model);
		}
	}

	@Override
	protected GebruikerHoofdMenuItem getActieveMenuItem()
	{
		return GebruikerHoofdMenuItem.COLON;
	}

}
