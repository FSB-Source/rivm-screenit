package nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.werklijst;

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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import nl.rivm.screenit.main.model.mamma.beoordeling.MammaBeWerklijstZoekObject;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.discrepantie_arbitrage.MammaDiscrepantiePage;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.markup.html.repeater.util.SortParam;
import org.apache.wicket.model.IModel;
import org.wicketstuff.shiro.ShiroConstraint;

import static nl.rivm.screenit.main.util.WicketSpringDataUtil.toSpringSort;

@SecurityConstraint(
	actie = Actie.INZIEN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = { Recht.GEBRUIKER_SCREENING_MAMMA_BEOORDELING_WERKLIJST },
	organisatieTypeScopes = { OrganisatieType.BEOORDELINGSEENHEID },
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.MAMMA })
public class MammaDiscrepantieWerklijstPage extends AbstractMammaBeWerklijstPage
{
	@Override
	public void openBeoordelingScherm(AjaxRequestTarget target, IModel<MammaBeoordeling> model, IModel<MammaBeWerklijstZoekObject> zoekObject, SortParam<String> sortParam)
	{
		ScreenitSession.get().setZoekObject(MammaDiscrepantieWerklijstPage.class, zoekObject); 
		MammaBeoordeling beoordeling = model.getObject();
		List<Long> beoordelingenIds = beWerklijstService.zoekBeoordelingenNummers(zoekObject.getObject(), toSpringSort(sortParam));
		setResponsePage(new MammaDiscrepantiePage(beoordeling.getId(), beoordelingenIds, this.getClass()));
	}

	@Override
	public List<MammaBeoordelingStatus> getDefaultStatussen()
	{
		return new ArrayList<>(Arrays.asList(MammaBeoordelingStatus.DISCREPANTIE));
	}

	@Override
	public List<MammaBeoordelingStatus> getBeschikbarePaginaStatussen()
	{
		return new ArrayList<>(Arrays.asList(MammaBeoordelingStatus.DISCREPANTIE));
	}
}
