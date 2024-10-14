package nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.werklijst;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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
import nl.rivm.screenit.main.service.mamma.MammaImsService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.beoordelen.MammaBeoordelenPage;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus;
import nl.rivm.screenit.model.mamma.enums.MammobridgeRole;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.markup.html.repeater.util.SortParam;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

import static nl.rivm.screenit.main.util.WicketSpringDataUtil.toSpringSort;

public class MammaBeoordelingenWerklijstPage extends AbstractMammaBeWerklijstPage
{
	@SpringBean
	private MammaImsService imsService;

	public MammaBeoordelingenWerklijstPage()
	{
		super();
		wijzigIDS7Role(MammobridgeRole.RADIOLOGIST);
	}

	@Override
	public boolean bevestigenButtonVisible()
	{
		return true;
	}

	@Override
	public boolean bevestigenButtonEnabled()
	{
		return beWerklijstService.is1eOf2eLezingenTeBevestigen(ScreenitSession.get().getLoggedInInstellingGebruiker());
	}

	@Override
	public void openBeoordelingScherm(AjaxRequestTarget target, IModel<MammaBeoordeling> model, IModel<MammaBeWerklijstZoekObject> zoekObject, SortParam<String> sortParam)
	{
		ScreenitSession.get().setZoekObject(MammaBeoordelingenWerklijstPage.class, zoekObject); 
		MammaBeoordeling beoordeling = model.getObject();
		List<Long> beoordelingenIds = beWerklijstService.zoekBeoordelingenNummers(zoekObject.getObject(), toSpringSort(sortParam));
		setResponsePage(new MammaBeoordelenPage(beoordeling.getId(), beoordelingenIds, MammaBeoordelingenWerklijstPage.class));
	}

	@Override
	public List<MammaBeoordelingStatus> getDefaultStatussen()
	{
		return new ArrayList<>(Arrays.asList(MammaBeoordelingStatus.EERSTE_LEZING, MammaBeoordelingStatus.TWEEDE_LEZING));
	}

	@Override
	public List<MammaBeoordelingStatus> getBeschikbarePaginaStatussen()
	{
		return new ArrayList<>(Arrays.asList(MammaBeoordelingStatus.EERSTE_LEZING, MammaBeoordelingStatus.TWEEDE_LEZING,
			MammaBeoordelingStatus.EERSTE_LEZING_OPGESLAGEN, MammaBeoordelingStatus.TWEEDE_LEZING_OPGESLAGEN));
	}

	@Override
	protected void handleImsError(AjaxRequestTarget target, String errorMessage, Long onderzoekId)
	{
		error(imsService.handleError(errorMessage, ScreenitSession.get().getLoggedInInstellingGebruiker(), b -> getString((String) b), onderzoekId));
	}
}
