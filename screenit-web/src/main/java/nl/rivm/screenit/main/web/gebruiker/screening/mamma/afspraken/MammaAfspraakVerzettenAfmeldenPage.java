package nl.rivm.screenit.main.web.gebruiker.screening.mamma.afspraken;

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

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerMenuItem;
import nl.rivm.screenit.main.web.gebruiker.clienten.ClientContactActieTypeWrapper;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.ClientContactPanel;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.enums.MammaAfspraakStatus;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.model.IModel;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.INZIEN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = { Recht.GEBRUIKER_SCREENING_MAMMA_AFSPRAKEN_BEHEER },
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.MAMMA })
public class MammaAfspraakVerzettenAfmeldenPage extends MammaAfsprakenBasePage
{

	private static final long serialVersionUID = 1L;

	private IModel<MammaAfspraak> oudeAfspraak;

	public MammaAfspraakVerzettenAfmeldenPage(IModel<Client> clientModel, List<Object> extraParameters, ClientContactActieTypeWrapper... defaultSelectedActies)
	{
		setDefaultModel(ModelUtil.sModel((MammaScreeningsEenheid) extraParameters.stream().filter(p -> p instanceof MammaScreeningsEenheid).findFirst().orElse(null)));
		LocalDate currentDate = (LocalDate) extraParameters.stream().filter(p -> p instanceof LocalDate).findFirst().orElse(null);
		oudeAfspraak = ModelUtil.sModel((MammaAfspraak) extraParameters.stream().filter(p -> p instanceof MammaAfspraak).findFirst().get());

		add(new ClientContactPanel("panel", clientModel, extraParameters, defaultSelectedActies)
		{
			@Override
			protected void contactAfgerond()
			{
				setResponsePage(new MammaAfsprakenDagOverzichtPage((IModel<MammaScreeningsEenheid>) MammaAfspraakVerzettenAfmeldenPage.this.getDefaultModel(),
					DateUtil.toUtilDate(currentDate)));
			}

			@Override
			protected void contactNietAfgerond(Client client)
			{
				List<Object> extraParameters = new ArrayList<>();
				extraParameters.add(oudeAfspraak.getObject());
				extraParameters.add(MammaAfspraakStatus.GEPLAND);
				extraParameters.add(getPage().getDefaultModelObject());
				extraParameters.add(Constants.CONTACT_EXTRA_PARAMETER_VANUIT_BK_PLANNING);
				extraParameters.add(currentDate);
				setResponsePage(new MammaAfspraakVerzettenAfmeldenPage(ModelUtil.sModel(client), extraParameters, defaultSelectedActies));
			}
		});
	}

	@Override
	protected List<GebruikerMenuItem> getContextMenuItems()
	{
		List<GebruikerMenuItem> contextMenuItems = new ArrayList<>();
		contextMenuItems.add(new GebruikerMenuItem("label.tab.mammascreening.planning.afspraken.beheer", MammaAfsprakenSEZoekenPage.class));
		contextMenuItems.add(new GebruikerMenuItem("label.tab.mammascreening.planning.afspraken.verzetten.afmelden", false, MammaAfspraakVerzettenAfmeldenPage.class));

		return contextMenuItems;
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(oudeAfspraak);
	}
}
