package nl.rivm.screenit.main.web.client.dashboard.colon;

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

import java.util.Map;

import nl.rivm.screenit.model.enums.ExtraOpslaanKey;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.client.base.ClientBasePage;
import nl.rivm.screenit.main.web.client.base.ClientHoofdMenuitem;
import nl.rivm.screenit.main.web.component.SimpleStringResourceModel;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.colon.ColonClientAfspraakVerplaatsenPanel;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.colon.ColonIntakeAfspraak;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.colon.AfspraakService;
import nl.rivm.screenit.util.ExceptionConverter;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.link.Link;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.INZIEN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = Recht.CLIENT_DASHBOARD,
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.COLON })
public class ClientAfspraakVerplaatsenPage extends ClientBasePage
{
	private static final long serialVersionUID = 1L;

	@SpringBean
	private AfspraakService afspraakService;

	public ClientAfspraakVerplaatsenPage(IModel<ColonIntakeAfspraak> afspraakModel)
	{
		final ColonClientAfspraakVerplaatsenPanel colonClientAfspraakVerplaatsenPanel = new ColonClientAfspraakVerplaatsenPanel("afspraakVerplaatsen", afspraakModel, true);
		add(colonClientAfspraakVerplaatsenPanel);
		add(new IndicatingAjaxLink<ColonIntakeAfspraak>("opslaan", afspraakModel)
		{

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				colonClientAfspraakVerplaatsenPanel.getOpslaanMeldingen();
				Map<ExtraOpslaanKey, Object> opslaanObjecten = colonClientAfspraakVerplaatsenPanel.getOpslaanObjecten();
				if (opslaanObjecten.containsKey(ExtraOpslaanKey.AFSPRAAK))
				{
					ColonIntakeAfspraak nieuweAfspraak = (ColonIntakeAfspraak) opslaanObjecten.get(ExtraOpslaanKey.AFSPRAAK);
					if (nieuweAfspraak.getId() == null)
					{
						try
						{
							afspraakService.verplaatsAfspraak(nieuweAfspraak, ScreenitSession.get().getLoggedInAccount(), null, false, true);
						}
						catch (RuntimeException e)
						{
							ScreenitSession.get().error(String.format(getString("afspraak.wijzigen.niet.mogelijk"), ExceptionConverter.getGebruikerMeldingUitTriggerMessage(e)));
							setResponsePage(ClientColonDashboardPage.class);
							return;
						}
					}
					ScreenitSession.get().success(getString("success.afspraak_gewijzigd"));
					setResponsePage(ClientColonDashboardPage.class);
				}
			}
		});

		add(new Link<Void>("annuleren")
		{
			@Override
			public void onClick()
			{
				setResponsePage(ClientColonDashboardPage.class);
			}

		});
	}

	@Override
	public ClientHoofdMenuitem getHoofdMenuitem()
	{
		return ClientHoofdMenuitem.MIJNBEVOLKINGSONDERZOEK_COLON;
	}

	@Override
	public IModel<String> getPageName()
	{
		return new SimpleStringResourceModel("label.afspraakverplaatsen");
	}

}
