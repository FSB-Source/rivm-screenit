package nl.rivm.screenit.main.web.client.dashboard.mamma;

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

import java.util.HashMap;
import java.util.Map;

import nl.rivm.screenit.service.ClientContactService;
import nl.rivm.screenit.model.enums.ExtraOpslaanKey;
import nl.rivm.screenit.exceptions.MammaTijdNietBeschikbaarException;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.client.base.ClientBasePage;
import nl.rivm.screenit.main.web.client.base.ClientHoofdMenuitem;
import nl.rivm.screenit.main.web.component.SimpleStringResourceModel;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.ClientContactPanel;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.ClientContactPanel.ClientContactPanelCreateContext;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.mamma.MammaClientAfspraakVerzettenPanel;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientContact;
import nl.rivm.screenit.model.ClientContactActie;
import nl.rivm.screenit.model.ClientContactActieType;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.util.ExceptionConverter;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.link.Link;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.INZIEN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = Recht.CLIENT_DASHBOARD,
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.MAMMA })
public class ClientMammaAfspraakVerzettenPage extends ClientBasePage
{
	private static final long serialVersionUID = 1L;

	@SpringBean
	private ClientContactService clientContactService;

	private ClientContactPanelCreateContext clientContactPanelCreateContext;

	public ClientMammaAfspraakVerzettenPage(IModel<MammaAfspraak> afspraakModel)
	{
		Form<Void> form = new Form<>("form");
		add(form);
		MammaAfspraak afspraak = afspraakModel.getObject();
		final MammaClientAfspraakVerzettenPanel clientAfspraakVerzettenPanel = new MammaClientAfspraakVerzettenPanel("afspraakVerzetten", afspraak);
		form.add(clientAfspraakVerzettenPanel);
		form.add(new IndicatingAjaxSubmitLink("opslaan", form)
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onSubmit(AjaxRequestTarget target)
			{
				clientAfspraakVerzettenPanel.validate();
				if (!form.hasError())
				{
					Map<ExtraOpslaanKey, Object> opslaanObjecten = clientAfspraakVerzettenPanel.getOpslaanObjecten();
					ClientContact contact = new ClientContact();
					ClientContactActie actie = new ClientContactActie();
					actie.setType(ClientContactActieType.MAMMA_AFSPRAAK_WIJZIGEN);
					contact.getActies().add(actie);
					Client ingelogdeClient = getIngelogdeClient();
					contact.setClient(ingelogdeClient);

					Map<ClientContactActieType, Map<ExtraOpslaanKey, Object>> extraOpslaanObjecten = new HashMap<>();
					extraOpslaanObjecten.put(ClientContactActieType.MAMMA_AFSPRAAK_WIJZIGEN, opslaanObjecten);
					try
					{
						clientContactService.saveClientContact(contact, extraOpslaanObjecten, ingelogdeClient);
						ScreenitSession.get().success(getString("success.afspraak_gewijzigd"));
					}
					catch (MammaTijdNietBeschikbaarException e)
					{
						ScreenitSession.get().error(getString("tijd.niet.beschikbaar"));
					}
					catch (RuntimeException e)
					{
						ScreenitSession.get().error(String.format(getString("afspraak.wijzigen.niet.mogelijk"), ExceptionConverter.getGebruikerMeldingUitTriggerMessage(e)));
					}
					setResponsePage(ClientMammaDashboardPage.class);
				}
			}
		});

		form.add(new IndicatingAjaxLink<MammaAfspraak>("uitstellen")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				setResponsePage(new ClientMammaUitstellenPage(clientAfspraakVerzettenPanel.getModelObject()));
			}
		}.setVisible(afspraak.getUitnodiging().getScreeningRonde().getDossier().getTehuis() == null && !afspraak.isGeforceerdeAfspraak()));

		form.add(new Link<Void>("annuleren")
		{
			private static final long serialVersionUID = 1L;

			@Override
			public void onClick()
			{
				setResponsePage(ClientMammaDashboardPage.class);
			}

		});

		clientContactPanelCreateContext = new ClientContactPanel.ClientContactPanelCreateContext();
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();

		if (getPage().getMetaData(ClientContactPanel.CREATE_CONTEXT_KEY) == null)
		{
			getPage().setMetaData(ClientContactPanel.CREATE_CONTEXT_KEY, clientContactPanelCreateContext);
		}
	}

	@Override
	public ClientHoofdMenuitem getHoofdMenuitem()
	{
		return ClientHoofdMenuitem.MIJNBEVOLKINGSONDERZOEK_MAMMA;
	}

	@Override
	public IModel<String> getPageName()
	{
		return new SimpleStringResourceModel("label.afspraakverplaatsen");
	}

}
