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
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.client.base.ClientBasePage;
import nl.rivm.screenit.main.web.client.base.ClientHoofdMenuitem;
import nl.rivm.screenit.main.web.component.SimpleStringResourceModel;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.AbstractClientContactActiePanel;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.ClientContactPanel;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.ClientContactPanel.ClientContactPanelCreateContext;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.mamma.MammaClientAfspraakUitstellenPanel;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.mamma.MammaUitstelKiezenPanel;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientContact;
import nl.rivm.screenit.model.ClientContactActie;
import nl.rivm.screenit.model.ClientContactActieType;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;

import org.apache.wicket.ajax.AjaxRequestTarget;
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
public class ClientMammaUitstellenPage extends ClientBasePage
{
	private static final long serialVersionUID = 1L;

	@SpringBean
	private ClientContactService clientContactService;

	private ClientContactPanelCreateContext clientContactPanelCreateContext;

	private final AbstractClientContactActiePanel<?> clientUitstellenPanel;

	private final Form<Void> form;

	public ClientMammaUitstellenPage(MammaAfspraak afspraak)
	{
		this(new MammaClientAfspraakUitstellenPanel("uitstellen", afspraak));
	}

	public ClientMammaUitstellenPage(IModel<Client> model)
	{
		this(new MammaUitstelKiezenPanel("uitstellen", model));
	}

	private ClientMammaUitstellenPage(AbstractClientContactActiePanel<?> panel)
	{
		clientUitstellenPanel = panel;
		form = new Form<>("form");
		add(form);
		form.add(panel);
	}

	private void maakAnnulerenButton()
	{
		form.add(new Link<Void>("annuleren")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick()
			{
				setResponsePage(ClientMammaDashboardPage.class);
			}

		});
	}

	private void maakOpslaanButton()
	{
		form.add(new IndicatingAjaxSubmitLink("opslaan", form)
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onSubmit(AjaxRequestTarget target)
			{
				clientUitstellenPanel.validate();
				if (!form.hasError())
				{
					Map<ExtraOpslaanKey, Object> opslaanObjecten = clientUitstellenPanel.getOpslaanObjecten();
					ClientContact contact = new ClientContact();
					ClientContactActie actie = new ClientContactActie();
					actie.setType(ClientContactActieType.MAMMA_AFSPRAAK_WIJZIGEN);
					contact.getActies().add(actie);
					Client ingelogdeClient = getIngelogdeClient();
					contact.setClient(ingelogdeClient);

					Map<ClientContactActieType, Map<ExtraOpslaanKey, Object>> extraOpslaanObjecten = new HashMap<>();
					extraOpslaanObjecten.put(ClientContactActieType.MAMMA_AFSPRAAK_WIJZIGEN, opslaanObjecten);
					clientContactService.saveClientContact(contact, extraOpslaanObjecten, ingelogdeClient);
					ScreenitSession.get().success(getString("success.uitgestel"));
					setResponsePage(ClientMammaDashboardPage.class);
				}
			}
		});
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();

		maakOpslaanButton();

		maakAnnulerenButton();

		maakAfspraakButtons();

		clientContactPanelCreateContext = new ClientContactPanel.ClientContactPanelCreateContext();
		if (getPage().getMetaData(ClientContactPanel.CREATE_CONTEXT_KEY) == null)
		{
			getPage().setMetaData(ClientContactPanel.CREATE_CONTEXT_KEY, clientContactPanelCreateContext);
		}
	}

	private void maakAfspraakButtons()
	{

		form.add(new Link<Void>("afspraakVerzetten")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick()
			{
				MammaClientAfspraakUitstellenPanel panel = (MammaClientAfspraakUitstellenPanel) clientUitstellenPanel;
				setResponsePage(new ClientMammaAfspraakVerzettenPage(panel.getModel()));
			}

		}.setVisible(clientUitstellenPanel instanceof MammaClientAfspraakUitstellenPanel));
		form.add(new Link<Void>("afspraakAanmaken")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick()
			{
				MammaUitstelKiezenPanel panel = (MammaUitstelKiezenPanel) clientUitstellenPanel;
				setResponsePage(new ClientMammaAfspraakAanmakenPage(panel.getModel()));
			}

		}.setVisible(clientUitstellenPanel instanceof MammaUitstelKiezenPanel));
	}

	@Override
	public ClientHoofdMenuitem getHoofdMenuitem()
	{
		return ClientHoofdMenuitem.MIJNBEVOLKINGSONDERZOEK_MAMMA;
	}

	@Override
	public IModel<String> getPageName()
	{
		return new SimpleStringResourceModel("label.uitstellen");
	}

}
