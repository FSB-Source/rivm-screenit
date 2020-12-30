
package nl.rivm.screenit.main.web.gebruiker.clienten.agenda;

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

import java.util.ArrayList;
import java.util.List;

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.gebruiker.clienten.ClientContactActieTypeWrapper;
import nl.rivm.screenit.main.web.gebruiker.clienten.ClientPaspoortPanel;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.ClientContactPage;
import nl.rivm.screenit.model.Afspraak;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.colon.ColonIntakeAfspraak;
import nl.rivm.screenit.model.colon.planning.AfspraakStatus;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.enums.MammaAfspraakStatus;
import nl.rivm.screenit.service.ClientService;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;
import nl.topicuszorg.wicket.hibernate.cglib.ModelProxyHelper;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class ClientAgendaPanel extends GenericPanel<Client>
{

	private static final long serialVersionUID = 1L;

	private BootstrapDialog dialog;

	@SpringBean
	private ClientService clientService;

	public ClientAgendaPanel(String id, IModel<Client> model)
	{
		super(id, model);

		add(new ClientPaspoortPanel("passpoort", model));
		dialog = new BootstrapDialog("dialog");
		add(dialog);

		IndicatingAjaxLink<Void> contactAanmaken = new IndicatingAjaxLink<Void>("contactAanmaken")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				setResponsePage(new ClientContactPage(ClientAgendaPanel.this.getModel()));
			}

		};
		contactAanmaken
			.setVisible(ScreenitSession.get().checkPermission(Recht.GEBRUIKER_CLIENT_CONTACT, null, model.getObject()) && !clientService.isClientOverleden(model.getObject()));
		add(contactAanmaken);

		ColonAfspraakPanel colonAfspraakPanel = new ColonAfspraakPanel("dkAfspraak", ModelUtil.cModel(model.getObject()))
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void afspraakWijzigen(AjaxRequestTarget target, Afspraak afspraak, boolean locatieWijzigen)
			{
				ColonIntakeAfspraak intakeAfspraak = (ColonIntakeAfspraak) HibernateHelper.deproxy(ModelProxyHelper.deproxy(afspraak));
				List<Object> extraParameters = new ArrayList<>();
				extraParameters.add(intakeAfspraak);
				extraParameters.add(AfspraakStatus.VERPLAATST);
				extraParameters.add(Boolean.valueOf(locatieWijzigen));
				setResponsePage(new ClientContactPage(ClientAgendaPanel.this.getModel(), extraParameters, ClientContactActieTypeWrapper.COLON_AFSPRAAK_WIJZIGEN_AFZEGGEN));
			}

			@Override
			public void afspraakAfzeggen(AjaxRequestTarget target, Afspraak afspraak)
			{
				ColonIntakeAfspraak intakeAfspraak = (ColonIntakeAfspraak) HibernateHelper.deproxy(ModelProxyHelper.deproxy(afspraak));
				List<Object> extraParameters = new ArrayList<>();
				extraParameters.add(intakeAfspraak);
				extraParameters.add(AfspraakStatus.GEANNULEERD_VIA_INFOLIJN);
				setResponsePage(new ClientContactPage(ClientAgendaPanel.this.getModel(), extraParameters, ClientContactActieTypeWrapper.COLON_AFSPRAAK_WIJZIGEN_AFZEGGEN));
			}
		};
		add(colonAfspraakPanel);

		MammaAfspraakPanel mammaAfspraakPanel = new MammaAfspraakPanel("bkAfspraak", ModelUtil.cModel(model.getObject()))
		{
			private static final long serialVersionUID = 1L;

			@Override
			public void verzetten(AjaxRequestTarget target, MammaAfspraak afspraak)
			{
				afspraak = (MammaAfspraak) HibernateHelper.deproxy(ModelProxyHelper.deproxy(afspraak));
				List<Object> extraParameters = new ArrayList<>();
				extraParameters.add(afspraak);
				extraParameters.add(MammaAfspraakStatus.GEPLAND);

				setResponsePage(new ClientContactPage(ClientAgendaPanel.this.getModel(), extraParameters, ClientContactActieTypeWrapper.MAMMA_AFSPRAAK_WIJZIGEN));
			}

			@Override
			public void uitstellen(AjaxRequestTarget target, MammaAfspraak afspraak)
			{
				afspraak = (MammaAfspraak) HibernateHelper.deproxy(ModelProxyHelper.deproxy(afspraak));
				List<Object> extraParameters = new ArrayList<>();
				extraParameters.add(afspraak);
				extraParameters.add(MammaAfspraakStatus.UITGESTELD);

				setResponsePage(new ClientContactPage(ClientAgendaPanel.this.getModel(), extraParameters, ClientContactActieTypeWrapper.MAMMA_AFSPRAAK_WIJZIGEN));
			}

			@Override
			public void afmelden(AjaxRequestTarget target, MammaAfspraak afspraak)
			{
				setResponsePage(new ClientContactPage(ClientAgendaPanel.this.getModel(), new ArrayList<>(), ClientContactActieTypeWrapper.MAMMA_AFMELDEN));
			}
		};
		add(mammaAfspraakPanel);
	}
}
